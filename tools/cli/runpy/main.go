package main

import (
	"archive/zip"
	"embed"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
)

//go:embed pyembed/*
var embeddedFiles embed.FS

func main() {
    targetDir := getTargetDir()
    venvDir := filepath.Join(targetDir, "venv")

    fmt.Println("Using target directory:", targetDir)

    //python, err := findPython()
    //if err != nil {
    //    fmt.Println("ERROR: No Python interpreter found on this system.")
    //    fmt.Println("Please install Python 3.8+ and try again.")
    //    os.Exit(1)
    //}
	python, err := installPythonIfMissing(targetDir)
	if err != nil {
    	fmt.Println("Failed to install Python:", err)
    	os.Exit(1)
	}

    fmt.Println("Using Python:", python)

    ensureVenv(python, venvDir)

    pkgPath := extractEmbeddedPackage(targetDir)
    installPackage(venvDir, pkgPath)

    installDependencies(venvDir, targetDir)

    //runEntryPoint(venvDir, "mypkg") // runs: python -m mypkg

    scriptsDir := extractEmbeddedScripts(targetDir)
    script := filepath.Join(scriptsDir, "myscript.py")
    dataDir, modelDir := resolveExternalDirs(targetDir)
    runEmbeddedScript(venvDir, script, dataDir, modelDir)
}

func getTargetDir() string {
    if len(os.Args) > 1 {
        return os.Args[1]
    }
    exe, err := os.Executable()
    if err != nil {
        panic(err)
    }
    return filepath.Dir(exe)
}

//
// PYTHON DISCOVERY
//

func findPython() (string, error) {
    candidates := []string{
        "python3",
        "python",
    }

    // Windows: also try py launcher
    if runtime.GOOS == "windows" {
        candidates = append([]string{"py", "python"}, candidates...)
    }

    for _, c := range candidates {
        path, err := exec.LookPath(c)
        if err == nil {
            return path, nil
        }
    }

    return "", errors.New("python not found")
}

//
// VENV CREATION
//

func ensureVenv(python, venvDir string) {
    if _, err := os.Stat(venvDir); err == nil {
        fmt.Println("Venv already exists")
        return
    }

    fmt.Println("Creating venv...")
    cmd := exec.Command(python, "-m", "venv", venvDir)
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    if err := cmd.Run(); err != nil {
        panic(fmt.Errorf("failed to create venv: %w", err))
    }
}

//
// PATH HELPERS
//

func venvPython(venvDir string) string {
    if runtime.GOOS == "windows" {
        return filepath.Join(venvDir, "Scripts", "python.exe")
    }
    return filepath.Join(venvDir, "bin", "python")
}

func venvPip(venvDir string) string {
    if runtime.GOOS == "windows" {
        return filepath.Join(venvDir, "Scripts", "pip.exe")
    }
    return filepath.Join(venvDir, "bin", "pip")
}

//
// EMBEDDED PACKAGE EXTRACTION
//

func extractEmbeddedPackage(targetDir string) string {
    outDir := filepath.Join(targetDir, "embedded_pkg")
    os.MkdirAll(outDir, 0755)

    entries, err := embeddedFiles.ReadDir("pyembed")
    if err != nil {
        panic(err)
    }

    var pkgPath string
    for _, e := range entries {
        data, err := embeddedFiles.ReadFile("pyembed/" + e.Name())
        if err != nil {
            panic(err)
        }
        dst := filepath.Join(outDir, e.Name())
        if err := os.WriteFile(dst, data, 0644); err != nil {
            panic(err)
        }
        pkgPath = dst
    }

    fmt.Println("Extracted embedded package to:", pkgPath)
    return pkgPath
}

//
// INSTALL PACKAGE
//

func installPackage(venvDir, pkgPath string) {
    pip := venvPip(venvDir)
    fmt.Println("Installing embedded package:", pkgPath)

    cmd := exec.Command(pip, "install", pkgPath)
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    if err := cmd.Run(); err != nil {
        panic(fmt.Errorf("pip install failed: %w", err))
    }
}

//
// INSTALL DEPENDENCIES
//

func installDependencies(venvDir, targetDir string) {
    req := filepath.Join(targetDir, "requirements.txt")
    if _, err := os.Stat(req); err != nil {
        return // no requirements.txt, skip
    }

    fmt.Println("Installing dependencies from requirements.txt")

    pip := venvPip(venvDir)
    cmd := exec.Command(pip, "install", "-r", req)
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    if err := cmd.Run(); err != nil {
        panic(fmt.Errorf("pip install requirements failed: %w", err))
    }
}

//
// RUN ENTRY POINT
//

func runEntryPoint(venvDir, module string) {
    python := venvPython(venvDir)
    fmt.Println("Running entry point:", module)

    cmd := exec.Command(python, "-m", module)
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    if err := cmd.Run(); err != nil {
        panic(fmt.Errorf("entry point failed: %w", err))
    }
}

// Called before ensureVenv()
func installPythonIfMissing(targetDir string) (string, error) {
    python, err := findPython()
    if err == nil {
        return python, nil
    }

    fmt.Println("Python not found. Installing local runtime...")

    switch runtime.GOOS {
    case "windows":
        return installPythonWindows(targetDir)
    case "darwin":
        return installPythonMac(targetDir)
    case "linux":
        return installPythonLinux(targetDir)
    default:
        return "", errors.New("unsupported OS for auto-install")
    }
}

//
// WINDOWS INSTALLER
//

func installPythonWindows(targetDir string) (string, error) {
    url := "https://www.python.org/ftp/python/3.12.1/python-3.12.1-embed-amd64.zip"
    dst := filepath.Join(targetDir, "python-embed.zip")
    outDir := filepath.Join(targetDir, "python-runtime")

    fmt.Println("Downloading Python embed package...")

    if err := downloadFile(dst, url); err != nil {
        return "", err
    }

    fmt.Println("Extracting Python runtime...")
    if err := unzip(dst, outDir); err != nil {
        return "", err
    }

    pythonExe := filepath.Join(outDir, "python.exe")
    if _, err := os.Stat(pythonExe); err != nil {
        return "", errors.New("python.exe missing after extraction")
    }

    return pythonExe, nil
}

//
// MAC INSTALLER
//

func installPythonMac(targetDir string) (string, error) {
    url := "https://www.python.org/ftp/python/3.12.1/python-3.12.1-macos11.pkg"
    pkg := filepath.Join(targetDir, "python.pkg")

    fmt.Println("Downloading Python macOS installer...")
    if err := downloadFile(pkg, url); err != nil {
        return "", err
    }

    fmt.Println("Running macOS installer (requires admin)...")
    cmd := exec.Command("sudo", "installer", "-pkg", pkg, "-target", "/")
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    if err := cmd.Run(); err != nil {
        return "", err
    }

    // Python installs to /usr/local/bin/python3
    return "/usr/local/bin/python3", nil
}

//
// LINUX INSTALLER
//

func installPythonLinux(targetDir string) (string, error) {
    // Try apt
    if _, err := exec.LookPath("apt-get"); err == nil {
        fmt.Println("Installing Python via apt...")
        cmd := exec.Command("sudo", "apt-get", "install", "-y", "python3", "python3-venv")
        cmd.Stdout = os.Stdout
        cmd.Stderr = os.Stderr
        if err := cmd.Run(); err == nil {
            return "python3", nil
        }
    }

    // Try dnf
    if _, err := exec.LookPath("dnf"); err == nil {
        fmt.Println("Installing Python via dnf...")
        cmd := exec.Command("sudo", "dnf", "install", "-y", "python3")
        cmd.Stdout = os.Stdout
        cmd.Stderr = os.Stderr
        if err := cmd.Run(); err == nil {
            return "python3", nil
        }
    }

    // Try pacman
    if _, err := exec.LookPath("pacman"); err == nil {
        fmt.Println("Installing Python via pacman...")
        cmd := exec.Command("sudo", "pacman", "-S", "--noconfirm", "python")
        cmd.Stdout = os.Stdout
        cmd.Stderr = os.Stderr
        if err := cmd.Run(); err == nil {
            return "python3", nil
        }
    }

    // Fallback: portable Python
    url := "https://github.com/indygreg/python-build-standalone/releases/download/20240107/cpython-3.12.1+20240107-x86_64-unknown-linux-gnu-install_only.tar.gz"
    tarball := filepath.Join(targetDir, "python-portable.tar.gz")
    outDir := filepath.Join(targetDir, "python-runtime")

    fmt.Println("Downloading portable Python...")
    if err := downloadFile(tarball, url); err != nil {
        return "", err
    }

    fmt.Println("Extracting portable Python...")
    if err := untar(tarball, outDir); err != nil {
        return "", err
    }

    python := filepath.Join(outDir, "bin", "python3")
    return python, nil
}

//
// HELPERS
//

func downloadFile(dst, url string) error {
    resp, err := http.Get(url)
    if err != nil {
        return err
    }
    defer resp.Body.Close()

    out, err := os.Create(dst)
    if err != nil {
        return err
    }
    defer out.Close()

    _, err = io.Copy(out, resp.Body)
    return err
}

func unzip(src, dest string) error {
    r, err := zip.OpenReader(src)
    if err != nil {
        return err
    }
    defer r.Close()

    os.MkdirAll(dest, 0755)

    for _, f := range r.File {
        fp := filepath.Join(dest, f.Name)

        if f.FileInfo().IsDir() {
            os.MkdirAll(fp, f.Mode())
            continue
        }

        if err := os.MkdirAll(filepath.Dir(fp), 0755); err != nil {
            return err
        }

        rc, err := f.Open()
        if err != nil {
            return err
        }
        defer rc.Close()

        out, err := os.OpenFile(fp, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, f.Mode())
        if err != nil {
            return err
        }

        _, err = io.Copy(out, rc)
        out.Close()
        if err != nil {
            return err
        }
    }

    return nil
}

func resolveExternalDirs(targetDir string) (string, string) {
	dataDir := filepath.Join(targetDir, "data")
	modelDir := filepath.Join(targetDir, "model")

	if _, err := os.Stat(dataDir); os.IsNotExist(err) {
		fmt.Println("WARNING: ./data folder not found at:", dataDir)
	}

	if _, err := os.Stat(modelDir); os.IsNotExist(err) {
		fmt.Println("WARNING: ./model folder not found at:", modelDir)
	}

	return dataDir, modelDir
}

func runEmbeddedScript(venvDir, scriptPath string, dataDir, modelDir string, args ...string) {
	python := venvPython(venvDir)

	fullArgs := append([]string{scriptPath}, args...)

	cmd := exec.Command(python, fullArgs...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	// inherit parent env
	env := os.Environ()
	env = append(env, "APP_DATA_DIR="+dataDir)
	env = append(env, "APP_MODEL_DIR="+modelDir)
	cmd.Env = env

	fmt.Println("Running embedded script:", scriptPath)
	fmt.Println("DATA_DIR:", dataDir)
	fmt.Println("MODEL_DIR:", modelDir)

	if err := cmd.Run(); err != nil {
		panic(fmt.Errorf("embedded script failed: %w", err))
	}
}
