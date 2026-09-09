package main

import (
	"fmt"
	"os"
	"path/filepath"
)

func main() {
    targetDir := getTargetDir()

    rscript, err := installRIfMissing(targetDir)
    if err != nil {
        panic(err)
    }

    libDir := filepath.Join(targetDir, "rlib")

    pkgPath := extractRPackage(targetDir)
    installRPackage(rscript, libDir, pkgPath)

    scriptsDir := extractRScripts(targetDir)
    script := filepath.Join(scriptsDir, "myscript.R")

    dataDir, modelDir := resolveExternalDirs(targetDir)

    runRScript(rscript, script, libDir, dataDir, modelDir)
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

func findR() (string, error) {
    candidates := []string{"Rscript", "R"}

    if runtime.GOOS == "windows" {
        candidates = append([]string{
            "Rscript.exe",
            "R.exe",
        }, candidates...)
    }

    for _, c := range candidates {
        if path, err := exec.LookPath(c); err == nil {
            return path, nil
        }
    }

    return "", errors.New("R not found")
}

func installRWindows(targetDir string) (string, error) {
    url := "https://cloud.r-project.org/bin/windows/base/R-4.4.1-win.exe"
    installer := filepath.Join(targetDir, "R-installer.exe")

    fmt.Println("Downloading R for Windows...")
    if err := downloadFile(installer, url); err != nil {
        return "", err
    }

    fmt.Println("Running R installer...")
    cmd := exec.Command(installer, "/VERYSILENT")
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    if err := cmd.Run(); err != nil {
        return "", err
    }

    return "Rscript.exe", nil
}

func installRMac(targetDir string) (string, error) {
    url := "https://cloud.r-project.org/bin/macosx/big-sur-arm64/base/R-4.4.1-arm64.pkg"
    pkg := filepath.Join(targetDir, "R.pkg")

    fmt.Println("Downloading R for macOS...")
    if err := downloadFile(pkg, url); err != nil {
        return "", err
    }

    fmt.Println("Installing R (requires admin)...")
    cmd := exec.Command("sudo", "installer", "-pkg", pkg, "-target", "/")
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    if err := cmd.Run(); err != nil {
        return "", err
    }

    return "/usr/local/bin/Rscript", nil
}

func installRLinux(targetDir string) (string, error) {
    if _, err := exec.LookPath("apt-get"); err == nil {
        cmd := exec.Command("sudo", "apt-get", "install", "-y", "r-base")
        cmd.Stdout = os.Stdout
        cmd.Stderr = os.Stderr
        if err := cmd.Run(); err == nil {
            return "Rscript", nil
        }
    }

    if _, err := exec.LookPath("dnf"); err == nil {
        cmd := exec.Command("sudo", "dnf", "install", "-y", "R")
        cmd.Stdout = os.Stdout
        cmd.Stderr = os.Stderr
        if err := cmd.Run(); err == nil {
            return "Rscript", nil
        }
    }

    return "", errors.New("could not install R automatically")
}

func installRIfMissing(targetDir string) (string, error) {
    r, err := findR()
    if err == nil {
        return r, nil
    }

    fmt.Println("R not found. Installing...")

    switch runtime.GOOS {
    case "windows":
        return installRWindows(targetDir)
    case "darwin":
        return installRMac(targetDir)
    case "linux":
        return installRLinux(targetDir)
    default:
        return "", errors.New("unsupported OS")
    }
}

func extractRPackage(targetDir string) string {
    outDir := filepath.Join(targetDir, "embedded_rpkg")
    os.MkdirAll(outDir, 0755)

    entries, _ := embeddedRPackage.ReadDir("rembedded/pkg")

    var pkgPath string
    for _, e := range entries {
        data, _ := embeddedRPackage.ReadFile("rembedded/pkg/" + e.Name())
        dst := filepath.Join(outDir, e.Name())
        os.WriteFile(dst, data, 0644)
        pkgPath = dst
    }

    return pkgPath
}

func installRPackage(rscript, libDir, pkgPath string) {
    os.MkdirAll(libDir, 0755)

    cmd := exec.Command(
        rscript,
        "--vanilla",
        "-e",
        fmt.Sprintf("install.packages('%s', repos=NULL, lib='%s')", pkgPath, libDir),
    )

    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    if err := cmd.Run(); err != nil {
        panic(err)
    }
}

func extractRScripts(targetDir string) string {
    outDir := filepath.Join(targetDir, "embedded_rscripts")
    os.MkdirAll(outDir, 0755)

    entries, _ := embeddedRScripts.ReadDir("rembedded/scripts")

    for _, e := range entries {
        data, _ := embeddedRScripts.ReadFile("rembedded/scripts/" + e.Name())
        dst := filepath.Join(outDir, e.Name())
        os.WriteFile(dst, data, 0644)
    }

    return outDir
}

func runRScript(rscript, scriptPath, libDir, dataDir, modelDir string) {
    cmd := exec.Command(
        rscript,
        "--vanilla",
        "-e",
        fmt.Sprintf("source('%s')", scriptPath),
    )

    env := os.Environ()
    env = append(env, "R_LIBS_USER="+libDir)
    env = append(env, "APP_DATA_DIR="+dataDir)
    env = append(env, "APP_MODEL_DIR="+modelDir)
    cmd.Env = env

    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr

    if err := cmd.Run(); err != nil {
        panic(err)
    }
}

