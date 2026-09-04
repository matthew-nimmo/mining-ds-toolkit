package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
	"time"
)

// ============================================================================
// Core Types & Definitions
// ============================================================================

type Message struct {
	Role    string `json:"role"`
	Content string `json:"content"`
}

type OllamaChatRequest struct {
	Model    string    `json:"model"`
	Messages []Message `json:"messages"`
	Stream   bool      `json:"stream"`
}

type OllamaChatResponse struct {
	Message Message `json:"message"`
}

type Cue struct {
	Raw         string
	AgentName   string
	Task        string
	StartIndex  int
	EndIndex    int
}

// Tool represents either a hardcoded system skill or a dynamic script skill from SKILL.md
type Tool struct {
	Name        string
	Description string
	IsScript    bool
	ScriptType  string // "bash" or "go"
	ScriptBody  string
	Execute     func(arg string) (string, error)
}

type AgentConfig struct {
	Name  string
	Model string
}

type Harness struct {
	OllamaURL    string
	FilePath     string
	Agents       map[string]AgentConfig
	ToolRegistry map[string]Tool
	Mu           sync.Mutex
}

// ============================================================================
// Dynamic SKILL.md Parser & Executor Engine
// ============================================================================

// LoadDynamicSkills parses SKILL.md and extracts executable bash/Go workflows.
func (h *Harness) LoadDynamicSkills(skillsFilePath string) error {
	if _, err := os.Stat(skillsFilePath); os.IsNotExist(err) {
		log.Printf("[Harness Warning] %s not found. Skipping dynamic skill loading.\n", skillsFilePath)
		return nil
	}

	contentBytes, err := os.ReadFile(skillsFilePath)
	if err != nil {
		return err
	}
	content := string(contentBytes)

	// Regex to extract Skill block regions: ## Skill: <name>\nDescription: <desc>\nWorkflow:\n```<lang>\n<body>\n```
	skillRegex := regexp.MustCompile(`(?s)##\s*Skill:\s*([^\n]+)\nDescription:\s*([^\n]+)\nWorkflow:\s*\n\x60\x60\x60(bash|go)\n(.*?)\n\x60\x60\x60`)
	matches := skillRegex.FindAllStringSubmatch(content, -1)

	for _, match := range matches {
		name := strings.TrimSpace(match[1])
		description := strings.TrimSpace(match[2])
		scriptType := strings.TrimSpace(match[3])
		scriptBody := strings.TrimSpace(match[4])

		// Instantiate closure targeting shell or Go task runners
		toolName := name
		executableBody := scriptBody
		langType := scriptType

		h.ToolRegistry[toolName] = Tool{
			Name:        toolName,
			Description: description,
			IsScript:    true,
			ScriptType:  langType,
			ScriptBody:  executableBody,
			Execute: func(arg string) (string, error) {
				return ExecuteScriptSkill(langType, executableBody, arg)
			},
		}
		log.Printf("[Harness Registry] Loaded dynamic project skill [%s] successfully.\n", toolName)
	}
	return nil
}

// ExecuteScriptSkill writes raw dynamic steps out to runtime artifacts to execute safely offline.
// ExecuteScriptSkill writes raw dynamic steps out to runtime artifacts to execute safely offline.
// Now handles "r" language engines explicitly using a protected tryCatch runtime block.
func ExecuteScriptSkill(scriptType, scriptBody, arg string) (string, error) {
	log.Printf("[Skill Execution] Running dynamic script workflow via type: %s\n", scriptType)

	switch scriptType {
	case "r":
		// Create an ephemeral .R script asset to pass directly downstream to the local binary
		tmpDir, err := os.MkdirTemp("", "r_skill_*")
		if err != nil {
			return "", err
		}
		defer os.RemoveAll(tmpDir)
		tmpFile := filepath.Join(tmpDir, "pipeline_harness.R")

		// Wrap scriptBody safely inside an R tryCatch statement to trap errors and log clean execution telemetry
		protectedRCode := fmt.Sprintf(`
options(keep.source = TRUE)
execute_harness_block <- function() {
	# Target agent block code execution begins
	%s
}

tryCatch({
	execute_harness_block()
}, error = function(e) {
	cat("[HARNESS EXECUTION ERROR DETECTED]\n")
	cat("Message: ", conditionMessage(e), "\n")
	cat("Traceback:\n")
	print(sys.calls())
	quit(status = 1)
})
`, scriptBody)

		if err := os.WriteFile(tmpFile, []byte(protectedRCode), 0644); err != nil {
			return "", err
		}

		// Execute code locally via Rscript, passing the task argument vector cleanly
		cmd := exec.Command("Rscript", tmpFile, arg)
		var out bytes.Buffer
		var stderr bytes.Buffer
		cmd.Stdout = &out
		cmd.Stderr = &stderr

		err = cmd.Run()
		if err != nil {
			// If R dropped a non-zero exit code due to code syntax/logical failure, return contextually
			return fmt.Sprintf("[R Engine Crash Output]\n%s\n%s", out.String(), stderr.String()), nil
		}
		return out.String(), nil

	case "bash":
		cmd := exec.Command("bash", "-c", scriptBody)
		cmd.Env = append(os.Environ(), fmt.Sprintf("AGENT_ARGUMENT=%s", arg))
		
		var out bytes.Buffer
		var stderr bytes.Buffer
		cmd.Stdout = &out
		cmd.Stderr = &stderr
		
		err := cmd.Run()
		if err != nil {
			return "", fmt.Errorf("bash script error: %s - %v", stderr.String(), err)
		}
		return out.String(), nil

	case "go":
		tmpDir, err := os.MkdirTemp("", "go_skill_*")
		if err != nil {
			return "", err
		}
		defer os.RemoveAll(tmpDir)

		tmpFile := filepath.Join(tmpDir, "main.go")
		if err := os.WriteFile(tmpFile, []byte(scriptBody), 0644); err != nil {
			return "", err
		}

		cmd := exec.Command("go", "run", tmpFile, arg)
		var out bytes.Buffer
		var stderr bytes.Buffer
		cmd.Stdout = &out
		cmd.Stderr = &stderr

		if err := cmd.Run(); err != nil {
			return "", fmt.Errorf("go runtime skill execution error: %s - %v", stderr.String(), err)
		}
		return out.String(), nil
	}

	return "", fmt.Errorf("unsupported offline runtime skill environment target: %s", scriptType)
}

// ============================================================================
// Core Harness System Routines
// ============================================================================

func NewHarness(filePath string) *Harness {
	agents := map[string]AgentConfig{
		"narrative": {Name: "narrative", Model: "qwen3.5:4b-mlx"},
		"coder":     {Name: "coder", Model: "gemma4:e2b-mlx"},
	}

	h := &Harness{
		OllamaURL:    "http://localhost:11434/api/chat",
		FilePath:     filePath,
		Agents:       agents,
		ToolRegistry: make(map[string]Tool),
	}

	// Native built-in capability fallback
	h.ToolRegistry["search_local_docs"] = Tool{
		Name:        "search_local_docs",
		Description: "Lookup fallback standards documentation.",
		IsScript:    false,
		Execute: func(arg string) (string, error) {
			return "[Local RAG Match] Default project markdown layouts verified.", nil
		},
	}

	return h
}

// ParseCues finds all HTML comments first, then splits on the first ':' 
// separating the agent name from its explicit text prompt instruction.
func (h *Harness) ParseCues(content string) []Cue {
	// Step 1: Extract all standard HTML comment structures globally from the file
	re := regexp.MustCompile(`<!--([\s\S]*?)-->`)
	matches := re.FindAllStringSubmatchIndex(content, -1)

	var cues []Cue
	for _, match := range matches {
		rawComment := content[match[0]:match[1]]
        innerContent := content[match[2]:match[3]]

		// Step 2: Split the inner comment content on the FIRST colon only
		parts := strings.SplitN(innerContent, ":", 2)
		if len(parts) < 2 {
			// Skip structural HTML comments that don't match our DSL (e.g., )
			continue
		}

		agentName := strings.TrimSpace(parts[0])
		taskPrompt := strings.TrimSpace(parts[1])

		// Double-check that the extracted target is actually a managed agent
		if _, knownAgent := h.Agents[agentName]; !knownAgent {
			continue
		}

		cues = append(cues, Cue{
			Raw:        rawComment,
			AgentName:  agentName,
			Task:       taskPrompt,
			StartIndex: match[0],
			EndIndex:   match[1],
		})
	}
	return cues
}

func (h *Harness) CallOllama(ctx context.Context, model string, systemPrompt, userPrompt string) (string, error) {
	reqBody := OllamaChatRequest{
		Model:    model,
		Messages: []Message{{Role: "system", Content: systemPrompt}, {Role: "user", Content: userPrompt}},
		Stream:   false,
	}

	jsonBytes, err := json.Marshal(reqBody)
	if err != nil {
		return "", err
	}

	req, err := http.NewRequestWithContext(ctx, "POST", h.OllamaURL, bytes.NewBuffer(jsonBytes))
	if err != nil {
		return "", err
	}
	req.Header.Set("Content-Type", "application/json")

	client := &http.Client{Timeout: 90 * time.Second}
	resp, err := client.Do(req)
	if err != nil {
		return "", fmt.Errorf("local offline link down: %w", err)
	}
	defer resp.Body.Close()

	respBytes, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}

	var chatResp OllamaChatResponse
	if err := json.Unmarshal(respBytes, &chatResp); err != nil {
		return "", err
	}
	return chatResp.Message.Content, nil
}

func (h *Harness) ExecuteCue(ctx context.Context, cue Cue, docContext string) (string, error) {
	agent, exists := h.Agents[cue.AgentName]
	if !exists {
		return "", fmt.Errorf("agent %s is unassigned", cue.AgentName)
	}

	// Contextual Routing Discovery Layer
	var skillContextInjections []string
	for toolName, tool := range h.ToolRegistry {
		// If SLM explicitly demands or references a task name matching a dynamic skill token
		if strings.Contains(strings.ToLower(cue.Task), strings.ToLower(toolName)) {
			log.Printf("[Harness Routing] Intercepted skill flag in task prompt. Driving tool: %s\n", toolName)
			out, err := tool.Execute(cue.Task)
			if err != nil {
				skillContextInjections = append(skillContextInjections, fmt.Sprintf("[Skill Error on %s]: %v", toolName, err))
			} else {
				skillContextInjections = append(skillContextInjections, fmt.Sprintf("[Skill Result %s]:\n%s", toolName, out))
			}
		}
	}

	systemPrompt := `You are an operational agent within a local automation harness.
Fulfill the task directly. You can pass tasks downstream to other expert agents using this HTML template match:
<!-- target: prompt -->
Targets available: 'narrative', 'coder'. Keep responses clean without conversational fluff.`

	userPrompt := fmt.Sprintf("Document Section context:\n%s\n\nTask: %s\nExecuted Skills Results Content:\n%s\nOutput Markdown:", 
		docContext, cue.Task, strings.Join(skillContextInjections, "\n\n"))

	return h.CallOllama(ctx, agent.Model, systemPrompt, userPrompt)
}

func (h *Harness) Run(ctx context.Context) error {
	for iteration := 1; iteration <= 5; iteration++ {
		h.Mu.Lock()
		b, err := os.ReadFile(h.FilePath)
		if err != nil {
			h.Mu.Unlock()
			return err
		}
		content := string(b)
		cues := h.ParseCues(content)

		if len(cues) == 0 {
			log.Println("[Harness] Execution queue complete. Workspace resolved.")
			h.Mu.Unlock()
			return nil
		}

		targetCue := cues[0]
		
		startWin := targetCue.StartIndex - 200
		if startWin < 0 { startWin = 0 }
		endWin := targetCue.EndIndex + 200
		if endWin > len(content) { endWin = len(content) }
		contextWindow := content[startWin:endWin]
		h.Mu.Unlock()

		output, err := h.ExecuteCue(ctx, targetCue, contextWindow)
        fmt.Println(output)
		if err != nil {
			return err
		}

		h.Mu.Lock()
		freshBytes, err := os.ReadFile(h.FilePath)
		if err != nil {
			h.Mu.Unlock()
			return err
		}
		
		processedTag := fmt.Sprintf("%s\n\n", targetCue.AgentName)
		updatedContent := strings.Replace(string(freshBytes), targetCue.Raw, processedTag+output+"\n", 1)
		
		err = os.WriteFile(h.FilePath, []byte(updatedContent), 0644)
		h.Mu.Unlock()
		if err != nil {
			return err
		}

		time.Sleep(500 * time.Millisecond)
	}
	return nil
}

func fileExists(filename string) bool {
	_, err := os.Stat(filename)
	if err == nil {
		return true // File exists
	}
	if errors.Is(err, os.ErrNotExist) {
		return false // File explicitly does not exist
	}
	// The file might exist, but we got a different error (e.g., permission denied)
	return false 
}

// ============================================================================
// Main Workspace Harness Driver
// ============================================================================

func main() {
	ctx := context.Background()
	skillsFile := "SKILL.md"
	docFile := "canvas.qmd"

	var mockSkills string
	if fileExists(skillsFile) {
		content, err := os.ReadFile(skillsFile)
		if err != nil {
			log.Fatalf("Failed to read file: %s", err)
		}
		mockSkills = string(content)
	} else {
		// Mock writing out workspace configuration state files
		mockSkills = `# Project Custom Skills
## Skill: build_report
Description: Compiles metrics report into production state layout.
Workflow:
` + "```bash\necho \"[System Command Executed] Local workspace compiled via dynamic build_report skill!\"\n```"
		_ = os.WriteFile(skillsFile, []byte(mockSkills), 0644)
	}

	var mockDoc string
	if fileExists(docFile) {
		content, err := os.ReadFile(docFile)
		if err != nil {
			log.Fatalf("Failed to read file: %s", err)
		}
		mockDoc = string(content)
	} else {
		// Mock writing out workspace configuration state files
		mockDoc = `---
title: "Dynamic Skill-Injected Run"
---

## Section 1

<!-- narrative: Describe the city of Brisbane in Queensland, Australia -->

`
		_ = os.WriteFile(docFile, []byte(mockDoc), 0644)
	}
	
	// Clean up mock assets upon completion
	//defer os.Remove(skillsFile)
	//defer os.Remove(docFile)

	harness := NewHarness(docFile)
	
	// Load the external tools dynamically inside the active pipeline loop
	if err := harness.LoadDynamicSkills(skillsFile); err != nil {
		log.Fatalf("Critical Skill Load Failure: %v", err)
	}

	if err := harness.Run(ctx); err != nil {
		log.Fatalf("Harness error loop trip: %v", err)
	}

	finalOutput, _ := os.ReadFile(docFile)
	fmt.Println("\n================ RESULTING MARKDOWN DOC WITH SKILL RUN ================")
	fmt.Println(string(finalOutput))
}