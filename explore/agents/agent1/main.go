// https://tutorialedge.net/ai/building-ai-agents-in-go/
package main

import (
    "context"
    "fmt"
    "log"
    "agent/myagent"
)

func main() {
    ctx := context.Background()

    // Set up tools
    toolRegistry := myagent.NewToolRegistry()
    toolRegistry.Register(&myagent.CalculatorTool{})
    toolRegistry.Register(&myagent.WebSearchTool{})
    toolRegistry.Register(&myagent.FileReaderTool{})

	// LLM usage
	// qwen2.5-coder:1.5b-base - IDE code complete / code focused
	// qwen3.5:4b - vibe coding / reasoning
	// granite3.1-moe:3b - text summarization
	// deepseek-r1:1.5b - thinking / reasoning focused
	// hermes3:8b - agentic workflows / reasoning
	// gemma4:4b - text
	// gemma4:26b - local agents / reasoning
	// granite4.1:8b - local agents / coding / reasoning
	// lfm2:24b - text summarisation / RAG

    // Best LLMs
    // qwen3.5:4b default workhorse - best small model for agentic loops tool use (local)
    // qwen2.5:7b model‑closer + structured tasks - best router for stability (local)
    // deepseek‑r1:8b reasoning-heavy tasks - best reasoning model (local)
    // gemma4:26b optional MoE if RAM allows (local)
    // qwen2.5:14b main planner + codegen - best all-rounder (server)
    // gemma2:27b final reviewer - best narrative + review (server)
    // deepseek‑r1:32b deep reasoning - best reasoning (server)

    // mistral passed
	// llava passed
	// llama3.1:8b failed
	// llama3.2:3b passed (too many iterations)
	// granite4:3b passed (needed to fix JSON loader)
    // granite4.1:3b passed (needed to fix JSON loader)
	// granite4.1:8b passed
    // phi3:latest (Correct answer but JSON format wrong)
    // phi3.5:latest (Correct answer but JSON format wrong)
	// deepseek-r1:1.5b passed
    // deepseek-r1:latest passed
	// qwen2.5:3b failed
	// qwen3.5:4b passed
	// qwen3.5:9b passed
    // gemma4:e4b passed
	// gemma4:26b passed (quick)

    // Initialize LLM client (using local Ollama)
    llmClient := myagent.NewOllamaClient("http://localhost:11434", "granite4.1:3b")

    // Create agent
    agentInstance := myagent.NewAgent(toolRegistry, llmClient)

    // Run the agent on a task
    task := "Find out what 45 multiplied by 12 is, and then tell me if it's greater than 500"

    log.Println("Starting agent with task:", task)

    result, err := agentInstance.Run(ctx, task)
    if err != nil {
        log.Fatalf("Agent failed: %v", err)
    }

    fmt.Println("Final Result:")
    fmt.Println(result)
}