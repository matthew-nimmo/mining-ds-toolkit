package myagent

import (
    "context"
)

// Tool represents a capability the agent can use
type Tool interface {
    // Name returns the name of the tool
    Name() string

    // Description explains what the tool does
    Description() string

    // Execute runs the tool with the given input
    Execute(ctx context.Context, input string) (string, error)
}

// ToolRegistry holds all available tools
type ToolRegistry struct {
    tools map[string]Tool
}

func NewToolRegistry() *ToolRegistry {
    return &ToolRegistry{
        tools: make(map[string]Tool),
    }
}

func (tr *ToolRegistry) Register(tool Tool) {
    tr.tools[tool.Name()] = tool
}

func (tr *ToolRegistry) Get(name string) Tool {
    return tr.tools[name]
}

func (tr *ToolRegistry) List() []Tool {
    tools := make([]Tool, 0, len(tr.tools))
    for _, tool := range tr.tools {
        tools = append(tools, tool)
    }
    return tools
}