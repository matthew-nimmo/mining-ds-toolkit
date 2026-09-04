package myagent

import (
    "context"
    "fmt"
    "math"
    "strconv"
    "strings"
)

// CalculatorTool performs basic math operations
type CalculatorTool struct{}

func (c *CalculatorTool) Name() string {
    return "calculator"
}

func (c *CalculatorTool) Description() string {
    return "Performs basic math operations. Input format: 'add 5 3', 'multiply 10 2', etc."
}

func (c *CalculatorTool) Execute(ctx context.Context, input string) (string, error) {
    parts := strings.Fields(input)
    if len(parts) < 3 {
        return "", fmt.Errorf("invalid input format")
    }

    operation := parts[0]
    num1, err := strconv.ParseFloat(parts[1], 64)
    if err != nil {
        return "", err
    }
    num2, err := strconv.ParseFloat(parts[2], 64)
    if err != nil {
        return "", err
    }

    var result float64
    switch operation {
    case "add":
        result = num1 + num2
    case "subtract":
        result = num1 - num2
    case "multiply":
        result = num1 * num2
    case "divide":
        if num2 == 0 {
            return "", fmt.Errorf("division by zero")
        }
        result = num1 / num2
    case "power":
        result = math.Pow(num1, num2)
    default:
        return "", fmt.Errorf("unknown operation: %s", operation)
    }

    return fmt.Sprintf("%.2f", result), nil
}

// WebSearchTool simulates searching the web
type WebSearchTool struct{}

func (w *WebSearchTool) Name() string {
    return "web_search"
}

func (w *WebSearchTool) Description() string {
    return "Searches the web for information. Input: search query"
}

func (w *WebSearchTool) Execute(ctx context.Context, input string) (string, error) {
    // In a real implementation, you'd call an actual search API
    // For this tutorial, we'll return a mock result
    return fmt.Sprintf("Search results for '%s': Found relevant information...", input), nil
}

// FileReaderTool reads file contents
type FileReaderTool struct{}

func (f *FileReaderTool) Name() string {
    return "read_file"
}

func (f *FileReaderTool) Description() string {
    return "Reads the contents of a file. Input: file path"
}

func (f *FileReaderTool) Execute(ctx context.Context, input string) (string, error) {
    // In a real implementation, you'd read the actual file
    return fmt.Sprintf("Contents of %s: [simulated file content]", input), nil
}
