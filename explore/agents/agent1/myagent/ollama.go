package myagent

import (
    "bytes"
    "context"
    "encoding/json"
    "io"
    "net/http"
)

// OllamaClient implements LLMClient using Ollama
type OllamaClient struct {
    baseURL string
    model   string
    client  *http.Client
}

func NewOllamaClient(baseURL, model string) *OllamaClient {
    return &OllamaClient{
        baseURL: baseURL,
        model:   model,
        client:  &http.Client{},
    }
}

type OllamaRequest struct {
    Model  string `json:"model"`
    Prompt string `json:"prompt"`
    Stream bool   `json:"stream"`
}

type OllamaResponse struct {
    Response string `json:"response"`
}

func (o *OllamaClient) GenerateResponse(ctx context.Context, prompt string) (string, error) {
    req := OllamaRequest{
        Model:  o.model,
        Prompt: prompt,
        Stream: false,
    }

    body, err := json.Marshal(req)
    if err != nil {
        return "", err
    }

    httpReq, err := http.NewRequestWithContext(ctx, "POST", o.baseURL+"/api/generate", bytes.NewBuffer(body))
    if err != nil {
        return "", err
    }

    httpReq.Header.Set("Content-Type", "application/json")

    resp, err := o.client.Do(httpReq)
    if err != nil {
        return "", err
    }
    defer resp.Body.Close()

    respBody, err := io.ReadAll(resp.Body)
    if err != nil {
        return "", err
    }

    var ollamaResp OllamaResponse
    if err := json.Unmarshal(respBody, &ollamaResp); err != nil {
        return "", err
    }

    return ollamaResp.Response, nil
}
