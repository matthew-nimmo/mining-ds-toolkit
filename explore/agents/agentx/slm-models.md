# Coding

Qwen 2.5 Coder 14B - code generation, debugging, code review, refactoring (base coder)
DeepSeek Coder - debugging, complex code analysis, 300+ languages
Phi-4 - math, logical tasks, structured code

# Text

Llama 3.3 8B - general chat, RAG, text writing, code
Mistral 7B - text regeneration, API testing, automation, fast responses
Gemma 4 E4B - general chat, image and screenshot analysis, thinking mode for more complex tasks

# Reasoning

DeepSeek R1 - chain-of-thought reasoning with reinforcement learning
QwQ - mathematics, structured analysis, if already in the Qwen ecosystem

# RAG

Llama 3.3 8B – 128K context, holds long document context well
Qwen 2.5 14B – 128K context, better quality on analytical tasks with documents

# Low RAM

General chat and text: Llama 3.3 8B – ollama pull llama3.3:8b
Code and programming: Qwen 2.5 Coder 7B – ollama pull qwen2.5-coder:7b
Fast responses: Mistral 7B – ollama pull mistral
Mathematics and logic: Phi-4 Mini – ollama pull phi4-mini
Multimodality and text on 8 GB: Gemma 4 E4B – ollama pull gemma4:e4b
Less than 4 GB RAM: Gemma 4 E2B – ollama pull gemma4:e2b
Reasoning on 8 GB: DeepSeek R1 8B – ollama pull deepseek-r1:8b


✔ General start, 8 GB RAM → Llama 3.3 8B
✔ Code, 16 GB RAM → Qwen 2.5 Coder 14B
✔ Code, 8 GB RAM → Qwen 2.5 Coder 7B
✔ Maximum speed → Mistral 7B
✔ Math and logic → Phi-4 or DeepSeek R1
✔ Complex analysis → DeepSeek R1 or QwQ
✔ RAG and documents → Llama 3.3 + nomic-embed-text
✔ Images and multimodality → Gemma 4 E4B or Llama 3.2 Vision
✔ Less than 4 GB RAM → Gemma 4 E2B or Phi-4 Mini

# How to test

## Step 1. Download and Run

ollama pull llama3.3:8b
ollama run llama3.3:8b📋

## Step 2. Check Quality on Your Task

✔ For code: "Write a Python function that [your task]" – check if the code runs without errors
✔ For text: "Rephrase this paragraph in a business style" – compare the result with the original
✔ For analysis: "Summarize this document in 5 points" – paste real work text
✔ For reasoning: "Solve the problem step by step: [mathematical or logical problem]"

## Step 3. Check Speed

After the response, Ollama shows tokens/sec. For comfortable work – at least 10–15 tokens/sec. If less – consider a smaller model or Q4_K_M instead of Q8.

## Step 4. Compare Two Candidates on the Same Prompt

Terminal 1:
ollama run llama3.3:8b "Write a Python function for parsing JSON"

Terminal 2:
ollama run qwen2.5-coder:7b "Write a Python function for parsing JSON"

#3 Step 5. Choose and Remove Unnecessary

The model that gives a better result on your task is your primary one. The rest can be removed to free up disk space:

ollama rm model-name
