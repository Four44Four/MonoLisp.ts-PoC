# AGENTS.md — Context Guide & Operational Rules

> **FOR COGNITIVE AGENTS (GPT Sol, Fable, etc...):** Read this file first. Adhere strictly to the architectural constraints, prompt formats, and context rules outlined below to optimize token efficiency and codebase safety.

---

## 1. Context Boundaries & Token Efficiency

### Context Pruning
* DO NOT ingest or process directories matching `.gitignore` or `.codexignore` (e.g., `node_modules`, `dist`, `build`, `.next`, or local databases).
* If a file is unrelated to the current feature implementation, request only its "skeleton" (exports, interfaces, and type signatures) rather than its complete implementation logic by delegating to a cheaper sub-agent.

### Output Miniaturization (Crucial)
* **Output tokens cost 5x more than input tokens.** Never rewrite an entire file to change a few lines.
* Aim to strategically modify functions/components unless major refactors are explicitly requested or absolutely necessary

## Output delegation
* Delegate out most actual coding output to cheaper agents fit for the job
* Provide cheaper agents with clear and exact instructions on how to implement the code
* Include a language syntax guide to Common Lisp as smaller models may not be well trained in its syntax and patterns 
* Only output using the cognitive agent when the output is determined to be too complex for a cheaper agent to properly do

---

## 2. Agent-Friendly Core Architecture

Aim to keep the framework codebase modularized.
* Prefer smaller files which have descriptive names to avoid overfetching of irrelevant information
* If changing an existing large framework codebase file to be smaller, notify developer that such a change was made and highlight all the files that it was split into or any code that was culled (prefer splitting existing files over culling if such as an action is necessary)

---

## 3. Framework Conventions

* Focus on security at all client-server boundary interactions by defensively coding against possible existing and future threats (this is where most cybersecurity vulernabilities with this type of framework arise)
* Explicit type safety via user provided types should be considered for all client-server data boundaries (if prompter's intent is ambiguous, ask for clarification)

---