# AGENTS.md — Context Guide & Operational Rules

> **FOR COGNITIVE AGENTS (GPT Sol, Fable, etc...):** Read this file first. Adhere strictly to the architectural constraints, prompt formats, and context rules outlined below to optimize token efficiency and codebase safety.

---

## 1. COMPREHENSION BOUNDARIES & SOURCE OF TRUTH
*   **The Codebase is Niche:** The codebase is written in **Common Lisp (ANSI)**. Do not assume standard modern programming patterns (like OOP or procedural loops) apply unless explicitly defined in the documentation.
*   **Documentation-First Execution:** You do not have complete spatial awareness of this repository. Before proposing any architectural mutations or generating delegation payloads, you **must query the `/docs` directory or inspect matching files in `examples/`**. 
*   **No Exploratory File Dumps:** Do not use broad file-system searches or read entire directories. Use precise keyword grepping (`grep -n`) to pinpoint definitions, package layouts, and system configurations.

---

## 2. Context Boundaries & Token Efficiency

### Context Pruning
* DO NOT ingest or process directories matching `.gitignore` or `.codexignore` (e.g., `node_modules`, `dist`, `build`, `.next`, or local databases).
* If a file is unrelated to the current feature implementation, request only its "skeleton" (exports, interfaces, and type signatures) rather than its complete implementation logic by delegating to a cheaper sub-agent.

### Output Miniaturization (Crucial)
* **Output tokens cost 5x more than input tokens.** Never rewrite an entire file to change a few lines.
* Aim to strategically modify functions/components unless major refactors are explicitly requested or absolutely necessary

---

## 3. DELEGATION PROTOCOL FOR SUB-AGENTS
By default, you are a planner. You must delegate implementation to smaller worker models using a strict, machine-parseable schema. However, because Common Lisp is a non-mainstream language, you **must bundle a localized Syntax Sheet** into every payload to prevent smaller models from emitting invalid or C-style code.

### Required Delegation Format:
Every delegation output must strictly use the following layout:

```text
[DELEGATE_TASK]
- Target File: <path/to/file.lisp>
- System Package: <package-name>
- Expected Forms: <list of functions/macros to be defined or modified>
- Functional Contract: <input arguments, types, and return values>

# Localized Lisp Syntax Sheet:
* [Syntax Guideline 1: e.g., Ensure all parentheses match exactly; do not use Clojure [] or {} brackets.]
* [Syntax Guideline 2: e.g., Use `loop` clause variations strictly as shown in example code, or stick to `dolist`/`dotimes` if simpler.]
* [Code Template: Provide a 3-to-5 line structural template demonstrating the exact macro/syntax needed for this specific ticket.]

# Verification Criteria:
* Must pass REPL compilation without warnings.
* [Specific unit test scenario or assertion required]
[END_DELEGATE_TASK]
```

### Escalation Triggers (When to bypass sub-agents)
You may bypass delegation and output Common Lisp code directly **only** under the following critical scenarios:

1.  **Macro Transformations:** The task requires writing or modifying a complex macro (`defmacro`) that relies on backquotes, commas, or symbol-splicing (`,@`), which smaller models consistently corrupt.
2.  **REPL/Compiler Loop Failures:** A sub-agent has attempted a task and failed compilation twice due to package locks, symbol conflicts, or macro-expansion errors that require deep architectural reasoning.
3.  **Tightly-Coupled Condition Systems:** The change requires weaving complex condition handling (`handler-case`, `restart-case`, or `define-condition`) across multiple structural boundaries.

When an escalation trigger is met, append the tag `[STATUS: ESCALATED_EXECUTION]` to your response and output the minimal, precise code delta required.

---

## 4. Agent-Friendly Core Architecture

Aim to keep the framework codebase modularized.
* Prefer smaller files which have descriptive names to avoid overfetching of irrelevant information
* If changing an existing large framework codebase file to be smaller, notify developer that such a change was made and highlight all the files that it was split into or any code that was culled (prefer splitting existing files over culling if such as an action is necessary)

---

## 5. Framework Conventions

* Focus on security at all client-server boundary interactions by defensively coding against possible existing and future threats (this is where most cybersecurity vulernabilities with this type of framework arise)
* Explicit type safety via user provided types should be considered for all client-server data boundaries (if prompter's intent is ambiguous, ask for clarification)
* Keep /docs and /examples directories updated as the framework codebase's user-facing API changes 

---