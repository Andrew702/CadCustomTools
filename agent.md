# Agent Workflow Instructions

Read this file at the start of every session in this repository and follow these rules strictly:

## Git Branching & Commit Workflow

1. **Ask before starting any feature/task whether to work on `main` or a separate branch**:
   - Before making any edits for a new feature or task, ask the user whether they want the work implemented directly on `main` or on a separate branch (e.g., `feat/<topic>`, `fix/<topic>`, `chore/<topic>`).
   - If the user chooses a separate branch, create and switch to it from `main` before editing.
   - If the user chooses `main`, work directly on `main`.
2. **Commit after every file change**:
   - After modifying, creating, or deleting any file in the environment, immediately stage and commit that change to the active branch with a clear commit message.
3. **Push work to the active branch**:
   - If working on a separate branch, push commits to the remote branch (`git push -u origin <branch-name>`) so work is always backed up on GitHub without touching `main`.
   - If working on `main` (per the user's choice), push commits to `origin main`.
4. **Only merge a separate branch into `main` on explicit user instruction**:
   - When working on a separate branch, do **not** merge into `main` or push to `main` unless the user explicitly tells you to merge/push to `main`.
   - When the user instructs to merge/push to `main`, merge the current branch into `main` and push `origin main`.
