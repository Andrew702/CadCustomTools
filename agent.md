# Agent Workflow Instructions

Read this file at the start of every session in this repository and follow these rules strictly:

## Git Branching & Commit Workflow

1. **Always work on a branch (never directly on `main`)**:
   - Before making any edits, ensure you are on a dedicated feature/task branch (e.g., `feat/<topic>`, `fix/<topic>`, `chore/<topic>`). Create one from `main` if you are currently on `main`.
2. **Commit after every file change**:
   - After modifying, creating, or deleting any file in the environment, immediately stage and commit that change to the active branch with a clear commit message.
3. **Push work to the branch**:
   - Push commits to the remote feature branch (`git push -u origin <branch-name>`) so work is always backed up on GitHub without touching `main`.
4. **Only merge/push to `main` on explicit user instruction**:
   - Do **not** merge into `main` or push to `main` unless the user explicitly tells you to push/merge to `main`.
   - When the user instructs to push to `main`, merge the current branch into `main` and push `origin main`.
