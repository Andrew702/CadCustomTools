# CadCustomTools — SelectionHistory (ZWCAD)

Automatic 5-slot selection history for ZWCAD. Silently records your last 5 object selections — whether made **outside a command** (pickfirst / implied selection) or **inside a command** (`Select objects:` prompts) — and lets you recall any slot via quick commands, an interactive picker dialog, or transparently inside an active command.

## Structure

- `SelectionHistory.lsp` — Entry point, module loader, auto-start reactor setup, commands (`SH`, `SELHIST`, `SH1`–`SH5`), and transparent recall helpers (`(sh 1)`–`(sh 5)`, `(sh1)`–`(sh5)`)
- `src/sh_store.lsp` — 5-slot handle-based storage, canonical set deduplication, compact object-type summaries, live entity validation, and recall logic
- `src/sh_reactor.lsp` — Reload-safe Visual LISP reactors (`vlr-miscellaneous-reactor` for `:vlr-pickfirstModified` + `vlr-command-reactor` for command start/end/cancel/fail)
- `src/sh_dialog.lsp` — Dynamic DCL picker dialog showing all 5 slots with live/original counts, source command, type summary, quick `1`–`5` buttons, and double-click recall

## Setup in ZWCAD

1. Add the repository folder (or `CadCustomTools/SelectionHistory`) to ZWCAD's **Support File Search Path** (`OPTIONS` → **Files** → **Support File Search Path**).
2. Run `APPLOAD` and load `SelectionHistory.lsp` (recommended: add it to your **Startup Suite** so tracking starts automatically for every drawing).
   - If you load `SelectionHistory.lsp` without adding the folder to Support File Search Path, it will prompt once to locate `SelectionHistory.lsp` so it can load `src/*`.

## Commands & Recall Methods

| Method | Usage | Description |
| --- | --- | --- |
| `SH` / `SELHIST` | Outside a command | Opens the **Selection History** dialog showing slots `#1`–`#5` (newest first) with live counts, source, and object-type summaries. Double-click a row or click `1`–`5` to recall. |
| `SH1` – `SH5` | Outside a command | Immediately recalls Slot `#1` (most recent) through `#5` with active pickfirst grips (`sssetfirst`). |
| `(sh 1)` – `(sh 5)` | Inside any `Select objects:` prompt | Transparently returns the slot's selection set directly to the running command (e.g., `MOVE` → `Select objects: (sh 2)`). Short aliases `(sh1)`–`(sh5)` also work. |

## Behavior & Notes

- **Order preserved on recall:** Recalling an older slot (e.g., `SH3`) keeps slots `#1`–`#5` in the same order so you can cycle predictably without slots jumping around. If you add/remove objects from a recalled selection, the modified set is recorded as a new selection.
- **Incremental pickfirst buffering:** Clicking multiple objects one by one outside a command buffers them into a single selection and commits it only when you finish (press `Esc`, deselect, or start a command).
- **Deduplication:** Noun-verb workflows (select objects → `MOVE`) are recorded once, and non-selecting commands (`ZOOM`, `PAN`, `LINE`, `QSAVE`) never create duplicates.
- **Erased entities:** Selections are stored by entity handle (avoiding AutoLISP's 128-selection-set limit). Erased entities are automatically skipped on recall and reported on the command line.
- **Performance guard:** Selections larger than 5,000 entities (configurable via `*sh-max-entities*`) are skipped to prevent lag on `Ctrl+A` in massive drawings.
- **Per-drawing scope:** History is stored in memory per open drawing and resets when the drawing is closed.

## Known CAD / AutoLISP Limitations

- **Multi-prompt commands:** Commands that ask for multiple separate selections (e.g., `TRIM` cutting edges, then objects to trim) only expose the final selection set via `(ssget "_P")`.
- **Single-pick sub-commands:** Commands like `FILLET` or `CHAMFER` do not update CAD's internal `_P` selection set.
