---
name: selection-history
description: Technical documentation and debugging reference for the ZWCAD SelectionHistory AutoLISP tool (SH, SELHIST, SH1-SH5, (sh n), SHDEBUG). Use when modifying, troubleshooting, or auditing Visual LISP reactors, 5-slot handle history storage, recall suppression, or SelectionHistory JSONL debug logs.
---

# SelectionHistory (ZWCAD AutoLISP Tool)

Automatic 5-slot selection history for ZWCAD. Silently records the user's last 5 object selections — made outside a command (pickfirst / implied selection) or inside a command (`_P` previous selection set) — via Visual LISP reactors, supports quick/dialog/transparent mid-command recall without reordering slots, and writes structured JSONL debug logs.

## Module Architecture (`SelectionHistory/`)

All symbols use the isolated `sh-` / `*sh-*` namespace.

| File | Responsibility |
| --- | --- |
| `SelectionHistory/SelectionHistory.lsp` | Entry point, root-directory resolver (`sh-resolve-root-dir`), module loader (`sh-load-modules`), auto-install of reactors on load, commands `SH`, `SELHIST`, `SH1`–`SH5`, `SHDEBUG`, and transparent functions `(sh 1)`–`(sh 5)` / `(sh1)`–`(sh5)`. |
| `SelectionHistory/src/sh_log.lsp` | Self-contained, `DIMZIN`-safe JSONL debug logger writing to `SelectionHistory/logs/selectionhistory_YYYYMMDD.jsonl` (fallback `TEMPPREFIX`). Uses a separate `cmd: "REACTOR"` / `run_id: "sh-reactor-YYYYMMDD"` stream for background reactor events so explicit command `RUN_START`/`RUN_END` pairs stay isolated. |
| `SelectionHistory/src/sh_store.lsp` | 5-slot handle-based ring buffer (`*sh-history*`), O(N) chunked string join (`sh-join-strings`), canonical sorted-handle key (`sh-handles->key`), top-3 type summary (`sh-format-type-summary`), snapshot builder (`sh-ss->snapshot`), deduplicated push (`sh-push-snapshot`), live entity validation (`sh-slot-live-info`, `sh-build-live-ss`), and slot recall (`sh-recall-slot`). |
| `SelectionHistory/src/sh_reactor.lsp` | Reload-safe Visual LISP reactors: `:vlr-miscellaneous-reactor` (`SH_PICKFIRST_REACTOR` → `:vlr-pickfirstModified`) and `:vlr-command-reactor` (`SH_COMMAND_REACTOR` → `:vlr-commandWillStart`, `:vlr-commandEnded`, `:vlr-commandCancelled`, `:vlr-commandFailed`). Protected via `vl-catch-all-apply` with `REACTOR_ERROR` logging. |
| `SelectionHistory/src/sh_dialog.lsp` | Dynamic temporary DCL picker (`selhist_dlg`) displaying all 5 slots with live/original counts, source command, type summary, quick `1`–`5` buttons, and double-click recall. |

## Commands & Transparent Helpers

- `SH` / `SELHIST`: Opens `selhist_dlg` picker; restores previous pickfirst grips on Cancel.
- `SH1`–`SH5`: Immediately recalls slot `#1` (newest) through `#5` with active pickfirst grips (`sssetfirst`).
- `(sh 1)`–`(sh 5)` / `(sh1)`–`(sh5)`: Transparent mid-command helpers for any `Select objects:` prompt; returns a `<Selection set>` directly without calling `sssetfirst`.
- `SHDEBUG`: Interactive debug logger control `[On/Off/Path/Clear/Status] <Status>`.

## Reactor State Machine & Globals

- `*sh-max-slots*`: `5` (default).
- `*sh-max-entities*`: `5000` (selections larger than this are skipped with `SNAPSHOT_SKIPPED` to avoid `Ctrl+A` lag).
- `*sh-history*`: List of up to 5 snapshot alists `((handles . ...) (key . ...) (source . ...) (summary . ...) (count . ...))`, newest first. Stored by DXF handle (group 5) to avoid AutoLISP's 128-selection-set limit.
- `*sh-pending-pf*`: Buffers incremental pickfirst clicks until the user finishes (Esc/deselect or starts a command).
- `*sh-last-prev-key*`: Canonical key of the last seen `(ssget "_P")` set, preventing non-selecting commands (`ZOOM`, `PAN`, `LINE`, `QSAVE`) from duplicating stale `_P` sets. Seeded on initial load.
- `*sh-suppress-key*`: Canonical live key of a recalled slot; prevents recalling a slot (or running a command on a recalled slot) from re-pushing or reordering `*sh-history*`. Cleared if the user modifies the selection or clears grips.
- `sh-own-command-p`: Ignores `SH`, `SELHIST`, `SH1`–`SH5`, `SHDEBUG` in command-end tracking.

## Known CAD / AutoLISP Limitations

- Multi-prompt commands (`TRIM`, `EXTEND`) only expose the final selection set via `(ssget "_P")`.
- Single-pick sub-commands (`FILLET`, `CHAMFER`) do not update `_P`.
- History is per-drawing in memory and resets when the drawing closes.

## Debug Log Events (`SelectionHistory/logs/selectionhistory_YYYYMMDD.jsonl`)

- Reactor stream (`cmd: "REACTOR"`):
  - `MODULE_LOAD`: `root_dir`, `ok`
  - `REACTORS_INSTALLED`: `seeded_prev`, `history_len`, `max_slots`
  - `SNAPSHOT_SKIPPED`: `reason` (`"over_max_entities"`), `len`, `max`, `source`
  - `HISTORY_PUSH`: `source`, `count`, `summary`, `history_len_before`, `history_len_after`, `max_slots`
  - `HISTORY_PUSH_SKIP_DUP`: `source`, `count`, `history_len`
  - `REACTOR_PICKFIRST`: `ss_count`, `action` (`buffer`/`suppress_match`/`commit_pending`/`clear_suppress`/`skip_snapshot`), `cmdactive`, `history_len`
  - `REACTOR_CMD_START`: `cmd`, `had_pending`, `committed`, `history_len`
  - `REACTOR_CMD_END`: `cmd`, `prev_count`, `action` (`pushed`/`skip_suppressed`/`skip_duplicate`/`skip_snapshot`), `history_len`
  - `REACTOR_ERROR`: `callback`, `message`
- Explicit command stream (`cmd: "SH"` / `"SH1"`..`"SH5"` / `"(sh)"`):
  - `RUN_START`, `RUN_END`, `ERROR`
  - `DIALOG_CLOSE`, `DIALOG_ERROR`
  - `RECALL_SLOT`: `idx`, `for_command`, `outcome` (`ok`/`invalid_idx`/`empty_history`/`slot_empty`/`all_erased`), `source`, `orig_count`, `live_count`, `erased_count`, `history_len`, `pickfirst_sysvar`

## Invariants to Check When Debugging

- `0 <= history_len <= 5` (or `max_slots`) on every event.
- On `HISTORY_PUSH`: `history_len_after == min(max_slots, history_len_before + 1)` and `count > 0`.
- On `RECALL_SLOT` (`ok` / `all_erased`): `live_count + erased_count == orig_count`; on `ok`, `live_count > 0`; if `for_command == false` and `pickfirst_sysvar == 0`, flag environment warning.
- Zero `REACTOR_ERROR` or `DIALOG_ERROR` events.
- Every explicit command `RUN_START` has a matching `RUN_END` or `ERROR` with the same `run_id`.
