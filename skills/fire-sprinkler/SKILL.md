---
name: fire-sprinkler
description: Technical documentation and debugging reference for the ZWCAD FireSprinkler AutoLISP tool (FFSPRINKLER, FFAPPEND, FFEXPORT, FFDEBUG). Use when modifying, troubleshooting, or auditing NFPA 13 sprinkler calculations, FFSPRINKLER_DATA XDATA, ROOMTAG_DATA cross-compatibility, or FireSprinkler JSONL debug logs.
---

# FireSprinkler (ZWCAD AutoLISP Tool — NFPA 13)

Modal-dialog room area tagger (Xref-compatible) that calculates required fire-fighting sprinklers per room using NFPA 13 hazard coverage presets, supports appending/updating sprinkler counts on existing `HvacRoom`/`MTEXT` area tags, exports Excel-compatible CSV schedules, and writes structured JSONL debug logs.

## Module Architecture (`FireSprinkler/`)

All symbols use the isolated `ff-` / `*ff-*` namespace so `FireSprinkler` coexists cleanly with `HvacRoom` and other tools.

| File | Responsibility |
| --- | --- |
| `FireSprinkler/FireSprinkler.lsp` | Entry point, root-directory resolver (`ff-resolve-root-dir`), module loader (`ff-load-modules`), tag placement helper (`ff-place-room-tag`), and commands `FFSPRINKLER`, `FFAPPEND`, `FFEXPORT`, `FFDEBUG`. |
| `FireSprinkler/src/ff_log.lsp` | Self-contained, `DIMZIN`-safe JSONL debug logger writing to `FireSprinkler/logs/firesprinkler_YYYYMMDD.jsonl` (fallback `TEMPPREFIX`). |
| `FireSprinkler/src/ff_state.lsp` | Persistent session defaults (`*ff-*`) and case-insensitive per-room-type counters (`ff-get-count`, `ff-set-count`). |
| `FireSprinkler/src/ff_math.lsp` | Strict validators (`ff-valid-number-p`, `ff-valid-uint-p`), rounding (`ff-round-val`), area pipeline (`ff-calc-area`), NFPA 13 ceiling sprinkler count (`ff-calc-sprinklers`), label formatter (`ff-format-label`), text-fallback area parser (`ff-extract-area-from-text`), and in-place text updater (`ff-append-or-replace-sprinklers`). |
| `FireSprinkler/src/ff_cad.lsp` | Local layer listing, boundary detection (`ff-get-area-at`, `ff-get-area-from-ent`), green preview (`ff-show-preview`, `ff-clear-preview`), `MTEXT` + `FFSPRINKLER_DATA` creation (`ff-create-room-mtext`), cross-tool tag reader (`ff-read-tag-area-info`), non-destructive XDATA attachment (`ff-attach-xdata`), and batch append (`ff-append-sprinklers-to-selection`). |
| `FireSprinkler/src/ff_dialog.dcl` | Static modal dialog layout (`ffsprinkler_dlg`). |
| `FireSprinkler/src/ff_dialog.lsp` | Dialog initialization, hazard preset sync, live preview, validation (`ff-dlg-validate-and-save`), and `ff-show-dialog` (returns `1`=Pick, `2`=Export, `3`=Append, `0`=Cancel). |
| `FireSprinkler/src/ff_export.lsp` | RFC-4180 CSV escaping, `FFSPRINKLER_DATA` XDATA parsing (`ff-parse-xdata`), natural room-name sorting, and Excel CSV export with `=SUM(...)` totals (`ff-do-export`). |

## Commands

- `FFSPRINKLER`: Opens `ffsprinkler_dlg`, then loops in Point-Pick or Select mode (`[Select/Pick/Undo/Dialog] <Done>`), or branches to Export / Append.
- `FFAPPEND`: Directly selects existing `MTEXT` area tags and appends (or replaces in-place) `Sprinklers: <N>` while attaching `FFSPRINKLER_DATA` XDATA without erasing existing `ROOMTAG_DATA`.
- `FFEXPORT`: Directly exports selected (or all) `FFSPRINKLER_DATA` tags to `FireSprinklers.csv`.
- `FFDEBUG`: Interactive debug logger control `[On/Off/Path/Clear/Status] <Status>`.

## NFPA 13 Coverage Presets & Formula

| Hazard Class | `*ff-hazard*` | Coverage Area (`*ff-cov-area*`) |
| --- | --- | --- |
| Light Hazard | `"Light"` | `18.6 m²` (200 ft²) |
| Ordinary Hazard | `"Ordinary"` | `12.1 m²` (130 ft²) |
| Extra Hazard | `"Extra"` | `9.3 m²` (100 ft²) |
| Custom | `"Custom"` | Any user-specified float `> 0` |

1. **Final Area (m²)** (`ff-calc-area`): `round(rawArea * factor + bias, prec, roundMode)`.
2. **Sprinkler Count** (`ff-calc-sprinklers`):
   - If `areaM2 <= 0` or `covArea <= 0` → returns `0`.
   - Otherwise → `max(1, ceil((areaM2 / covArea) - 1e-9))` (always integer ≥ 1, rounded UP).

## XDATA Schema (`FFSPRINKLER_DATA`) & Cross-Tool Compatibility

Attached under `-3`:
- `0`: `(1000 . roomName)` — e.g. `"Office#1"`
- `1`: `(1000 . rtype)` — e.g. `"Office"`
- `2`: `(1040 . areaM2)` — final area in m² (float)
- `3`: `(1000 . hazardLabel)` — `"Light"`, `"Ordinary"`, `"Extra"`, or `"Custom"`
- `4`: `(1040 . covArea)` — coverage area per sprinkler in m² (float)
- `5`: `(1070 . sprCount)` — integer sprinkler count

In `FFAPPEND`, `ff-read-tag-area-info` resolves area in priority order:
1. `FFSPRINKLER_DATA` XDATA (`source = "FFSPRINKLER_DATA"`)
2. `ROOMTAG_DATA` XDATA from `HvacRoom` (`source = "ROOMTAG_DATA"`)
3. Text fallback parsing `"Area: <num>"` (`source = "TEXT_FALLBACK"`)

`ff-attach-xdata` preserves all non-`FFSPRINKLER_DATA` XDATA apps on the entity.

## Debug Log Events (`FireSprinkler/logs/firesprinkler_YYYYMMDD.jsonl`)

- `MODULE_LOAD`, `RUN_START`, `RUN_END`, `ERROR`
- `DIALOG_CLOSE`, `DIALOG_VALIDATION_FAIL`, `DIALOG_ERROR`
- `MODE_SWITCH`, `BOUNDARY_DETECT`
- `TAG_PLACED`: `room_name`, `rtype`, `num_before`, `num_after`, `raw_area`, `factor`, `bias`, `round_mode`, `prec`, `area_m2`, `hazard`, `cov_area`, `sprinklers`, `layer`, `mtext_handle`, `label`
- `TAG_UNDONE`, `UNDO_EMPTY`
- `APPEND_ITEM`: `handle`, `room_name`, `source`, `area_m2`, `cov_area`, `sprinklers`, `updated`
- `APPEND_COMPLETE`: `selected_count`, `updated`, `skipped`, `hazard`, `cov_area`
- `EXPORT_COMPLETE` / `EXPORT_EMPTY` / `EXPORT_CANCELLED` / `EXPORT_ERROR`

## Invariants to Check When Debugging

- On `TAG_PLACED`: `mtext_handle` is non-null; `area_m2` matches `ff-calc-area`; `sprinklers == max(1, ceil(area_m2 / cov_area - 1e-9))` for `area_m2 > 0`; `num_after == num_before + 1`.
- On `APPEND_ITEM` with `updated == true`: `sprinklers` matches `ff-calc-sprinklers(area_m2, cov_area)`.
- On `APPEND_COMPLETE`: `updated + skipped == selected_count`.
- On `EXPORT_COMPLETE`: `parsed_rows + skipped_rows == entity_count` and `total_sprinklers >= parsed_rows` (if all areas > 0).
- Every `RUN_START` has a matching `RUN_END` or `ERROR` with the same `run_id`.
