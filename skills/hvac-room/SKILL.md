---
name: hvac-room
description: Technical documentation and debugging reference for the ZWCAD HvacRoom AutoLISP tool (HVACROOM, HVACROOMEXPORT, RTDEBUG). Use when modifying, troubleshooting, or auditing HvacRoom behavior, CHW flow/pipe sizing calculations, ROOMTAG_DATA XDATA, or HvacRoom JSONL debug logs.
---

# HvacRoom (ZWCAD AutoLISP Tool)

Modal-dialog HVAC room area tagger (Xref-compatible) with scaling, bias, rounding, Chilled Water Flow (GPM), pipe diameter sizing (`EDRC-STD-H-PD-001`), layer selection, Excel-compatible CSV export, and structured JSONL debug logging.

## Module Architecture (`HvacRoom/`)

All symbols use the isolated `rt-` / `*rt-*` namespace so `HvacRoom` can be loaded alongside other tools without collisions.

| File | Responsibility |
| --- | --- |
| `HvacRoom/HvacRoom.lsp` | Entry point, root-directory resolver (`rt-resolve-root-dir`), module loader (`rt-load-modules`), tag placement helper (`rt-place-room-tag`), and commands `HVACROOM`, `HVACROOMEXPORT`, `RTDEBUG`. |
| `HvacRoom/src/rt_log.lsp` | Self-contained, `DIMZIN`-safe JSONL debug logger writing to `HvacRoom/logs/hvacroom_YYYYMMDD.jsonl` (fallback `TEMPPREFIX`). |
| `HvacRoom/src/rt_state.lsp` | Persistent session defaults (`*rt-*`) and case-insensitive per-room-type counters (`rt-get-count`, `rt-set-count`). |
| `HvacRoom/src/rt_math.lsp` | Strict numeric/uint validators (`rt-valid-number-p`, `rt-valid-uint-p`), floor/ceil/round (`rt-round-val`), area pipeline (`rt-calc-area`), CHW GPM (`rt-calc-chw`), `EDRC-STD-H-PD-001` pipe table (`*rt-pipe-table*`, `rt-calc-pipe-size`), and label formatting (`rt-format-label`). |
| `HvacRoom/src/rt_cad.lsp` | Local layer listing (excludes Xref `\|` layers), boundary detection (`rt-get-area-at` via `_.-boundary`, `rt-get-area-from-ent` via `vla-get-Area`), green boundary preview (`rt-show-preview`, `rt-clear-preview`), and `MTEXT` + `ROOMTAG_DATA` XDATA creation (`rt-create-room-mtext`). |
| `HvacRoom/src/rt_dialog.dcl` | Static modal dialog layout (`roomtag_dlg`). |
| `HvacRoom/src/rt_dialog.lsp` | Dialog initialization, preset/unit sync, live ASCII preview, input validation (`rt-dlg-validate-and-save`), and `rt-show-dialog` (returns `1`=Pick, `2`=Export, `0`=Cancel). |
| `HvacRoom/src/rt_export.lsp` | RFC-4180 CSV escaping, backward-compatible `ROOMTAG_DATA` XDATA parsing (`rt-parse-xdata`), natural room-name sorting (`Cafe#2` < `Cafe#10`), and Excel CSV export with `=SUM(...)` totals (`rt-do-export`). |

## Commands

- `HVACROOM`: Opens `roomtag_dlg`, then loops in Point-Pick (`getpoint` + `_.-boundary`) or Select (`entsel` + `vla-get-Area`) mode. Supports `[Select/Pick/Undo/Dialog] <Done>`.
- `HVACROOMEXPORT`: Directly runs `rt-do-export` without opening the dialog (prompts to select tags or press Enter for all `ROOMTAG_DATA` MTEXT entities in drawing).
- `RTDEBUG`: Interactive debug logger control `[On/Off/Path/Clear/Status] <Status>`.

## Persistent Session Globals (`*rt-*`)

- Room & text: `*rt-counts*` (alist `(UPPER_TYPE . lastNum)`), `*rt-rtype*` (`"Cafe"`), `*rt-num*` (`1`), `*rt-extra*` (`""`), `*rt-height*` (`TEXTSIZE` or `250.0`).
- Area math: `*rt-preset*` (`"mm"`/`"cm"`/`"m"`/`"custom"`), `*rt-factor*` (`1e-6`), `*rt-factor-str*` (`"1e-6"`), `*rt-bias*` (`0.0`), `*rt-round-mode*` (`"nearest"`/`"up"`/`"down"`), `*rt-prec*` (`2`, range `0..6`).
- CHW & pipe: `*rt-chw-en*` (`"0"`/`"1"`), `*rt-ton-factor*` (`15.0` m²/ton), `*rt-pipe-en*` (`"0"`/`"1"`), `*rt-pipe-unit*` (`"mm"`/`"inch"`), `*rt-pipe-min-idx*` (`0..20`).
- CAD output: `*rt-multiline*`, `*rt-keepbndry*`, `*rt-fast-bndry*` (`"0"`/`"1"`), `*rt-mtext-layer*`, `*rt-bndry-layer*` (`"<Current>"` or local layer name).

## Calculation Pipeline & Formulas

1. **Final Area (m²)** (`rt-calc-area`):
   - `scaled = rawArea * factor`
   - `biased = scaled + bias`
   - `areaM2 = round(biased, prec, roundMode)` using epsilon `1e-9` for `"up"` (`ceil`) and `"down"` (`floor`), half-away-from-zero for `"nearest"`.
2. **Chilled Water Flow (GPM)** (`rt-calc-chw`, when `*rt-chw-en* = "1"`):
   - `chwGpm = round((areaM2 / tonFactor) * 2.4, prec, "nearest")`
3. **Pipe Diameter Sizing** (`rt-calc-pipe-size`, when `*rt-chw-en* = "1"` and `*rt-pipe-en* = "1"`):
   - Looks up the smallest entry in `*rt-pipe-table*` (`EDRC-STD-H-PD-001`, 21 entries from `2.0 gpm -> 15mm / 1/2"` up to `16745.0 gpm -> 750mm / 30"`, overflow `">750mm"` / `">30\""`) where `chwGpm <= maxGpm`, clamped below by `pipe_min_idx`.

## XDATA Schema (`ROOMTAG_DATA`)

Attached to each created `MTEXT` under `-3`:
- `0`: `(1000 . roomName)` — e.g. `"Cafe#1"`
- `1`: `(1000 . rtype)` — e.g. `"Cafe"`
- `2`: `(1040 . areaM2)` — final rounded area (float)
- `3`: `(1070 . chwEnInt)` — `1` or `0` (v1.1+)
- `4`: `(1040 . chwGpm)` — CHW GPM (v1.1+)
- `5`: `(1040 . tonFactor)` — Area Ton Factor (v1.1+)
- `6`: `(1070 . pipeEnInt)` — `1` or `0` (v2+)
- `7`: `(1000 . pipeSize)` — e.g. `"20mm"`, `"1\""`, or `""` (v2+)

`rt-parse-xdata` gracefully reads legacy v1 (3-item) and v1.1 (6-item) tags.

## Debug Log Events (`HvacRoom/logs/hvacroom_YYYYMMDD.jsonl`)

- `MODULE_LOAD`: `root_dir`, `ok`
- `RUN_START` / `RUN_END` / `ERROR`: lifecycle envelope (`clayer`, `textsize`, `insunits`, `outcome`, `placed_count`, `undone_count`, `elapsed_ms`)
- `DIALOG_CLOSE`, `DIALOG_VALIDATION_FAIL`, `DIALOG_ERROR`
- `MODE_SWITCH`: `mode` (`"point"` / `"select"`)
- `BOUNDARY_DETECT`: `mode`, `success`, `raw_area`, `ent_handle`, `fast_bndry`
- `TAG_PLACED`: full calculation inputs + outputs (`raw_area`, `factor`, `bias`, `round_mode`, `prec`, `area_m2`, `chw_en`, `ton_factor`, `chw_gpm`, `pipe_en`, `pipe_unit`, `pipe_min_idx`, `pipe_size`, `num_before`, `num_after`, `mtext_handle`, `layer`, `label`)
- `TAG_UNDONE` / `UNDO_EMPTY`: `undone_handle`, `rtype`, `num_before`, `num_after`
- `EXPORT_COMPLETE` / `EXPORT_EMPTY` / `EXPORT_CANCELLED` / `EXPORT_ERROR`

## Invariants to Check When Debugging

- On `TAG_PLACED`: `mtext_handle` is non-null; `area_m2` matches `rt-calc-area`; when `chw_en` is `true`, `chw_gpm` matches `round((area_m2 / ton_factor) * 2.4, prec)` and `ton_factor > 0`; `num_after == num_before + 1`.
- On `TAG_UNDONE`: `num_after == num_before - 1` (when `num_before > 0`).
- On `EXPORT_COMPLETE`: `parsed_rows + skipped_rows == entity_count` and `total_area >= 0`.
- Every `RUN_START` has a matching `RUN_END` or `ERROR` with the same `run_id`.
