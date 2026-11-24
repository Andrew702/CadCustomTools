# CadCustomTools — RoomTag (ZWCAD)

Modal-dialog room area tagger (Xref-compatible) with area scaling, bias, rounding, optional Chilled Water Flow (GPM) calculation, and Excel-compatible CSV export.

## Structure

- `RoomTag.lsp` — Entry point, module loader, and `ROOMTAG` / `ROOMEXPORT` commands
- `src/rt_state.lsp` — Persistent session defaults and per-room-type numbering
- `src/rt_math.lsp` — Number validation, area scaling/bias/rounding, CHW flow calculation, and label formatting
- `src/rt_dialog.dcl` — Modal dialog layout
- `src/rt_dialog.lsp` — Dialog callbacks, live preview, and validation
- `src/rt_cad.lsp` — Boundary area detection and `MTEXT` + `ROOMTAG_DATA` XDATA creation
- `src/rt_export.lsp` — Excel-compatible CSV export with `=SUM(...)` totals

## Setup in ZWCAD

1. Add the repository folder (containing `RoomTag.lsp`) to ZWCAD's **Support File Search Path** (`OPTIONS` → **Files** → **Support File Search Path**).
2. Run `APPLOAD` and load `RoomTag.lsp` (optionally add it to your Startup Suite).
   - If you load `RoomTag.lsp` without adding the folder to Support File Search Path, it will prompt once to locate `RoomTag.lsp` so it can load `src/*`.

## Commands

- `ROOMTAG` — Opens the modal dialog, lets you pick rooms sequentially (type `D` to return to the dialog at any time), and places `MTEXT` tags with attached XDATA.
- `ROOMEXPORT` — Directly exports selected (or all) room tags in the drawing to a `.csv` file ready for Excel.

## Calculation Pipeline

1. **Scaled Area**: `rawArea * scaleFactor` (use presets `mm2->m2`, `cm2->m2`, `m2->m2`, or enter any custom multiplier such as `1e-2` or `0.01`).
2. **Area Bias**: `scaledArea + bias` (positive, zero, or negative).
3. **Rounding**: `Nearest`, `Up` (ceiling), or `Down` (floor) to the configured decimal places.
4. **Chilled Water Flow (optional)**:
   ```
   CHW Flow (gpm) = (Final Area / Area Ton Factor) * 2.4
   ```
   When enabled, `CHW: <value> gpm` is appended to the `MTEXT` label and exported in the `CHW Flow (gpm)` CSV column with a `TOTAL` sum row.
