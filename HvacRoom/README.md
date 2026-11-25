# CadCustomTools — HvacRoom (ZWCAD)

Modal-dialog HVAC room area tagger (Xref-compatible) with area scaling, bias, rounding, optional Chilled Water Flow (GPM) & pipe diameter sizing, layer selection, and Excel-compatible CSV export.

## Structure

- `HvacRoom.lsp` — Entry point, module loader, and `HVACROOM` / `HVACROOMEXPORT` commands
- `src/rt_state.lsp` — Persistent session defaults and per-room-type numbering
- `src/rt_math.lsp` — Number validation, area scaling/bias/rounding, CHW flow, pipe sizing table, and label formatting
- `src/rt_dialog.dcl` — Modal dialog layout
- `src/rt_dialog.lsp` — Dialog callbacks, live preview, and validation
- `src/rt_cad.lsp` — Local layer listing, boundary area detection, polyline preview, and `MTEXT` + `ROOMTAG_DATA` XDATA creation
- `src/rt_export.lsp` — Natural-sorted Excel-compatible CSV export with `=SUM(...)` totals

## Setup in ZWCAD

1. Add the repository folder (containing `HvacRoom.lsp`) to ZWCAD's **Support File Search Path** (`OPTIONS` → **Files** → **Support File Search Path**).
2. Run `APPLOAD` and load `HvacRoom.lsp` (optionally add it to your Startup Suite).
   - If you load `HvacRoom.lsp` without adding the folder to Support File Search Path, it will prompt once to locate `HvacRoom.lsp` so it can load `src/*`.

## Commands

- `HVACROOM` — Opens the modal dialog, lets you choose target layers for both the `MTEXT` and (optionally) a persistent **Area Polyline**, highlights each detected boundary in bright green as a quick preview, and places `MTEXT` tags with attached XDATA. Type `U` to undo the last placed tag if a boundary leaks, or `D` to return to the dialog.
- `HVACROOMEXPORT` — Directly exports selected (or all) room tags in the drawing to a naturally sorted `.csv` file ready for Excel.

## Calculation Pipeline

1. **Scaled Area**: `rawArea * scaleFactor` (use presets `mm2->m2`, `cm2->m2`, `m2->m2`, or enter any custom multiplier such as `1e-2` or `0.01`).
2. **Area Bias**: `scaledArea + bias` (positive, zero, or negative).
3. **Rounding**: `Nearest`, `Up` (ceiling), or `Down` (floor) to the configured decimal places.
4. **Chilled Water Flow (optional)**:
   ```
   CHW Flow (gpm) = (Final Area / Area Ton Factor) * 2.4
   ```
5. **Pipe Diameter Sizing (optional, when CHW is enabled)**:
   - Uses the **4 ft / 100 ft** friction-loss table for pipes ≤ 4" (100 mm) and **8 fps** velocity limit for pipes ≥ 5" (125 mm).
   - Enforces the user-selected **Min Dia** lower threshold (`15mm`..`750mm` / `1/2"`..`30"`).
   - Appends `| ϕ <size>` (e.g., `| ϕ 25mm` or `| ϕ 1"`) to the `MTEXT` label and exports a `Pipe Size` column in the CSV.
