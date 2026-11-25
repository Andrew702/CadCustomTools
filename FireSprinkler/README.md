# FireSprinkler (ZWCAD — NFPA 13)

Modal-dialog room area tagger (Xref-compatible) that calculates required fire-fighting sprinklers per room based on NFPA 13 hazard classes, with boundary polyline preview, layer selection, and Excel-compatible CSV export.

## Structure

- `FireSprinkler.lsp` — Entry point, module loader, and `FFSPRINKLER` / `FFEXPORT` commands
- `src/ff_state.lsp` — Persistent session defaults and per-room-type numbering
- `src/ff_math.lsp` — Number validation, area scaling/bias/rounding, NFPA 13 sprinkler count (`ceil`), and label formatting
- `src/ff_dialog.dcl` — Modal dialog layout
- `src/ff_dialog.lsp` — Dialog callbacks, hazard preset sync, layer dropdowns, live preview, and validation
- `src/ff_cad.lsp` — Local layer listing, boundary detection, polyline preview, and `MTEXT` + `FFSPRINKLER_DATA` XDATA creation
- `src/ff_export.lsp` — Natural-sorted Excel-compatible CSV export with `=SUM(...)` totals

## Setup in ZWCAD

1. Add the `CadCustomTools` folder (or `CadCustomTools/FireSprinkler`) to ZWCAD's **Support File Search Path** (`OPTIONS` → **Files** → **Support File Search Path**).
2. Run `APPLOAD` and load `FireSprinkler/FireSprinkler.lsp`.

## Commands

- `FFSPRINKLER` — Opens the modal dialog, highlights each detected room boundary in bright green as a quick preview, and places an `MTEXT` tag with room area and required sprinkler count (`Sprinklers: <N>`). Type `U` to undo the last tag, or `D` to return to the dialog.
- `FFEXPORT` — Exports selected (or all) `FFSPRINKLER` tags to a naturally sorted `.csv` file (`Room Name, Room Type, Area (m2), Hazard, Sprinklers` + `TOTAL` sums).

## NFPA 13 Coverage Presets

| Hazard Class | Area per Sprinkler |
| --- | --- |
| Light Hazard | `18.6 m²` (200 ft²) |
| Ordinary Hazard | `12.1 m²` (130 ft²) |
| Extra Hazard | `9.3 m²` (100 ft²) |
| Custom | Any user-specified value in `m²` |

```
Sprinklers = ceil(Final Room Area / Area per Sprinkler)   [integer, rounded UP]
```
