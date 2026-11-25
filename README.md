# CadCustomTools

Collection of custom ZWCAD AutoLISP tools organized by tool subfolder.

## Tools

| Tool | Entry File | Commands | Description |
| --- | --- | --- | --- |
| [HvacRoom](HvacRoom/README.md) | `HvacRoom/HvacRoom.lsp` | `HVACROOM`, `HVACROOMEXPORT` | Xref-compatible room area tagger with scaling, bias, rounding, Chilled Water Flow (GPM), pipe diameter sizing (`EDRC-STD-H-PD-001`), layer selection, and Excel CSV export |
| [SmartFilter](SmartFilter/README.md) | `SmartFilter/SmartFilter.lsp` | `SF`, `SMARTFILTER` | Revit-style interactive selection filter with live object-type checkboxes, counts, Check All / Check None / Invert, and pickfirst grip support |

## Quick Setup in ZWCAD

1. Add the `CadCustomTools` folder (or an individual tool subfolder like `CadCustomTools/HvacRoom`) to ZWCAD's **Support File Search Path** (`OPTIONS` → **Files** → **Support File Search Path**).
2. Run `APPLOAD` and load the desired tool's entry `.lsp` file (e.g., `HvacRoom/HvacRoom.lsp`).
