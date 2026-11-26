# CadCustomTools

Collection of custom ZWCAD AutoLISP tools organized by tool subfolder.

## Tools

| Tool | Entry File | Commands | Description |
| --- | --- | --- | --- |
| [HvacRoom](HvacRoom/README.md) | `HvacRoom/HvacRoom.lsp` | `HVACROOM`, `HVACROOMEXPORT` | Xref-compatible room area tagger with scaling, bias, rounding, Chilled Water Flow (GPM), pipe diameter sizing (`EDRC-STD-H-PD-001`), layer selection, and Excel CSV export |
| [FireSprinkler](FireSprinkler/README.md) | `FireSprinkler/FireSprinkler.lsp` | `FFSPRINKLER`, `FFEXPORT` | Xref-compatible room area tagger with NFPA 13 hazard presets (`Light`, `Ordinary`, `Extra`, `Custom`), rounded-up sprinkler count, layer selection, and Excel CSV export |
| [SmartFilter](SmartFilter/README.md) | `SmartFilter/SmartFilter.lsp` | `SF`, `SMARTFILTER` | Revit-style interactive selection filter with live object-type & per-layer checkboxes, counts, Check All / Check None / Invert, and pickfirst grip support |
| [SelectionHistory](SelectionHistory/README.md) | `SelectionHistory/SelectionHistory.lsp` | `SH`, `SELHIST`, `SH1`–`SH5`, `(sh 1)`–`(sh 5)` | Automatic 5-slot selection history tracking selections both outside and inside commands via reactors, with quick commands, DCL picker, and transparent mid-command recall |
| [XrefLock](XrefLock/README.md) | `XrefLock/XrefLock.lsp` | `XREFLOCK`, `XRL`, `XREFLOCKON`, `XREFLOCKOFF` | Revit-style "Select Links" toggle that automatically strips Xrefs from pickfirst selections and disables Xref hover preview while preserving OSNAP and plotting |

## Quick Setup in ZWCAD

1. Add the `CadCustomTools` folder (or an individual tool subfolder like `CadCustomTools/HvacRoom`) to ZWCAD's **Support File Search Path** (`OPTIONS` → **Files** → **Support File Search Path**).
2. Run `APPLOAD` and load the desired tool's entry `.lsp` file (e.g., `HvacRoom/HvacRoom.lsp`).
