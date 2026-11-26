# CadCustomTools

Collection of custom ZWCAD AutoLISP tools organized by tool subfolder.

## Tools

| Tool | Entry File | Commands | Description |
| --- | --- | --- | --- |
| [HvacRoom](HvacRoom/README.md) | `HvacRoom/HvacRoom.lsp` | `HVACROOM`, `HVACROOMEXPORT`, `RTDEBUG` | Xref-compatible room area tagger with scaling, bias, rounding, Chilled Water Flow (GPM), pipe diameter sizing (`EDRC-STD-H-PD-001`), layer selection, and Excel CSV export |
| [FireSprinkler](FireSprinkler/README.md) | `FireSprinkler/FireSprinkler.lsp` | `FFSPRINKLER`, `FFAPPEND`, `FFEXPORT`, `FFDEBUG` | Xref-compatible room area tagger with NFPA 13 hazard presets (`Light`, `Ordinary`, `Extra`, `Custom`), rounded-up sprinkler count, batch append, layer selection, and Excel CSV export |
| [SmartFilter](SmartFilter/README.md) | `SmartFilter/SmartFilter.lsp` | `SF`, `SMARTFILTER`, `SFDEBUG` | Revit-style interactive selection filter with live object-type & per-layer checkboxes, counts, Check All / Check None / Invert, and pickfirst grip support |
| [SelectionHistory](SelectionHistory/README.md) | `SelectionHistory/SelectionHistory.lsp` | `SH`, `SELHIST`, `SH1`–`SH5`, `(sh 1)`–`(sh 5)`, `SHDEBUG` | Automatic 5-slot selection history tracking selections both outside and inside commands via reactors, with quick commands, DCL picker, and transparent mid-command recall |
| [XrefLock](XrefLock/README.md) | `XrefLock/XrefLock.lsp` | `XREFLOCK`, `XRL`, `XREFLOCKON`, `XREFLOCKOFF` | Revit-style "Select Links" toggle that automatically strips Xrefs from pickfirst selections and disables Xref hover preview while preserving OSNAP and plotting |

## Debug Logging

Every tool includes a self-contained JSONL debug logger (enabled by default) that records each run to `<Tool>/logs/<tool>_YYYYMMDD.jsonl` (gitignored, with automatic fallback to `TEMPPREFIX` if the folder is read-only):

- `RTDEBUG` (`HvacRoom`), `FFDEBUG` (`FireSprinkler`), `SFDEBUG` (`SmartFilter`), `SHDEBUG` (`SelectionHistory`) — supports `[On/Off/Path/Clear/Status]`.

## AI Agent Skills (`skills/`)

Structured `SKILL.md` documentation for AI agents is stored under `skills/`:

- [hvac-room](skills/hvac-room/SKILL.md)
- [fire-sprinkler](skills/fire-sprinkler/SKILL.md)
- [smart-filter](skills/smart-filter/SKILL.md)
- [selection-history](skills/selection-history/SKILL.md)
- [cad-debug-analyzer](skills/cad-debug-analyzer/SKILL.md) — audit rules and invariants for checking `<Tool>/logs/*.jsonl` files for errors or illogical behavior

## Quick Setup in ZWCAD

1. Add the `CadCustomTools` folder (or an individual tool subfolder like `CadCustomTools/HvacRoom`) to ZWCAD's **Support File Search Path** (`OPTIONS` → **Files** → **Support File Search Path**).
2. Run `APPLOAD` and load the desired tool's entry `.lsp` file (e.g., `HvacRoom/HvacRoom.lsp`).
