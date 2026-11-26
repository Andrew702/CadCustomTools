---
name: smart-filter
description: Technical documentation and debugging reference for the ZWCAD SmartFilter AutoLISP tool (SF, SMARTFILTER, SFDEBUG). Use when modifying, troubleshooting, or auditing Revit-style object-type and layer selection filtering, dynamic DCL generation, or SmartFilter JSONL debug logs.
---

# SmartFilter (ZWCAD AutoLISP Tool)

Revit-style interactive selection filter for ZWCAD. Narrows a pickfirst (`_I`) or prompted selection set by DXF object type — and optionally by layer under each object type — using a dynamically generated multi-column DCL checkbox dialog, preserving grips via `sssetfirst`, and writing structured JSONL debug logs.

## Module Architecture (`SmartFilter/`)

All symbols use the isolated `sf-` / `*sf-*` namespace.

| File | Responsibility |
| --- | --- |
| `SmartFilter/SmartFilter.lsp` | Entry point, root-directory resolver (`sf-resolve-root-dir`), module loader (`sf-load-modules`), core runner (`sf-run`), and commands `SF`, `SMARTFILTER`, `SFDEBUG`. |
| `SmartFilter/src/sf_log.lsp` | Self-contained, `DIMZIN`-safe JSONL debug logger writing to `SmartFilter/logs/smartfilter_YYYYMMDD.jsonl` (fallback `TEMPPREFIX`). |
| `SmartFilter/src/sf_types.lsp` | DXF entity classification (`sf-entity-type-label`), cached Xref vs Block Reference detection (`sf-is-xref-block` checking `tblsearch "BLOCK"` group 70 bit 4), 3D Polyline / Polygon Mesh / Polyface Mesh classification via `POLYLINE` group 70 bits `8`/`16`/`64`, and alphabetical sorting (`sf-sort-groups`). |
| `SmartFilter/src/sf_collect.lsp` | O(N) grouping by object type and per-type layer (`sf-group-selection`), filtered selection set builder (`sf-build-selection`), and command-line summary formatter (`sf-format-summary`). |
| `SmartFilter/src/sf_dialog.lsp` | Dynamic temporary `.dcl` generator (`sf-write-temp-dcl`), DCL string escaping (`sf-dcl-escape`, doubling `&` → `&&`), balanced multi-column layout (`maxPerCol` = 14 compact / 17 by-layer), parent/child checkbox synchronization, Check All / Check None / Invert actions, and modal loop (`sf-show-dialog`, return code `2` rebuilds dialog when toggling layer mode). |

## Commands

- `SF` / `SMARTFILTER`: Checks `(ssget "_I")` first; if empty, prompts `(ssget)`. Opens dynamic checkbox dialog, applies filtered selection via `(sssetfirst nil filteredSS)` on OK, or restores original pickfirst selection on Cancel.
- `SFDEBUG`: Interactive debug logger control `[On/Off/Path/Clear/Status] <Status>`.

## Core Data Structures

- **Grouped Selection** (`sf-group-selection` output): `(sortedGroups totalCount)`
  - Each group: `(typeLabel typeCount layerGroups)`
  - Each layer group: `(layerName layerCount entList)` (missing/empty DXF group 8 defaults to `"0"`)
- **Checked Flags** (`*sf-dlg-layer-flags*` and `sf-show-dialog` return value):
  - Parallel list of lists of `"1"` / `"0"` strings aligned 1:1 with `groups` and each group's `layerGroups`.
- **Session State**:
  - `*sf-by-layer*`: `"0"` (Object Types only) or `"1"` (Break down by Layer), remembered across runs.
  - `*sf-temp-dcl*`: Tracks temporary `.dcl` path; always deleted by `sf-cleanup-temp-dcl` on normal close and in `*error*`.

## Debug Log Events (`SmartFilter/logs/smartfilter_YYYYMMDD.jsonl`)

- `MODULE_LOAD`: `root_dir`, `ok`
- `RUN_START` / `RUN_END` / `ERROR`: lifecycle envelope (`pickfirst`, `by_layer`, `outcome`, `total_count`, `selected_count`, `elapsed_ms`)
- `SELECTION_INPUT`: `source` (`"pickfirst"` / `"prompt"`), `total_count`
- `GROUPS_BUILT`: `total_count`, `sum_group_counts`, `sum_layer_counts`, `type_count`, `layer_bucket_count`
- `DIALOG_MODE_TOGGLE`: `by_layer`
- `DIALOG_CLOSE`: `rc` (`1`=ok, `0`=cancel), `outcome`, `by_layer`
- `DIALOG_ERROR`: `reason` (`temp_dcl_write_failed` / `load_dialog_failed` / `new_dialog_failed`), `path`
- `FILTER_APPLIED`: `total_count`, `selected_count`, `by_layer`, `pickfirst_sysvar`, `summary`
- `FILTER_CANCELLED`: `restored_pickfirst`, `original_count`

## Invariants to Check When Debugging

- On `GROUPS_BUILT`: `sum_group_counts == total_count` and `sum_layer_counts == total_count` (a mismatch indicates entities with `nil` `entget` or broken bucketing).
- On `FILTER_APPLIED`: `0 <= selected_count <= total_count`; if `selected_count > 0` and `pickfirst_sysvar == 0`, grips will not stay active (user environment warning).
- No `DIALOG_ERROR` events (indicates temp directory permission failure or malformed DCL escaping).
- Every `RUN_START` has a matching `RUN_END` or `ERROR` with the same `run_id`.
