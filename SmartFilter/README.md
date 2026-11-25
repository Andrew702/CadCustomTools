# CadCustomTools — SmartFilter (ZWCAD)

Revit-style smart selection filter for ZWCAD. Quickly narrow any selection set by object type — and optionally by layer under each object type — using a live checkbox dialog with counts, **Check All**, **Check None**, and **Invert** controls.

## Structure

- `SmartFilter.lsp` — Entry point, module loader, and `SF` / `SMARTFILTER` commands
- `src/sf_types.lsp` — DXF entity type classification (including Xref vs. Block Reference and 3D Polyline/Mesh detection) and alphabetical sorting
- `src/sf_collect.lsp` — Fast selection grouping (by object type and per-type layer), filtered selection set builder, and command-line summary formatter
- `src/sf_dialog.lsp` — Dynamic Revit-style checkbox DCL generator with an optional **Break down by Layer** tree view, parent/child checkbox sync, multi-column overflow support, and live selection counter

## Setup in ZWCAD

1. Add the repository folder (or `CadCustomTools/SmartFilter`) to ZWCAD's **Support File Search Path** (`OPTIONS` → **Files** → **Support File Search Path**).
2. Run `APPLOAD` and load `SmartFilter.lsp` (optionally add it to your Startup Suite).
   - If you load `SmartFilter.lsp` without adding the folder to Support File Search Path, it will prompt once to locate `SmartFilter.lsp` so it can load `src/*`.

## Commands

- `SF` (short alias) / `SMARTFILTER` — Filters the current pickfirst selection (or prompts you to select objects if none are pre-selected).

## Workflow

1. Select objects in your drawing using any method (window, crossing, pick) — or run `SF` first to be prompted.
2. Type `SF` and press **Enter**.
3. In the **Smart Selection Filter** dialog:
   - Check or uncheck any object type (each shows its exact count in the selection).
   - Toggle **Break down by Layer** (`Alt+L`) to expand indented layer checkboxes under each object type. Toggling a parent object type checks/unchecks all of its layers; toggling individual layers syncs the parent checkbox automatically. Your preference is remembered for the session.
   - Use **Check All**, **Check None**, or **Invert** (`Alt+C`, `Alt+N`, `Alt+I`) for rapid filtering.
   - Watch the live `"Selected: X of Y objects"` counter update as you toggle.
4. Click **OK** (or press **Enter**) to keep only the checked objects selected with active grips (`sssetfirst`), ready for `MOVE`, `COPY`, `DELETE`, `PROPERTIES`, etc.
5. Click **Cancel** (or press **Esc**) to abort and preserve your original pre-selection.

## Supported Object Types

Common CAD entities are grouped under clean, readable labels:
- `Line`, `Polyline`, `3D Polyline`, `Polygon Mesh`, `Polyface Mesh`
- `Circle`, `Arc`, `Ellipse`, `Spline`, `Point`, `Xline`, `Ray`, `Multiline`
- `Block Reference`, `Xref` (automatically separated from regular blocks)
- `Text`, `MText`, `Attribute Definition`, `Table`
- `Hatch`, `Dimension`, `Leader`, `Multileader`, `Wipeout`
- `2D Solid`, `3D Solid`, `3D Face`, `Region`, `Surface`, `Body`, `Helix`
- `Raster Image`, `PDF Underlay`, `DWF Underlay`, `DGN Underlay`, `Viewport`
- Any other/custom entity type is shown under its DXF type name so no selected object is ever omitted.
