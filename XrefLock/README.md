# CadCustomTools — XrefLock (ZWCAD)

Revit-style **Select Links** toggle for ZWCAD. Automatically prevents external references (Xrefs) from being selected via click or crossing window and disables Xref hover highlighting, while keeping **OSNAP** and **plotting** fully intact.

## Structure

- `XrefLock.lsp` — Entry point, module loader, auto-start reactor setup, and commands (`XREFLOCK`, `XRL`, `XREFLOCKON`, `XREFLOCKOFF`)
- `src/xlk_core.lsp` — Xref detection (`INSERT` block table flag bit 4), pickfirst selection filtering with re-entrancy guard, and `PREVIEWFILTER` bit 2 synchronization
- `src/xlk_reactor.lsp` — Reload-safe Visual LISP reactor (`vlr-miscellaneous-reactor` on `:vlr-pickfirstModified`)

## Setup in ZWCAD

1. Add the repository folder (or `CadCustomTools/XrefLock`) to ZWCAD's **Support File Search Path** (`OPTIONS` → **Files** → **Support File Search Path**).
2. Run `APPLOAD` and load `XrefLock.lsp` (recommended: add it to your **Startup Suite** so Xref protection starts automatically in every drawing).
   - If you load `XrefLock.lsp` without adding the folder to Support File Search Path, it will prompt once to locate `XrefLock.lsp` so it can load `src/*`.

## Commands

| Command | Description |
| --- | --- |
| `XREFLOCK` / `XRL` | Toggles Xref selection lock between **LOCKED** (unselectable) and **UNLOCKED** (selectable). |
| `XREFLOCKON` | Explicitly enables Xref lock (useful for toolbars, ribbons, or scripts). |
| `XREFLOCKOFF` | Explicitly disables Xref lock so Xrefs can be selected normally. |

## Behavior & Notes

- **Enabled by default on load:** Loading `XrefLock.lsp` installs the background reactor and starts in **LOCKED** mode immediately.
- **Instant pickfirst filtering:** Clicking an Xref or dragging a crossing window over it automatically drops all Attached and Overlay Xrefs from the selection while keeping your own drawing geometry selected.
- **Automatic hover-preview suppression (`PREVIEWFILTER`):** When locked, XrefLock ensures bit `2` (*Exclude objects in xrefs*) is set on `PREVIEWFILTER` so hovering over Xref geometry does not highlight or lag your cursor. When unlocked, it restores bit `2` only if XrefLock turned it on.
- **OSNAP & plotting stay intact:** Unlike non-plotting `DEFPOINTS` workarounds, Xrefs remain on their normal plotting layers and remain fully snappable.
- **Recommended companion settings (optional):**
  - Lock the Xref's layer (e.g., `00-XREF`) so mid-command edits (`MOVE`, `ERASE`, `ROTATE`) also ignore it.
  - Set `SELECTIONCYCLING` to `0` (`Ctrl+W`) to avoid overlap popups when drawing over Xref lines.
  - Set `XDWGFADECTL` (e.g., `60`–`75`) to visually fade Xrefs into the background.
- **Reload-safe:** Re-running `APPLOAD` on `XrefLock.lsp` cleanly replaces the existing tagged reactor without duplicating callbacks.

## Known CAD / AutoLISP Limitations

- **Mid-command `Select objects:` prompts:** AutoLISP's `:vlr-pickfirstModified` reactor operates on outside-command (pickfirst / grip) selections. If you start a command first (verb-noun) and drag a crossing window at a `Select objects:` prompt, CAD may still highlight the Xref inside that prompt; placing the Xref on a **locked layer** prevents any modification commands from affecting it.
