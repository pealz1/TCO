# RomazHub Overhaul — Design Spec
Date: 2026-03-20

## Overview

Five coordinated changes to `RomazHub.lua` and `PealLib/Library.lua`:
1. Fix server browser (servers never load)
2. Retheme all tool GUIs to match RomazHub/PealLib styling
3. Rebuild decal tool UI using the Library theme system
4. Add `CreateToolImagePreview` to PealLib
5. Full code cleanup of RomazHub.lua

---

## A. Server Browser Fix

**Root cause:** `fetchServers` calls `game:HttpGet(url)` without passing the cookie flag. The Roblox public server list API (`games.roblox.com/v1/games/{placeId}/servers/Public`) requires the `.ROBLOSECURITY` cookie to return data. Other HTTP calls in the file (thumbnails) correctly pass `true` as the second arg or use `requestFunc`.

**Fix:** Replace `game:HttpGet(url)` with `requestFunc({Url=url, Method="GET"})` in `fetchServers`. Parse `resp.Body` via `HttpService:JSONDecode`. Add error guard if `resp.StatusCode ~= 200`. Also pass `true` flag as fallback: `game:HttpGet(url, true)`.

**Acceptance:** Server cards populate when the Server Browser popout is opened.

---

## B. Tool GUI Theme Unification

All tool panels (Rotate, Linear Velocity, Rotational Velocity, Block Editor) already use `createToolPanel` which applies the Library theme. The **decal tool** does not — it has a raw `ScreenGui` (`sui`) with hardcoded gray colors, floating `TextBox`/`TextButton` elements, and no connection to the theme registry.

**Fix:** Delete `sui` and all its raw children. Replace with a `createToolPanel` call matching the other tools:
- Same `UDim2.new(0.5, 0, 0.03, 0)` position
- Same accent bar, dark background, title label
- `createToolInput` for the decal ID field
- `createToolButton` for the "Rotate 90°" button
- `CreateToolImagePreview` (new PealLib function) for the image preview

All tool panels should appear at the same screen position and have consistent sizing.

---

## C. Decal Tool UI Rebuild

The existing `sui` ScreenGui floats over the entire screen with:
- `setdecal`: gray TextBox at `(0.465, 0.05)` screen position
- `rotatebutton`: gray TextButton at `(0.535, 0.05)`
- `imageindicator`: ImageLabel for preview

Replacement: a single `createToolPanel({name="DecalToolGui", title="Decal Tool", size=UDim2.new(0,210,0,200)})` containing:
- `createToolInput` at yPos 0.18 — "Enter Decal ID"
- `createToolButton` at yPos 0.36 — "Rotate 90°"
- `Library:CreateToolImagePreview(frame, 0.52)` — 1:1 aspect ratio image area

The `memeifyid` variable and `updatememeifydisplays` function connect to the new input's `GetPropertyChangedSignal("Text")`. The rotate button increments `decalrotation` by 90.

---

## D. `CreateToolImagePreview` in PealLib

New function added to Library.lua after `CreateToolLabel`:

```lua
function Library:CreateToolImagePreview(parent, yPos)
    -- Returns { label: ImageLabel, SetImage(id), SetVisible(bool) }
end
```

**Visual:** Rounded ImageLabel, dark background matching `Library.BackgroundColor`, centered in panel at 88% width, 1:1 aspect ratio via `UIAspectRatioConstraint`. Falls back gracefully if image ID is invalid.

**API:**
- `preview:SetImage(id)` — sets `ImageLabel.Image = "rbxassetid://" .. id`
- `preview:SetVisible(bool)` — shows/hides the container

Added to `Library._ToolPanels`-style registry for theme updates.

---

## E. Code Cleanup (RomazHub.lua)

**Comments:** Remove all `-- comment` lines and `--[[ block comments ]]`.

**`local` removal:** Variables declared at file/function scope that are used globally (e.g. `local _relayParts`, `local SBControlGroup`) should drop `local`. Variables inside functions that are truly local keep `local`.

**Variable renames (long → short):**
- `decalrotations` → `drot`
- `decalrotations2` → `drot2`
- `idkwhattonamets` → `dsizes`
- `_relayParts` → `_rp`
- `_relayUrl` → `_ru`
- `AUTOSCRIPT_URL` → `AUTOURL`
- `requestFunc` → `req`

**Formatting:** Consistent 4-space indentation, group related code blocks, stack tool registrations together, remove blank line clusters (>2 consecutive blank lines → 1).

**Bug fixes observed:**
- `getServerThumbnails` builds a POST body but also calls a GET — the GET result (`body`) is assigned but never used (dead code). Remove it.
- `scriptConnections` guard pattern (`if X and scriptConnections then ... else X:Connect... end`) is duplicated and over-complicated — simplify to `table.insert(scriptConnections, X:Connect(...))`.

---

## Files Changed

| File | Change |
|------|--------|
| `RomazHub.lua` | Server browser fix, decal tool rebuild, code cleanup |
| `PealLib/Library.lua` | Add `CreateToolImagePreview` function |

## Out of Scope
- No changes to other tabs (ESP, Home, etc.)
- No changes to SaveManager/ThemeManager
- No feature additions beyond what's specified
