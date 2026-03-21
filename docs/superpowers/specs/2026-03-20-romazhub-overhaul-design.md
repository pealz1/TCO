# RomazHub Overhaul — Design Spec (v2)
Date: 2026-03-20

## Overview

Five coordinated changes to `RomazHub.lua` and `PealLib/Library.lua`:
1. Fix server browser (servers never load)
2. Rebuild decal tool UI to match Library theme (all other tools already use it)
3. Add `CreateToolImagePreview` to PealLib for the 2D panel preview
4. Full code cleanup of RomazHub.lua
5. Commit PealLib to GitHub

---

## A. Server Browser Fix

**Root cause:** In executor environments, `game:HttpGet` runs through the executor's HTTP layer. The Roblox server list API (`games.roblox.com/v1/games/{placeId}/servers/Public`) requires the player's `.ROBLOSECURITY` cookie to return server data. `game:HttpGet(url)` without the `true` flag may not pass cookies in some executors. The script already defines `requestFunc = (syn and syn.request) or request or http_request or ...` for authenticated requests — `fetchServers` should use it.

**Fix:**
- Replace `game:HttpGet(url)` in `fetchServers` with `requestFunc({Url=url, Method="GET"})` + parse `resp.Body` via `HttpService:JSONDecode`
- Guard: if `requestFunc` returns `StatusCode ~= 200` or pcall fails, fall back to `game:HttpGet(url, true)` (note: `true` bypasses Roblox HTTP filtering, allowing external Roblox API calls in some executors)
- The entire dead `pcall` block in `getServerThumbnails` that does `body = HttpService:JSONDecode(game:HttpGet("https://thumbnails.roblox.com/v1/batch", true))` is deleted — this is a GET to a POST-only endpoint that always fails silently, and `body` is never read. The `requestBody`-based POST call that follows it is the real request and is kept.

**Acceptance:** Server cards populate when Server Browser popout is opened.

---

## B. Decal Tool UI Rebuild

**Current state:**
- `sui` — ScreenGui floating over full screen, hardcoded gray/black, contains: `setdecal` (TextBox), `rotatebutton` (TextButton), `imageindicator` (ImageLabel for 2D panel preview)
- `suui` — SurfaceGui attached to `fakememe` invisible Part, shows live decal preview projected onto the hovered block surface; contains `imageindicator2`
- `fakememe` — invisible Part in Workspace used as adornee for `suui`

**What changes:** `sui` and all its children (`setdecal`, `rotatebutton`, `imageindicator`, aspect constraints) are deleted. Replaced with a `createToolPanel` matching all other tool panels.

**What stays:** `fakememe`, `suui`, and `imageindicator2` are preserved unchanged — they handle the live surface projection on hover, which is core gameplay.

**What is deleted with `sui`:** `setdecal`, `rotatebutton`, `imageindicator` (2D panel preview), and all three `UIAspectRatioConstraint` instances are deleted with `sui`. `imageindicator` no longer exists after this.

**`updatememeifydisplays` update:** Currently references both `imageindicator.Image` and `imageindicator2.Image`. After the rebuild, it is updated to use `_decPreview:SetImage(memeifyid)` for the panel preview and continues to update `imageindicator2.Image` directly for the surface preview.

**Replacement panel:**
```
createToolPanel({name="DecalToolGui", title="Decal Tool", size=UDim2.new(0,210,0,185)})
  createToolInput   yPos=0.18  placeholder="Enter Decal ID"
  createToolButton  yPos=0.36  text="Rotate 90°"  xPos=0.06  width=0.88
  CreateToolImagePreview  yPos=0.52   (2D panel preview)
```

**Equip/unequip wiring:** In `createdecaltool()`, lines that set `sui.Enabled = true/false` are replaced with `_decPanel.gui.Enabled = true/false` (where `_decPanel` is the panel table returned by `createToolPanel`). `suui.Enabled` wiring is unchanged.

**Connections:** The guard pattern (`if X and scriptConnections then ... else X:Connect end`) is simplified to `table.insert(scriptConnections, X:Connect(...))` everywhere — `scriptConnections` always exists by the time this code runs.

---

## C. `CreateToolImagePreview` in PealLib

New function added to `Library.lua` immediately after the `CreateToolLabel` function body (before the `task.defer` loop). Inserted at file scope, not inside any other function.

```lua
function Library:CreateToolImagePreview(parent, yPos)
    -- outer container
    local container = Library:Create('Frame', {
        AnchorPoint = Vector2.new(0.5, 0);
        BackgroundColor3 = Library.BackgroundColor;
        BorderSizePixel = 0;
        Position = UDim2.new(0.5, 0, yPos, 0);
        Size = UDim2.new(0.88, 0, 0, 0);   -- height driven by aspect ratio
        ZIndex = 5;
        Parent = parent;
    })
    Library:Create('UIAspectRatioConstraint', { AspectRatio = 1; Parent = container })
    Library:Create('UICorner', { CornerRadius = UDim.new(0, 4); Parent = container })
    Library:AddToRegistry(container, { BackgroundColor3 = 'BackgroundColor' })

    local img = Library:Create('ImageLabel', {
        BackgroundTransparency = 1;
        Size = UDim2.new(1, 0, 1, 0);
        ScaleType = Enum.ScaleType.Fit;
        Image = '';
        ZIndex = 6;
        Parent = container;
    })

    local preview = {}
    function preview:SetImage(id)
        img.Image = id and id ~= '' and ('rbxassetid://' .. tostring(id)) or ''
    end
    function preview:SetVisible(vis)
        container.Visible = vis
    end
    preview.container = container
    preview.label = img
    return preview  -- returns table: { container, label, SetImage(id), SetVisible(bool) }
end
```

**Theme:** `container.BackgroundColor3` is registered to `'BackgroundColor'` so it updates with theme changes. The ImageLabel itself has no background so no registration needed.

**No separate `_ToolPanels` entry needed** — the container is a child of the already-registered panel inner frame, so it inherits theme updates through the frame hierarchy.

---

## D. Animations

The user requested animations. The Library's `createToolButton` already applies hover tweens (0.12s, `AccentColor` border on hover). The `createToolPanel` accent bar and dark theme are static but consistent. No additional animation logic is required — the existing Library animations cover all interactive elements. This is the same behavior as all other tool panels.

---

## E. Code Cleanup (RomazHub.lua)

**Comments:** Remove all `-- single line` and `--[[ block ]]` comments.

**`local` removal — specific variables only (verified against actual usage):**
The following are module-scope globals already used without `local` in many places, but some are declared with `local`. Drop `local` only where the variable is genuinely used across scopes:
- `SBControlGroup` (line 688) — used only in its own block, keep `local`
- `_relayParts` (line 146) — consumed immediately by `table.concat`, never reused → keep `local` (removing it is pointless noise)
- `_relayUrl` (line 153) — used only locally → keep `local`
- General rule: only remove `local` where a variable needs to be accessible from a different scope and isn't already global

**Variable renames (long → shorter, all references updated):**
- `decalrotations` → `drot`
- `decalrotations2` → `drot2`
- `idkwhattonamets` → `dsizes`; local temp `idkwhattonamets2` → `dsizes2` (appears inside `createdecaltool` and `Heartbeat` handler)
- `memeifyid` → `mid` (only if not conflicting with local `mid` in `createdecaltool`)
- `AUTOSCRIPT_URL` → `AUTOURL`
- Do NOT rename `requestFunc` — it is the same global defined at line 139, referenced globally; no new definition introduced in the server browser fix
- Do NOT rename `decalrotation` (the integer counter) — already short and would be confused with renamed `drot`/`drot2`

**Formatting:**
- Consistent 4-space indentation throughout
- Group related code blocks with single blank line separators
- Collapse 3+ consecutive blank lines to 1
- Stack tool panel declarations together

**Bug fixes:**
1. Dead GET call in `getServerThumbnails` removed (entire pcall wrapping unused `body` assignment)
2. `scriptConnections` guard duplication removed — always use `table.insert(scriptConnections, X:Connect(...))`
3. `suui.Enabled` toggle on unequip uses `.connect` (lowercase) at line 7059 — fix to `.Connect`

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
- `suui`/`fakememe`/`imageindicator2` surface preview system preserved unchanged
