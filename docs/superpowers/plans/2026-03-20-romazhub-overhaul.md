# RomazHub Overhaul Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix server browser, rebuild decal tool UI to match Library theme, add `CreateToolImagePreview` to PealLib, and clean up all comments/formatting/variable names in RomazHub.lua.

**Architecture:** Two files change — `Library.lua` on GitHub (PealLib repo) gets one new function; `RomazHub.lua` locally gets a server browser fix, a full decal tool UI replacement, and a global cleanup pass. Changes are independent and ordered so PealLib is updated first, then RomazHub uses it.

**Tech Stack:** Lua 5.1 (Roblox), PealLib UI library, GitHub CLI (`gh`)

**Spec:** `docs/superpowers/specs/2026-03-20-romazhub-overhaul-design.md`

---

## File Map

| File | Action | Responsibility |
|------|--------|----------------|
| `RomazHub.lua` | Modify | Server browser fix, decal tool rebuild, cleanup |
| `PealLib/Library.lua` (GitHub) | Modify | Add `CreateToolImagePreview` function |

---

## Task 1: Add `CreateToolImagePreview` to PealLib

**Files:**
- Modify: `PealLib/Library.lua` — clone repo locally, add function after `CreateToolLabel`, push

This is done first so RomazHub can reference it.

- [ ] **Step 1.1: Clone PealLib locally**

```bash
git clone https://github.com/pealz1/PealLib.git /tmp/PealLib
```

- [ ] **Step 1.2: Read the end of CreateToolLabel to find exact insertion point**

Open `/tmp/PealLib/Library.lua`, find line 5874 (`function Library:CreateToolLabel`). The function ends around line 5894 (the `return lbl` + `end`). Insert the new function immediately after that `end`, before the `task.defer` loop.

- [ ] **Step 1.3: Insert `CreateToolImagePreview` function**

After `Library:CreateToolLabel`'s closing `end` and before `task.defer(function()`, insert:

```lua
function Library:CreateToolImagePreview(parent, yPos)
    local container = Library:Create('Frame', {
        AnchorPoint      = Vector2.new(0.5, 0);
        BackgroundColor3 = Library.BackgroundColor;
        BorderSizePixel  = 0;
        Position         = UDim2.new(0.5, 0, yPos, 0);
        Size             = UDim2.new(0.88, 0, 0, 0);
        ZIndex           = 5;
        Parent           = parent;
    })
    Library:Create('UIAspectRatioConstraint', { AspectRatio = 1; Parent = container })
    Library:Create('UICorner', { CornerRadius = UDim.new(0, 4); Parent = container })
    Library:AddToRegistry(container, { BackgroundColor3 = 'BackgroundColor' })

    local img = Library:Create('ImageLabel', {
        BackgroundTransparency = 1;
        Size                   = UDim2.new(1, 0, 1, 0);
        ScaleType              = Enum.ScaleType.Fit;
        Image                  = '';
        ZIndex                 = 6;
        Parent                 = container;
    })

    local preview = {}
    function preview:SetImage(id)
        img.Image = id and id ~= '' and ('rbxassetid://' .. tostring(id)) or ''
    end
    function preview:SetVisible(vis)
        container.Visible = vis
    end
    preview.container = container
    preview.label     = img
    return preview
end
```

- [ ] **Step 1.4: Commit and push to GitHub**

```bash
cd /tmp/PealLib
git add Library.lua
git commit -m "feat: add CreateToolImagePreview function"
git push origin main
```

- [ ] **Step 1.5: Verify push succeeded**

```bash
gh api repos/pealz1/PealLib/commits/main --jq '.commit.message'
```

Expected output: `feat: add CreateToolImagePreview function`

---

## Task 2: Fix Server Browser

**Files:**
- Modify: `RomazHub.lua` lines ~548–582 (getServerThumbnails dead block) and ~648–685 (fetchServers)

- [ ] **Step 2.1: Remove dead GET block from `getServerThumbnails`**

In `RomazHub.lua`, inside `getServerThumbnails` (around line 553–558), find and delete this entire `pcall` block:

```lua
pcall(function()
     body = HttpService:JSONDecode(game:HttpGet(
        "https://thumbnails.roblox.com/v1/batch",
        true
    ))
end)
```

The `requestBody`-based POST pcall immediately below it is kept as-is.

- [ ] **Step 2.2: Replace `fetchServers` HTTP call**

Find `fetchServers` (around line 648). Replace the two `game:HttpGet` calls with `requestFunc`-based calls:

Current (to replace):
```lua
local ok, result = pcall(function()
    return HttpService:JSONDecode(
        game:HttpGet("https://games.roblox.com/v1/games/" .. game.PlaceId .. "/servers/Public?sortOrder=Asc&limit=100")
    )
end)
```

Replace with:
```lua
local ok, result = pcall(function()
    local url = "https://games.roblox.com/v1/games/" .. game.PlaceId .. "/servers/Public?sortOrder=Asc&limit=100"
    local resp = requestFunc({Url = url, Method = "GET"})
    if not resp or resp.StatusCode ~= 200 then
        resp = {Body = game:HttpGet(url, true)}
    end
    return HttpService:JSONDecode(resp.Body)
end)
```

Do the same for the fallback call (limit=10 version) immediately below it:
```lua
local ok2, result2 = pcall(function()
    local url = "https://games.roblox.com/v1/games/" .. game.PlaceId .. "/servers/Public?sortOrder=Asc&limit=10"
    local resp = requestFunc({Url = url, Method = "GET"})
    if not resp or resp.StatusCode ~= 200 then
        resp = {Body = game:HttpGet(url, true)}
    end
    return HttpService:JSONDecode(resp.Body)
end)
```

- [ ] **Step 2.3: Commit**

```bash
cd /c/Users/pealz/.local/bin
git add RomazHub.lua
git commit -m "fix: use requestFunc for server browser to pass auth cookies"
```

---

## Task 3: Rebuild Decal Tool UI

**Files:**
- Modify: `RomazHub.lua` lines ~6875–6981 (sui/setdecal/rotatebutton/imageindicator block) and ~7054–7063 (equip/unequip wiring in createdecaltool)

- [ ] **Step 3.1: Delete old `sui` block and replace with themed panel**

Find the block that starts at `local sui = Instance.new("ScreenGui")` (~line 6875) and ends at the `sizeconst3.Parent = imageindicator` line (~line 6928). Delete all of it.

Replace with:

```lua
_decPanel = createToolPanel({name = "DecalToolGui", title = "Decal Tool", size = UDim2.new(0, 210, 0, 185)})
decalInput = createToolInput(_decPanel.frame, 0.18, "Enter Decal ID")
createToolButton(_decPanel.frame, "Rotate 90°", 0.36, 0.06, 0.88, function()
    decalrotation = decalrotation + 90
end)
_decPreview = Library:CreateToolImagePreview(_decPanel.frame, 0.52)
```

Note: `_decPreview` calls `Library:CreateToolImagePreview` directly (not a local wrapper). The local wrapper block at lines ~5871–5874 defines `createToolPanel`, `createToolInput`, `createToolButton`, and `createToolLabel` — do NOT add a `createToolImagePreview` wrapper there; just call `Library:CreateToolImagePreview` inline as shown above.

Note: `fakememe`, `suui`, and `imageindicator2` blocks (lines ~6930–6950) are left completely unchanged.

- [ ] **Step 3.2: Update `updatememeifydisplays`**

Find `updatememeifydisplays` (~line 6952). Replace:
```lua
function updatememeifydisplays()
    imageindicator.Image = "https://www.roblox.com/Thumbs/Asset.ashx?width=420&height=420&assetId="..memeifyid
    imageindicator2.Image = "https://www.roblox.com/Thumbs/Asset.ashx?width=420&height=420&assetId="..memeifyid
end
```

With:
```lua
function updatememeifydisplays()
    _decPreview:SetImage(memeifyid)
    imageindicator2.Image = "https://www.roblox.com/Thumbs/Asset.ashx?width=420&height=420&assetId=" .. memeifyid
end
```

- [ ] **Step 3.3: Simplify `setdecal` connection and remove rotate button connection**

Find the connections block (~lines 6957–6981). The old `setdecal` and `rotatebutton` connection code (with the guard pattern) is deleted entirely. Replace with a single clean connection for the new `decalInput`:

```lua
table.insert(scriptConnections, decalInput:GetPropertyChangedSignal("Text"):Connect(function()
    memeifyid = isolatenumbers(decalInput.Text)
    updatememeifydisplays()
end))
```

(The rotate button callback is already wired inline in the `createToolButton` call in Step 3.1.)

Also delete the `local decalrotation = 0` line from here — `decalrotation` will be declared at module scope earlier (see cleanup task).

- [ ] **Step 3.4: Fix equip/unequip wiring in `createdecaltool`**

Find the `Equipped` and `Unequipped` connection in `createdecaltool` (~lines 7054–7063):

```lua
table.insert(connections,currentdectool.Equipped:Connect(function()
    equipped = true
    sui.Enabled = true
    suui.Enabled = true
end))
table.insert(connections,currentdectool.Unequipped:connect(function()
    equipped = false
    sui.Enabled = false
    suui.Enabled = false
end))
```

Replace with:
```lua
table.insert(connections, currentdectool.Equipped:Connect(function()
    equipped = true
    _decPanel.gui.Enabled = true
    suui.Enabled = true
end))
table.insert(connections, currentdectool.Unequipped:Connect(function()
    equipped = false
    _decPanel.gui.Enabled = false
    suui.Enabled = false
end))
```

Note the `.connect` typo on `Unequipped` is also fixed to `.Connect`.

- [ ] **Step 3.5: Commit**

```bash
git add RomazHub.lua
git commit -m "feat: rebuild decal tool UI to match Library theme"
```

---

## Task 4: Variable Renames

**Files:**
- Modify: `RomazHub.lua` — rename all occurrences of specific variables

Do these renames one at a time using find-and-replace. Each rename must be whole-word to avoid partial matches.

- [ ] **Step 4.1: Rename `decalrotations` → `drot`**

Important: rename `decalrotations` but NOT `decalrotation` (the integer). Use exact-string replacement to avoid collision:
- Replace `decalrotations2` → `drot2` FIRST (to avoid partial match with step below)
- Then replace `decalrotations[` → `drot[` and `decalrotations =` → `drot =`

All occurrences of `drot2` (formerly `decalrotations2`):
- Declaration `local decalrotations2 = {}` (~line 6847)
- Six `decalrotations2[...]` assignments (~lines 6848–6853)
- Two uses in `createdecaltool` Heartbeat and Activated handlers

All occurrences of `drot` (formerly `decalrotations`):
- Declaration `local decalrotations = {}` (~line 6841)
- Six `decalrotations[...]` assignments (~lines 6841–6846)
- `local screwroblox = decalrotations[selectside]` (appears twice, ~lines 7095, 7145)

- [ ] **Step 4.2: Rename `idkwhattonamets` → `dsizes`**

Replace the table declaration (`local idkwhattonamets = {}`) and all `idkwhattonamets[...]` table-key assignments/lookups.

Also rename the local temp variable `idkwhattonamets2` → `dsizes2` everywhere it appears (inside `createdecaltool` Activated handler and Heartbeat handler — these are local function-scope variables, not the table itself).

- [ ] **Step 4.3: Rename `AUTOSCRIPT_URL` → `AUTOURL`**

One declaration, one use in `payload = ...` format string.

**Skip `memeifyid → mid`** — `createdecaltool` already has a local `mid` variable (~line 7077) that would shadow the global. Leave `memeifyid` unchanged throughout the file. Do NOT rename `requestFunc` (referenced by external scripts via `getgenv()`). Do NOT rename `decalrotation` (already short; would collide visually with `drot`/`drot2`).

- [ ] **Step 4.4: Commit renames**

```bash
git add RomazHub.lua
git commit -m "refactor: rename long variables to shorter equivalents"
```

---

## Task 5: Full Code Cleanup

**Files:**
- Modify: `RomazHub.lua` — remove comments, fix indentation, collapse blank lines

This is the largest task. Work through the file top-to-bottom in sections.

- [ ] **Step 5.1: Remove all single-line comments**

Remove every line that is solely a `-- comment` (no code on the line). Also remove inline `-- comments` appended to code lines (e.g., `...CFrame.new(...) * lat -- dont know why...` → keep the code, drop the comment text).

- [ ] **Step 5.2: Remove all block comments**

Find and delete all `--[[ ... ]]` blocks. There are several — including the commented-out sound block at the top of `createdecaltool` and the `--[[if decalrotation ...]]` block in `getfixedthing`.

- [ ] **Step 5.3: Collapse excessive blank lines**

Find any place with 3 or more consecutive blank lines and reduce to 1.

- [ ] **Step 5.4: Fix indentation consistency**

Ensure all code uses 4-space indentation. Pay particular attention to the top of the file where services are assigned (lines 24–33) — these are inconsistently spaced.

- [ ] **Step 5.5: Remove unnecessary `local` from module-scope globals**

Only remove `local` where the variable is genuinely used as a cross-scope global. Go through these specific cases:

- `local _rotPanel` (line ~6177) — used only within its surrounding scope; keep `local` unless the panel needs to be accessed in `createdecaltool`. Check actual usage before deciding.
- `local selectionbox` (line ~6171) — used inside `createrotool`'s closures; keep `local`
- `local rotatehandles` (line ~6169) — keep `local`
- General rule: if removing `local` has no effect (variable isn't read outside its scope), leave `local` in place

- [ ] **Step 5.6: Commit cleanup**

```bash
git add RomazHub.lua
git commit -m "refactor: remove comments, fix formatting, clean up variable scopes"
```

---

## Task 6: Final Verification and Push

- [ ] **Step 6.1: Verify `_decPreview` is referenced correctly**

Search for any remaining references to the deleted variables:
```bash
grep -n "sui\.\|setdecal\|rotatebutton\b\|imageindicator[^2]" RomazHub.lua | grep -v "suui\|imageindicator2"
```

Expected: no output (all old references gone)

- [ ] **Step 6.2: Verify renames are complete**

```bash
grep -n "decalrotations\b\|idkwhattonamets\b\|AUTOSCRIPT_URL\b" RomazHub.lua
```

Expected: no output

- [ ] **Step 6.3: Verify no duplicate blank lines**

```bash
grep -c "^$" RomazHub.lua
```

Check that the count is reasonable (should be much lower than before cleanup).

- [ ] **Step 6.4: Final commit and push RomazHub**

```bash
cd /c/Users/pealz/.local/bin
git add RomazHub.lua
git status
git push origin master
```

- [ ] **Step 6.5: Verify PealLib is live**

```bash
curl -s "https://raw.githubusercontent.com/pealz1/PealLib/main/Library.lua" | grep -c "CreateToolImagePreview"
```

Expected: `1`
