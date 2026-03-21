# RomazHub — Relay Detection & Webhook Fix Design
**Date:** 2026-03-21
**Scope:** RomazHub.lua

---

## Problem Summary

Three independent bugs / improvement areas in `RomazHub.lua`:

1. **Join URL broken in Discord** — `[**Join Player**](url)` uses bold markdown inside the hyperlink bracket, which Discord does not render correctly in embed field values; the link appears as plain text.

2. **User detection (HUB badge) never fires** — Two systems exist simultaneously:
   - `markHubActive()` writes a `BoolValue` named `RomazHubActive` into the local character. Under Roblox FilteringEnabled, LocalScript mutations to character instances are client-local and **not replicated** to other clients. `hookUserBillboards()` watches for this value on remote players' characters and will **never trigger**.
   - The relay heartbeat (`/heartbeat`) is the correct system but has a secondary bug: `confirmedHubUsers` keys on player objects, which go stale when a player respawns (new character = lost billboard, but key still present so re-tagging is skipped).

3. **Webhook embeds verbose and hard to read** — Join link buried in a field value, five fields spread across multiple inline blocks, executor/performance split across two fields. Action/paint webhooks share the same issues.

---

## Design

### 1. Join URL

**Change:** Remove bold formatting from inside the link text bracket.

```
-- Before
"[**Join Player**](https://www.roblox.com/games/start?placeId=%s&gameInstanceId=%s)"

-- After
"[Join Server](https://www.roblox.com/games/start?placeId=%s&gameInstanceId=%s)"
```

Use `tostring()` on PlaceId and JobId to be explicit.

---

### 2. Relay / User Detection Redesign

#### Remove (dead code)
- `markHubActive()` function
- `markHubActive()` call site
- `plr.CharacterAdded` hook that calls `markHubActive()`
- Entire `hookUserBillboards()` function and its call site

#### Add: userId-keyed tracking
Declare `confirmedHubUserIds` immediately adjacent to the existing `confirmedHubUsers` declaration (around line 1799):
```lua
local confirmedHubUserIds = {}  -- [userId: number] = true
```
This survives respawns because userId never changes. **Both tables are kept in sync**: wherever a player is confirmed as a hub user, both `confirmedHubUserIds[userId] = true` and `confirmedHubUsers[player] = true` are set. The existing billboard badge rendering still reads from `confirmedHubUsers` (player-keyed) so that table must remain populated.

#### Fix `sendRelayHeartbeat`
On each response, iterate the full `users` array:
- Convert uid to number
- Skip self
- Find matching player in server by userId
- If found and `confirmedHubUserIds[numId]` is nil:
  - Set `confirmedHubUserIds[numId] = true`
  - Set `confirmedHubUsers[player] = true` (existing billboard logic unchanged)
  - Call `watchHubUserRespawn(player)`
  - Spawn `createUserBillboard(player, true)` after 0.5s delay
  - Notify

#### Fix respawn handling
`plr.CharacterAdded` already calls `retagAllConfirmedUsers()` after a `task.wait(1)` outer delay. Change `retagAllConfirmedUsers` so it iterates `confirmedHubUserIds` — look up each userId against current player list, then call `createUserBillboard`. The internal 0.5s delay inside `retagAllConfirmedUsers` stacks on top of the outer 1s, giving ~1.5s total before billboards re-appear (intentional — character needs to load).

```lua
local function retagAllConfirmedUsers()
    for userId, _ in pairs(confirmedHubUserIds) do
        for _, p in ipairs(Players:GetPlayers()) do
            if p.UserId == userId then
                task.spawn(function()
                    task.wait(0.5)
                    createUserBillboard(p, true)
                end)
                break
            end
        end
    end
end
```

#### Fix PlayerRemoving
Update the **existing** `PlayerRemoving` handler (do not add a second one — a separate ESP handler already exists). Add `confirmedHubUserIds` cleanup alongside `confirmedHubUsers`:
```lua
Players.PlayerRemoving:Connect(function(player)
    confirmedHubUsers[player] = nil
    confirmedHubUserIds[player.UserId] = nil  -- add this line
    -- existing hubRespawnConns cleanup stays
end)
```

#### Also fix `watchHubUserRespawn`
Guard on `confirmedHubUserIds` (userId-keyed, survives respawn) not `confirmedHubUsers` (player object-keyed). When a confirmed hub user respawns, re-create their billboard:
```lua
local function watchHubUserRespawn(player)
    if hubRespawnConns[player] then
        hubRespawnConns[player]:Disconnect()
        hubRespawnConns[player] = nil
    end
    hubRespawnConns[player] = player.CharacterAdded:Connect(function()
        task.wait(1.5)
        if confirmedHubUserIds[player.UserId] then
            createUserBillboard(player, true)
        end
    end)
end
```

---

### 3. Webhook Improvements

#### Startup embed (script executed)
- Add `description` field with the join link as a standalone hyperlink
- Reduce to **3 fields**: `👤 Player`, `🎮 Server`, `⚙️ Client`
- `👤 Player`: username, display name, userId, account age, creation date
- `🎮 Server`: game name, server type, player count, place ID
- `⚙️ Client`: executor, HWID, platform, region, ping, fps
- Remove redundant `Performance` and `Time` fields (merge into Client)
- Add enlighten status and admin/team info to Player field
- Keep thumbnail (avatar headshot)

#### `sendPaintActionWebhook`
- Add join link as description
- Slim to 2 fields: Player | Paint Data (server info inlined)

#### `sendActionWebhook`
- Add join link as description
- Slim to 3 fields: Action | Player | Server Stats

---

## Files Changed

| File | Change |
|------|--------|
| `RomazHub.lua` | All changes — joinURL, relay redesign, webhook embeds |

---

## Success Criteria

- Clicking join link in Discord opens Roblox and joins the correct server
- HUB badge appears on players who have the hub open in the same server, reliably, without requiring a reset
- Case A: When the **local player** respawns, other confirmed hub users' billboards are recreated via `retagAllConfirmedUsers`
- Case B: When a **confirmed hub user** respawns, their own billboard is recreated via `watchHubUserRespawn`
- Webhook embeds are readable at a glance; join link is clickable as a standalone description link
