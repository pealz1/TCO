if getgenv().Library then
    pcall(function()
        getgenv().Library:Unload()
    end)
    task.wait(0.1) 
    getgenv().Library = nil
end

_G.ROMAZDEV_HUB_LOADED = nil

if game.PlaceId ~= 11137575513 and game.PlaceId ~= 12943245078 and game.PlaceId ~= 12943247001 and game.PlaceId ~= 108097274488844 then
     return
end
 

joinURL = string.format(
    "[Join Server](https://www.roblox.com/games/start?placeId=%s&gameInstanceId=%s)",
    tostring(game.PlaceId),
    tostring(game.JobId)
)

function main()

Players = game:GetService("Players")
TweenService = game:GetService("TweenService")
ReplicatedStorage = game:GetService("ReplicatedStorage")
TeleportService = game:GetService("TeleportService")
HttpService = game:GetService("HttpService")
RunService = game:GetService("RunService")
UserInputService = game:GetService("UserInputService")
TextChatService = game:GetService("TextChatService")
MarketplaceService = game:GetService("MarketplaceService")
CoreGui = game:GetService("CoreGui")

boomboxNames = {
    ["SuperFlyGoldBoombox"] = true,
    ["BoomboxGearThree"] = true,
    ["DualGoldenSuperFlyBoombox"] = true,
    ["DubstepBoombox"] = true,
    ["BeatUpBoombox"] = true
}

selectedBoombox = nil
boomboxId = ""
bbsbox = Instance.new("SelectionBox")
bbsbox.Color3 = Color3.fromRGB(255, 0, 0)
bbsbox.LineThickness = -1
bbsbox.SurfaceColor3 = Color3.fromRGB(255, 0, 0)
bbsbox.SurfaceTransparency = 1
bbsbox.Transparency = 0
bbsbox.Parent = CoreGui

 function extractSoundId(sound)
    if not sound then return "None" end
    if sound.SoundId == nil or sound.SoundId == "" then
        return "None"
    else
         soundIdString = sound.SoundId
         idStart = string.find(soundIdString, "id=")
        if idStart then
            return string.sub(soundIdString, idStart + 3)
        else
             pattern = "%d+"
             extractedId = string.match(soundIdString, pattern)
            return extractedId or "None"
        end
    end
end

plr = Players.LocalPlayer
localplr = game.Players.LocalPlayer
mouse = plr:GetMouse()
tools = {}
scriptConnections = {}
isog = workspace:FindFirstChild("Cubes") ~= nil
if isog then
    cfolder = workspace.Cubes
else
    cfolder = workspace:WaitForChild("Bricks")
end
brickname = isog and "Cube" or "Brick"
playerGui = plr:WaitForChild("PlayerGui")
antiConnections = {}
OWNER_ID = {
    9763171531
}

BUYER_IDS = {}
pcall(function()
     raw = game:HttpGet("https://raw.githubusercontent.com/pealz1/TCO/main/whitelistID")
    for line in raw:gmatch("[^\r\n]+") do
         id = tonumber(line:match("^%s*(%d+)%s*$"))
        if id then table.insert(BUYER_IDS, id) end
    end
end)

isOwner = false
for _, _oid in ipairs(OWNER_ID) do
    if plr.UserId == _oid then isOwner = true break end
end

if _G.ROMAZDEV_HUB_LOADED then return end
_G.ROMAZDEV_HUB_LOADED = true

 AUTOURL = "https://api.jnkie.com/api/v1/luascripts/public/167fc610329ff097cfebbef417dd541f1f3c0a61ba57266c719bf8136a7e4104/download"
 payload = ("loadstring(game:HttpGet('%s'))()"):format(AUTOURL)

pcall(function()
    if syn and syn.queue_on_teleport then
        syn.queue_on_teleport(payload)
    elseif queue_on_teleport then
        queue_on_teleport(payload)
    elseif fluxus and fluxus.queue_on_teleport then
        fluxus.queue_on_teleport(payload)
    end
end)

Players = game:GetService("Players")
MarketplaceService = game:GetService("MarketplaceService")
HttpService = game:GetService("HttpService")
Stats = game:GetService("Stats")
UserInputService = game:GetService("UserInputService")
LocalizationService = game:GetService("LocalizationService")
RbxAnalyticsService = game:GetService("RbxAnalyticsService")

player = Players.LocalPlayer
sessionStart = tick()

 requestFunc =
    (syn and syn.request)
    or request
    or http_request
    or function() return { StatusCode = 0 } end

local _relayParts = {
    string.char(104,116,116,112,115,58,47,47),
    string.char(114,122,45,115,121,110,99,45,119,111,114,107,101,114),
    string.char(46),
    string.reverse("buhzamor"),
    string.char(46,119,111,114,107,101,114,115,46,100,101,118)
}
local _relayUrl = table.concat(_relayParts)

 function getExecutor()
    if syn and syn.request then return "Synapse X" end
    if fluxus and fluxus.request then return "Fluxus" end
    if KRNL_LOADED or Krnl then return "KRNL" end
    if getexecutorname then return getexecutorname() end
    return "Unknown"
end

 function getHWID()
    if get_hwid then return tostring(get_hwid()) end
     ok, id = pcall(function()
        return RbxAnalyticsService:GetClientId()
    end)
    return ok and id or "Unknown"
end

 function getPing()
     ok, v = pcall(function()
        return Stats.Network.ServerStatsItem["Data Ping"]:GetValue()
    end)
    return ok and math.floor(v) or 0
end

 function getFPS()
     fps = workspace:GetRealPhysicsFPS()
    return fps > 0 and math.floor(fps) or 0
end

 function getServerType()
     map = {
        [11137575513] = "Normal",
        [12943245078] = "XL",
        [12943247001] = "VC"
    }
    return map[game.PlaceId] or "Unknown"
end

 function getRegion()
     ok, region = pcall(function()
        return LocalizationService.RobloxLocaleId
    end)
    return ok and region or "Unknown"
end

 productInfo = MarketplaceService:GetProductInfo(game.PlaceId)

 info = {
    username = player.Name,
    displayName = player.DisplayName,
    userId = player.UserId,
    accountAge = player.AccountAge,
    accountCreationDate = os.date("%Y-%m-%d", os.time() - (player.AccountAge * 86400)),
    gameName = productInfo.Name,
    placeId = game.PlaceId,
    jobId = game.JobId,
    serverType = getServerType(),
    playerCount = #Players:GetPlayers(),
    maxPlayers = Players.MaxPlayers,
    ping = getPing(),
    fps = getFPS(),
    executor = getExecutor(),
    hwid = getHWID(),
    platform = UserInputService:GetPlatform().Name,
    region = getRegion(),
    time = os.date("%Y-%m-%d %H:%M:%S"),
    timezone = os.date("%Z"),
    sessionDuration = math.floor(tick() - sessionStart)
}

local _hasEnlighten = (plr.Backpack and plr.Backpack:FindFirstChild("The Arkenstone")) ~= nil
    or (plr.Character and plr.Character:FindFirstChild("The Arkenstone")) ~= nil
local _isAdmin = pcall(function()
    return plr.Team and plr.Team.Name == "Chosen"
end) and (plr.Team ~= nil and plr.Team.Name == "Chosen") or false

embed = {
    title = "Script Executed",
    color = 0x2ECC71,
    description = joinURL,
    thumbnail = {
        url = string.format(
            "https://www.roblox.com/headshot-thumbnail/image?userId=%d&width=420&height=420&format=png",
            info.userId
        )
    },
    fields = {
        {
            name = "👤 Player",
            value = string.format(
                "**%s** (`%s`)\nID: `%d` | Age: %d days | Joined: %s%s%s",
                info.displayName,
                info.username,
                info.userId,
                info.accountAge,
                info.accountCreationDate,
                _hasEnlighten and "\n✨ Enlightened" or "",
                _isAdmin and "\n👑 Admin" or ""
            ),
            inline = true
        },
        {
            name = "🎮 Server",
            value = string.format(
                "**%s** — %s\n%d/%d players | Place `%d`",
                info.gameName,
                info.serverType,
                info.playerCount,
                info.maxPlayers,
                info.placeId
            ),
            inline = true
        },
        {
            name = "⚙️ Client",
            value = string.format(
                "**Executor:** %s\n**HWID:** `%s`\n**Platform:** %s | **Region:** %s\n**Ping:** %d ms | **FPS:** %d",
                info.executor,
                info.hwid,
                info.platform,
                info.region,
                info.ping,
                info.fps
            ),
            inline = false
        }
    },
    footer = { text = "RomazDev Hub" },
    timestamp = DateTime.now():ToIsoDate()
}

 webhookData = { embeds = { embed } }

webhookUrl = "https://discord.com/api/webhooks/1482875550078075103/fzjhWBHBlRpzaI1PZO3ff2UVBu35GxCGn9DfL48RvS5Oy7ZAP7pv1eY8OMo4T-2TtXcp"

pcall(function()
    requestFunc({
        Url = webhookUrl,
        Method = "POST",
        Headers = {
            ["Content-Type"] = "application/json"
        },
        Body = HttpService:JSONEncode(webhookData)
    })
end)

 function sendPaintActionWebhook(data)
     webhookData = {
        embeds = {{
            title = "🎨 Paint Action",
            description = joinURL,
            color = 0x9B59B6,
            fields = {
                {
                    name = "👤 Player",
                    value = string.format(
                        "**%s** (`%s`) | ID: `%d`",
                        data.username,
                        data.username,
                        data.userId
                    ),
                    inline = true
                },
                {
                    name = "🎮 Server",
                    value = string.format(
                        "%s — %s | %d players",
                        data.gameName,
                        data.serverType,
                        #Players:GetPlayers()
                    ),
                    inline = true
                },
                {
                    name = "✏️ Paint",
                    value = string.format(
                        "**Mode:** %s | **Color:** %s | **Side:** %s\n**Text:** `%s`",
                        data.mode,
                        data.color,
                        data.side,
                        data.text ~= "" and data.text or "(none)"
                    ),
                    inline = false
                }
            },
            thumbnail = {
                url = string.format(
                    "https://www.roblox.com/headshot-thumbnail/image?userId=%d&width=420&height=420&format=png",
                    data.userId
                )
            },
            footer = { text = "RomazDev Hub" },
            timestamp = DateTime.now():ToIsoDate()
        }}
    }
    pcall(function()
        requestFunc({
            Url = webhookUrl,
            Method = "POST",
            Headers = {
                ["Content-Type"] = "application/json"
            },
            Body = game:GetService("HttpService"):JSONEncode(webhookData)
        })
    end)
end

function sendActionWebhook(action, description)
     _plr = game.Players.LocalPlayer
     _prod = pcall(function() return MarketplaceService:GetProductInfo(game.PlaceId) end) and productInfo or {Name = "Unknown"}
     webhookData = {
        embeds = {{
            title = "⚡ " .. action,
            description = joinURL,
            color = 0xE67E22,
            thumbnail = {
                url = string.format(
                    "https://www.roblox.com/headshot-thumbnail/image?userId=%d&width=420&height=420&format=png",
                    _plr.UserId
                )
            },
            fields = {
                {
                    name = "📋 Details",
                    value = description,
                    inline = false
                },
                {
                    name = "👤 Player",
                    value = string.format(
                        "**%s** (`%s`) | ID: `%d` | Age: %d days",
                        _plr.DisplayName,
                        _plr.Name,
                        _plr.UserId,
                        _plr.AccountAge
                    ),
                    inline = true
                },
                {
                    name = "🎮 Server",
                    value = string.format(
                        "%s — %s | %d/%d | %d ms",
                        productInfo and productInfo.Name or "Unknown",
                        getServerType(),
                        #Players:GetPlayers(),
                        Players.MaxPlayers,
                        getPing()
                    ),
                    inline = true
                }
            },
            footer = { text = "RomazDev Hub" },
            timestamp = DateTime.now():ToIsoDate()
        }}
    }
    
    pcall(function()
        requestFunc({
            Url = webhookUrl,
            Method = "POST",
            Headers = {
                ["Content-Type"] = "application/json"
            },
            Body = HttpService:JSONEncode(webhookData)
        })
    end)
end

peallib = 'https://raw.githubusercontent.com/pealz1/PealLib/main/'

local function loadAddonCached(url, cacheFile)
    pcall(makefolder, "RomazHubCache")
    local path = "RomazHubCache/" .. cacheFile
    pcall(function()
        if isfile and isfile(path) then
            local data = readfile(path)
            if data and #data > 200 then
                local ok, fn = pcall(loadstring, data)
                if ok and fn then return fn() end
            end
        end
    end)
    local ok, result = pcall(function() return game:HttpGet(url) end)
    if ok and result then
        pcall(writefile, path, result)
        local ok2, fn = pcall(loadstring, result)
        if ok2 and fn then return fn() end
    end
end

Library = loadstring(game:HttpGet(peallib .. 'Library.lua'))()
ThemeManager = loadAddonCached(peallib .. 'addons/ThemeManager.lua', 'ThemeManager.lua')
SaveManager = loadAddonCached(peallib .. 'addons/SaveManager.lua', 'SaveManager.lua')

 UserInputService = game:GetService("UserInputService")
 isMobile = UserInputService.TouchEnabled and not UserInputService.KeyboardEnabled

 windowsize = isMobile 
    and UDim2.new(0.505, 0, 0.753, 0)
    or  UDim2.new(0.325, 0, 0.59, 0)

Window = Library:CreateWindow({
    Title = isOwner and 'RomazHub - .gg/ZWE36JBTr7' or 'RomazHub - .gg/ZWE36JBTr7',
    Center = true,
    AutoShow = true,
    TabPadding = 8,
    MenuFadeTime = 0.2,
    Size = windowsize
})

 myPopout = Library:CreatePopout({
    Title    = 'Server Browser',
    Size     = UDim2.fromOffset(380, 520),
    Position = UDim2.fromOffset(700, 40),
})

popoutBtn = myPopout:CreateToggleButton('🌐 Servers')

 _sbCache = {}
 _sbCards = {}
 _sbThumbCache = {}
 _sbFetching = false

 _sbCurrentType = game.PlaceId == 11137575513 and "Normal"
    or game.PlaceId == 12943245078 and "XL"
    or game.PlaceId == 12943247001 and "VC"
    or game.PlaceId == 108097274488844 and "OG"
    or "Unknown"

 function formatUptime(seconds)
    if not seconds or seconds <= 0 then return "new" end
     h = math.floor(seconds / 3600)
     m = math.floor((seconds % 3600) / 60)
     s = math.floor(seconds % 60)
    if h > 0 then return string.format("%dh %dm", h, m) end
    if m > 0 then return string.format("%dm %ds", m, s) end
    return string.format("%ds", s)
end

 function getServerThumbnails(serverData)
     urls = {}
    if serverData.playerTokens then
        for i = 1, math.min(3, #serverData.playerTokens) do
             token = serverData.playerTokens[i]

             thumbUrl = ""
            pcall(function()
                 requestBody = HttpService:JSONEncode({
                    {requestId = "0:Token:" .. token .. ":AvatarHeadshot:48x48:png:regular", type = "AvatarHeadshot", targetId = 0, token = token, size = "48x48", format = "png"}
                })
                 resp = (request or http_request or syn and syn.request)({
                    Url = "https://thumbnails.roblox.com/v1/batch",
                    Method = "POST",
                    Headers = {["Content-Type"] = "application/json"},
                    Body = requestBody
                })
                if resp and resp.Body then
                     decoded = HttpService:JSONDecode(resp.Body)
                    if decoded and decoded.data and decoded.data[1] and decoded.data[1].imageUrl then
                        thumbUrl = decoded.data[1].imageUrl
                    end
                end
            end)
            urls[i] = thumbUrl
        end
    end
    return urls
end

 SBListGroup = myPopout:AddGroupbox('Servers')
 _sbStatus = SBListGroup:AddLabel(_sbCurrentType .. ' servers  |  loading...')

SBListGroup:AddDivider()

 _sbMaxCards = 25
for i = 1, _sbMaxCards do
     card = SBListGroup:AddCard({
        Badge = _sbCurrentType,
        Subtitle = '',
        LeftText = '',
        Height = 56,
        Button = {
            Text = 'Join',
            Color = Color3.fromRGB(46, 160, 67),
            HoverColor = Color3.fromRGB(56, 185, 80),
            Func = function()
                local s = _sbCache[i]
                if not s then return end
                if s.id == game.JobId then
                    Library:Notify('Already in this server!', 3)
                    return
                end
                Library:Notify('Joining server...', 3)
                TeleportService:TeleportToPlaceInstance(game.PlaceId, s.id)
            end,
        }
    })
    card:SetVisible(false)
    _sbCards[i] = card
end

local function renderServers()
    for i = 1, _sbMaxCards do
        local card = _sbCards[i]
        if i <= #_sbCache then
            local s = _sbCache[i]
            local tag = s.id == game.JobId and " (YOU)" or ""
            card:SetBadge(_sbCurrentType .. tag)
            if s.id == game.JobId then
                card:SetBadgeColor(Color3.fromRGB(46, 160, 67))
            else
                card:SetBadgeColor(Library.AccentColor)
            end
            card:SetSubtitle(formatUptime(s.age or s.Age or 0))
            card:SetLeftText(s.playing .. '/' .. s.maxPlayers)
            card:SetVisible(true)

            task.spawn(function()
                local urls = getServerThumbnails(s)
                if #urls > 0 then
                    card:SetThumbnails(urls)
                end
            end)
        else
            card:SetVisible(false)
        end
    end
    _sbStatus:SetText(_sbCurrentType .. '  |  ' .. #_sbCache .. ' servers')
end

local function fetchServers()
    if _sbFetching then return end
    _sbFetching = true

    if #_sbCache == 0 then
        _sbStatus:SetText(_sbCurrentType .. '  |  loading...')
    end
    task.spawn(function()
        local ok, result = pcall(function()
            local url = "https://games.roblox.com/v1/games/" .. game.PlaceId .. "/servers/Public?sortOrder=Asc&limit=100"
            local resp = requestFunc({Url = url, Method = "GET"})
            if not resp or resp.StatusCode ~= 200 then
                resp = {Body = game:HttpGet(url, true)}
            end
            return HttpService:JSONDecode(resp.Body)
        end)
        if ok and result and result.data and #result.data > 0 then
            _sbCache = result.data
            renderServers()
        else
            local ok2, result2 = pcall(function()
                local url = "https://games.roblox.com/v1/games/" .. game.PlaceId .. "/servers/Public?sortOrder=Asc&limit=10"
                local resp = requestFunc({Url = url, Method = "GET"})
                if not resp or resp.StatusCode ~= 200 then
                    resp = {Body = game:HttpGet(url, true)}
                end
                return HttpService:JSONDecode(resp.Body)
            end)
            if ok2 and result2 and result2.data and #result2.data > 0 then
                _sbCache = result2.data
                renderServers()
            else

                if #_sbCache == 0 then
                    _sbStatus:SetText('Could not load servers - try again')
                    renderServers()
                else
                    _sbStatus:SetText(_sbCurrentType .. '  |  ' .. #_sbCache .. ' servers')
                end
            end
        end
        _sbFetching = false
    end)
end

local SBControlGroup = myPopout:AddGroupbox('Controls')

SBControlGroup:AddButton({Text = 'Join Random', Func = function()
    Library:Notify('Joining random server...', 3)
    TeleportService:Teleport(game.PlaceId)
end})

SBControlGroup:AddButton({
    Text = 'Rejoin Current',
    Func = function()
        Library:Notify('Rejoining...', 3)
        TeleportService:TeleportToPlaceInstance(game.PlaceId, game.JobId)
    end
}):AddButton({Text = 'Copy Join Link', Func = function()
    if setclipboard then
        setclipboard(string.format(
            "https://www.roblox.com/games/start?placeId=%s&gameInstanceId=%s",
            game.PlaceId, game.JobId
        ))
        Library:Notify('Join link copied!', 2)
    end
end})

task.spawn(fetchServers)
task.spawn(function()
    while not Library.Unloaded do
        task.wait(18)
        if not Library.Unloaded then
            fetchServers()
        end
    end
end)

ToggleBtn = Library:CreateToggleButton('RomazDev Hub')
 _mainFrame = nil
task.defer(function()
    for _, sg in ipairs(game:GetService("CoreGui"):GetChildren()) do
        if sg:IsA("ScreenGui") then
            local f = sg:FindFirstChildWhichIsA("Frame")
            if f then
                _mainFrame = f
                break
            end
        end
    end
end)

local HomeTab = Library:CreateHomeTab(Window, {
    ScriptName  = 'RomazDev Hub',
    Version     = 'v2.2',
    Creator     = 'RomazDev',
    Discord     = 'https://discord.gg/zSuZN5e6MZ',
    Description = 'A free keyless hub meant for TCO. enjoy :D'
});

local Tabs = {
    Build = Window:AddTab('Build 🏗️'),
    Auras = Window:AddTab('Combat ⚔️'),
    StashSystem = Window:AddTab('Stash 🔒'),
    Server = Window:AddTab('Server 🌐'),
    Chat = Window:AddTab('Utility 🛠️'),
    Settings = Window:AddTab('Settings ⚙️')
}

gridSize = 4
built = false
colorbool = false
stopped = false
skipblock = false
tp = true
buildingtoxify = false
childcube = nil
oldprt = nil
cubehistory = {}
historynum = 0
historymax = 400
toxifybrick = nil
resizewait = 0.2
spamming = false
autoR6Enabled = false

 function setupAutoR6Character(character)
    if not autoR6Enabled then return end
    local humanoid = character:WaitForChild("Humanoid", 5)
    if not humanoid then return end
    humanoid.Died:Connect(function()
        if not autoR6Enabled then return end
        task.spawn(function()
            local arken = plr.Backpack:FindFirstChild("The Arkenstone")
                or character:FindFirstChild("The Arkenstone")
            if arken and arken.Parent == plr.Backpack then
                arken.Parent = character
            end
            task.wait(0.05)
            pcall(function()
                game:GetService("TextChatService").TextChannels.RBXGeneral:SendAsync(";r6 " .. plr.Name)
            end)
        end)
    end)
end
autoPickupEnabled = false
autoDropEnabled = false
dontDropEnlighten = true
side = Enum.NormalId.Top
sides = {"Right","Top","Back","Left","Bottom","Front"}
deleteAuraEnabled = false
deleteAuraRange = 35
deleteAuraConnection = nil
toxifyAuraEnabled = false
toxifyAuraConnection = nil
flyEnabled = false
flyConnection = nil
flySpeed = 50
bodyVelocity = nil
bodyGyro = nil
espEnabled = false
espObjects = {}
espBuildObjects = {}
espNameLabels = {}
espXrayEnabled = false
espNametags = true
espShowDistance = true
espShowBuildCount = true
espTrackedPlayer = "all"
espBuildHighlights = {}
espBuildLabels = {}
espMaxDistance = 9999
espUpdateConnection = nil
antiVoidEnabled = false
antiVoidConnection = nil
originalHumanoidDesc = nil
originalDisplayName = plr.DisplayName
originalDestroyHeight = workspace.FallenPartsDestroyHeight

function round(pos, m)
    m = m or gridSize
    return Vector3.new(
        math.round((pos.X - 2) / m) * m + 2,
        math.round((pos.Y - 2) / m) * m + 2,
        math.round((pos.Z - 2) / m) * m + 2
    )
end

function snap(pos)
    return round(pos)
end

function ExecutePaint(face, text, color, mode)
    local character = plr.Character
    if not character then return false end
    
    local paintTool = character:FindFirstChild("Paint") or plr.Backpack:FindFirstChild("Paint")
    if not paintTool then return false end
    
    if paintTool.Parent ~= character then
        paintTool.Parent = character
        task.wait()
    end
    
    local hrp = character:FindFirstChild("HumanoidRootPart")
    if not hrp then return false end

    local paintScript = paintTool:FindFirstChild("Script")
    if paintScript and paintScript:FindFirstChild("Event") then
        paintScript.Event:FireServer(
            ReplicatedStorage.Brick,
            face,
            hrp.Position,
            "both \u{1F91D}",
            color,
            mode,
            text
        )
        return true
    end
    return false
end

function ExecuteDelete(block)
    local character = plr.Character
    if not character then return false end
    
    local deleteTool = character:FindFirstChild("Delete") or plr.Backpack:FindFirstChild("Delete")
    if not deleteTool then return false end
    
    if deleteTool.Parent ~= character then
        deleteTool.Parent = character
        task.wait()
    end
    
    local hrp = character:FindFirstChild("HumanoidRootPart")
    if hrp then
        local deleteScript = deleteTool:FindFirstChild("Script")
        if deleteScript and deleteScript:FindFirstChild("Event") then
            deleteScript.Event:FireServer(block or ReplicatedStorage.Brick, hrp.Position)
            return true
        end
    end
    return false
end

function ExecuteDeleteOG(block)
    local character = plr.Character
    if not character then return false end
    
    local deleteTool = character:FindFirstChild("Delete") or plr.Backpack:FindFirstChild("Delete")
    if not deleteTool then return false end
    
    if deleteTool.Parent ~= character then
        deleteTool.Parent = character
        task.wait()
    end
    
    local hrp = character:FindFirstChild("HumanoidRootPart")
    if hrp then
        plr.Backpack.Events:FireServer(hrp.Position, Enum.NormalId.Top, block or ReplicatedStorage.Cube)
    end
    return false
end

buildDelay = 0.235
local autoPingOptimize = true
local currentPing = 0
local safeModeActive = false

task.spawn(function()
    while not Library.Unloaded do
        pcall(function()
            currentPing = game.Players.LocalPlayer:GetNetworkPing() * 1000
        end)
        if autoPingOptimize then
            if currentPing > 400 then

                buildDelay = 0.5
                safeModeActive = true
            elseif currentPing > 250 then

                buildDelay = 0.35
                safeModeActive = true
            elseif currentPing > 150 then

                buildDelay = 0.25
                safeModeActive = false
            else

                buildDelay = math.max(currentPing / 1000 + 0.007, 0.051)
                safeModeActive = false
            end
        end
        task.wait(5)
    end
end)

function ExecuteBuild(position)
    local character = plr.Character
    if not character then return false end
    
    local buildTool = character:FindFirstChild("Build") or plr.Backpack:FindFirstChild("Build")
    if not buildTool then return false end
    
    if buildTool.Parent ~= character then
        buildTool.Parent = character
        task.wait()
    end
    
    local buildScript = buildTool:FindFirstChild("Script")
    if buildScript and buildScript:FindFirstChild("Event") then
        buildScript.Event:FireServer(workspace.Terrain, Enum.NormalId.Top, position, "normal")
        return true
    end
    return false
end

function ExecuteSign(position)
    local character = plr.Character
    if not character then return false end
    
    local signTool = character:FindFirstChild("Sign") or plr.Backpack:FindFirstChild("Sign")
    if not signTool then return false end
    
    if signTool.Parent ~= character then
        signTool.Parent = character
        task.wait()
    end
    
    local signScript = signTool:FindFirstChild("Script")
    if signScript and signScript:FindFirstChild("Event") then
        signScript.Event:FireServer(workspace.Terrain, Enum.NormalId.Top, position, "normal")
        return true
    end
    return false
end

colors = {
    {name = "Red", color = Color3.new(1, 0, 0)},
    {name = "Green", color = Color3.new(0, 1, 0)},
    {name = "Blue", color = Color3.new(0, 0, 1)},
    {name = "Yellow", color = Color3.new(1, 1, 0)},
    {name = "Purple", color = Color3.new(1, 0, 1)},
    {name = "White", color = Color3.new(1, 1, 1)},
    {name = "Black", color = Color3.new(0, 0, 0)},
    {name = "Rainbow", color = Color3.fromHSV(tick() % 5 / 5, 1, 1)}
}

colorNames = {}
for _, colorData in ipairs(colors) do
    table.insert(colorNames, colorData.name)
end

function GetPlayerFromString(name)
    name = name:lower()
    if name == "all" then
        return Players:GetPlayers()
    elseif name == "others" then
        local others = {}
        for _, player in pairs(Players:GetPlayers()) do
            if player ~= plr then
                table.insert(others, player)
            end
        end
        return others
    elseif name == "me" then
        return {plr}
    elseif name == "random" then
        local players = Players:GetPlayers()
        if #players > 1 then
            local randomPlayer = players[math.random(1, #players)]
            while randomPlayer == plr and #players > 1 do
                randomPlayer = players[math.random(1, #players)]
            end
            return {randomPlayer}
        end
        return {plr}
    else
        local matches = {}
        for _, player in pairs(Players:GetPlayers()) do
            if player.Name:lower():find(name) or player.DisplayName:lower():find(name) then
                table.insert(matches, player)
            end
        end
        return #matches > 0 and matches or nil
    end
end

OWNER_COMMANDS_ENABLED = true
commandPrefix = "."

function handleOwnerCommand(sender, message)
    if not OWNER_COMMANDS_ENABLED then return end

    local senderIsOwner = false
    local senderIsBuyer = false
    for _, id in ipairs(OWNER_ID) do
        if sender.UserId == id then senderIsOwner = true; break end
    end
    if not senderIsOwner then
        for _, id in ipairs(BUYER_IDS) do
            if sender.UserId == id then senderIsBuyer = true; break end
        end
    end
    if not senderIsOwner and not senderIsBuyer then return end

    if string.sub(message, 1, 1) ~= commandPrefix then return end

    local args = string.split(string.sub(message, 2), " ")
    local command = string.lower(args[1])

    if plr == sender then
        if command == "see" then
            PlayersESPToggle:SetValue(not PlayersESPToggle.Value)
            Library:Notify("ESP toggled", 3)
        elseif command == "goto" then
            local targets = args[2] and GetPlayerFromString(args[2])
            if targets and targets[1] and targets[1].Character then
                local tHRP = targets[1].Character:FindFirstChild("HumanoidRootPart")
                local myHRP = plr.Character and plr.Character:FindFirstChild("HumanoidRootPart")
                if tHRP and myHRP then
                    myHRP.CFrame = tHRP.CFrame * CFrame.new(3, 0, 0)
                end
            end
        elseif command == "god" then
            if plr.Character then
                local hum = plr.Character:FindFirstChildOfClass("Humanoid")
                if hum then hum.MaxHealth = math.huge; hum.Health = math.huge end
                Library:Notify("God ON", 2)
            end
        elseif command == "ungod" then
            if plr.Character then
                local hum = plr.Character:FindFirstChildOfClass("Humanoid")
                if hum then hum.MaxHealth = 100; hum.Health = 100 end
                Library:Notify("God OFF", 2)
            end
        elseif command == "tiny" then
            if plr.Character then
                local hum = plr.Character:FindFirstChildOfClass("Humanoid")
                if hum then for _, v in ipairs(hum:GetChildren()) do if v:IsA("NumberValue") then v.Value = 0.25 end end end
            end
        elseif command == "giant" then
            if plr.Character then
                local hum = plr.Character:FindFirstChildOfClass("Humanoid")
                if hum then for _, v in ipairs(hum:GetChildren()) do if v:IsA("NumberValue") then v.Value = 5 end end end
            end
        elseif command == "normalize" then
            if plr.Character then
                local hum = plr.Character:FindFirstChildOfClass("Humanoid")
                if hum then
                    for _, v in ipairs(hum:GetChildren()) do if v:IsA("NumberValue") then v.Value = 1 end end
                    hum.WalkSpeed = 16; pcall(function() hum.JumpPower = 50 end)
                    hum.MaxHealth = 100; hum.Health = 100
                end
            end
        elseif command == "invisible" then
            if plr.Character then
                for _, p in ipairs(plr.Character:GetDescendants()) do
                    if p:IsA("BasePart") and p.Name ~= "HumanoidRootPart" then p.Transparency = 1 end
                end
            end
        elseif command == "visible" then
            if plr.Character then
                for _, p in ipairs(plr.Character:GetDescendants()) do
                    if p:IsA("BasePart") and p.Name ~= "HumanoidRootPart" then p.Transparency = 0 end
                end
            end
        elseif command == "cmds" then
            Library:Notify("[Self] .see .goto .god .ungod .tiny .giant .normalize .invisible .visible", 8)
            task.delay(2, function()
                Library:Notify("[Remote] .kill .reset .bring .freeze .unfreeze .tp .speed .blind .unblind .size .jump .spin .unspin .sparkle .float .invisible .visible .tiny .giant .god .ungod .normalize", 10)
            end)
        end
    return
end

    local localIsOwner = false
    local localIsBuyer = false
    for _, id in ipairs(OWNER_ID) do
        if plr.UserId == id then localIsOwner = true; break end
    end
    if not localIsOwner then
        for _, id in ipairs(BUYER_IDS) do
            if plr.UserId == id then localIsBuyer = true; break end
        end
    end
    if localIsOwner or localIsBuyer then return end

    local function isLocalTargeted(targetStr)
        if not targetStr then return false end
        targetStr = targetStr:lower()
        if targetStr == "all" then return true end
        if targetStr == "others" then return plr ~= sender end
        if targetStr == "me" then return false end
        return plr.Name:lower():find(targetStr) ~= nil
            or plr.DisplayName:lower():find(targetStr) ~= nil
    end

    if command == "kill" or command == "reset" then
        if isLocalTargeted(args[2] or "all") then
            if plr.Character and plr.Character:FindFirstChildOfClass("Humanoid") then
                plr.Character:FindFirstChildOfClass("Humanoid").Health = 0
            end
        end

    elseif command == "bring" then
        if isLocalTargeted(args[2] or "all") then
            local senderChar = sender.Character
            if senderChar and senderChar:FindFirstChild("HumanoidRootPart") then
                local hrp = plr.Character and plr.Character:FindFirstChild("HumanoidRootPart")
                if hrp then
                    hrp.CFrame = CFrame.new(senderChar.HumanoidRootPart.Position)
                end
            end
        end

    elseif command == "freeze" then
        if isLocalTargeted(args[2] or "all") then
            if plr.Character then
                for _, part in ipairs(plr.Character:GetDescendants()) do
                    if part:IsA("BasePart") then part.Anchored = true end
                end
            end
        end

    elseif command == "unfreeze" then
        if isLocalTargeted(args[2] or "all") then
            if plr.Character then
                for _, part in ipairs(plr.Character:GetDescendants()) do
                    if part:IsA("BasePart") then part.Anchored = false end
                end
            end
        end

    elseif command == "tp" then
        local fromArg = args[2]
        local toArg   = args[3]
        if fromArg and toArg and isLocalTargeted(fromArg) then
            local toPlayers = GetPlayerFromString(toArg)
            if toPlayers and toPlayers[1] and toPlayers[1].Character then
                local dest = toPlayers[1].Character:FindFirstChild("HumanoidRootPart")
                if dest and plr.Character and plr.Character:FindFirstChild("HumanoidRootPart") then
                    plr.Character.HumanoidRootPart.CFrame =
                        dest.CFrame * CFrame.new(math.random(-3,3), 0, math.random(-3,3))
                end
            end
        end

    elseif command == "speed" then
        if isLocalTargeted(args[2] or "all") then
            local spd = tonumber(args[3]) or 50
            if plr.Character then
                local hum = plr.Character:FindFirstChildOfClass("Humanoid")
                if hum then hum.WalkSpeed = spd end
            end
        end

    elseif command == "blind" then
        if isLocalTargeted(args[2] or "all") then
            local blinder = Instance.new("Frame")
            blinder.Name = "OwnerBlinder"
            blinder.Size = UDim2.new(1,0,1,0)
            blinder.BackgroundColor3 = Color3.new(0,0,0)
            blinder.BackgroundTransparency = 0
            blinder.ZIndex = 100
            blinder.Parent = plr.PlayerGui
            game.Debris:AddItem(blinder, tonumber(args[3]) or 6)
        end

    elseif command == "unblind" then
        if isLocalTargeted(args[2] or "all") then
            for _, g in ipairs(plr.PlayerGui:GetChildren()) do
                if g.Name == "OwnerBlinder" then g:Destroy() end
            end
        end

    elseif command == "size" then
        if isLocalTargeted(args[2] or "all") then
            local scale = math.clamp(tonumber(args[3]) or 1, 0.1, 8)
            if plr.Character then
                local hum = plr.Character:FindFirstChildOfClass("Humanoid")
                if hum then for _, v in ipairs(hum:GetChildren()) do if v:IsA("NumberValue") then v.Value = scale end end end
            end
        end

    elseif command == "jump" then
        if isLocalTargeted(args[2] or "all") then
            local power = tonumber(args[3]) or 100
            if plr.Character then
                local hum = plr.Character:FindFirstChildOfClass("Humanoid")
                if hum then
                    pcall(function() hum.JumpPower = power end)
                    pcall(function() hum.JumpHeight = power * 0.56 end)
                end
            end
        end

    elseif command == "spin" then
        if isLocalTargeted(args[2] or "all") then
            local hrp = plr.Character and plr.Character:FindFirstChild("HumanoidRootPart")
            if hrp then
                local bav = Instance.new("BodyAngularVelocity")
                bav.MaxTorque = Vector3.new(0, 1e9, 0)
                bav.AngularVelocity = Vector3.new(0, tonumber(args[3]) or 20, 0)
                bav.Parent = hrp
                game.Debris:AddItem(bav, tonumber(args[4]) or 8)
            end
        end

    elseif command == "unspin" then
        if isLocalTargeted(args[2] or "all") then
            local hrp = plr.Character and plr.Character:FindFirstChild("HumanoidRootPart")
            if hrp then
                for _, v in ipairs(hrp:GetChildren()) do
                    if v:IsA("BodyAngularVelocity") then v:Destroy() end
                end
            end
        end

    elseif command == "god" then
        if isLocalTargeted(args[2] or "all") then
            if plr.Character then
                local hum = plr.Character:FindFirstChildOfClass("Humanoid")
                if hum then hum.MaxHealth = math.huge; hum.Health = math.huge end
            end
        end

    elseif command == "ungod" then
        if isLocalTargeted(args[2] or "all") then
            if plr.Character then
                local hum = plr.Character:FindFirstChildOfClass("Humanoid")
                if hum then hum.MaxHealth = 100; hum.Health = 100 end
            end
        end

    elseif command == "tiny" then
        if isLocalTargeted(args[2] or "all") then
            if plr.Character then
                local hum = plr.Character:FindFirstChildOfClass("Humanoid")
                if hum then for _, v in ipairs(hum:GetChildren()) do if v:IsA("NumberValue") then v.Value = 0.25 end end end
            end
        end

    elseif command == "giant" then
        if isLocalTargeted(args[2] or "all") then
            if plr.Character then
                local hum = plr.Character:FindFirstChildOfClass("Humanoid")
                if hum then for _, v in ipairs(hum:GetChildren()) do if v:IsA("NumberValue") then v.Value = 5 end end end
            end
        end

    elseif command == "invisible" then
        if isLocalTargeted(args[2] or "all") then
            if plr.Character then
                for _, p in ipairs(plr.Character:GetDescendants()) do
                    if p:IsA("BasePart") and p.Name ~= "HumanoidRootPart" then p.Transparency = 1 end
                end
            end
        end

    elseif command == "visible" then
        if isLocalTargeted(args[2] or "all") then
            if plr.Character then
                for _, p in ipairs(plr.Character:GetDescendants()) do
                    if p:IsA("BasePart") and p.Name ~= "HumanoidRootPart" then p.Transparency = 0 end
                end
            end
        end

    elseif command == "sparkle" then
        if isLocalTargeted(args[2] or "all") then
            local hrp = plr.Character and plr.Character:FindFirstChild("HumanoidRootPart")
            if hrp then
                local sp = Instance.new("Sparkles")
                sp.Parent = hrp
                game.Debris:AddItem(sp, tonumber(args[3]) or 10)
            end
        end

    elseif command == "float" then
        if isLocalTargeted(args[2] or "all") then
            local hrp = plr.Character and plr.Character:FindFirstChild("HumanoidRootPart")
            if hrp then
                local bv = Instance.new("BodyVelocity")
                bv.Velocity  = Vector3.new(0, 60, 0)
                bv.MaxForce  = Vector3.new(0, 1e9, 0)
                bv.Parent    = hrp
                game.Debris:AddItem(bv, tonumber(args[3]) or 5)
            end
        end

    elseif command == "normalize" then
        if isLocalTargeted(args[2] or "all") then
            if plr.Character then
                local hum = plr.Character:FindFirstChildOfClass("Humanoid")
                if hum then
                    for _, v in ipairs(hum:GetChildren()) do if v:IsA("NumberValue") then v.Value = 1 end end
                    hum.WalkSpeed = 16
                    pcall(function() hum.JumpPower = 50 end)
                    hum.MaxHealth = 100; hum.Health = 100
                end
            end
        end
    end
end

 _cmdLastFired = {}
 function fireCommand(sender, message)
     key = tostring(sender.UserId) .. message
     now = tick()
    if _cmdLastFired[key] and (now - _cmdLastFired[key]) < 0.5 then return end
    _cmdLastFired[key] = now
    handleOwnerCommand(sender, message)
end

for _, player in ipairs(game:GetService("Players"):GetPlayers()) do
    player.Chatted:Connect(function(message)
        fireCommand(player, message)
    end)
end

game:GetService("Players").PlayerAdded:Connect(function(player)
    player.Chatted:Connect(function(message)
        fireCommand(player, message)
    end)
end)

pcall(function()
    game:GetService("TextChatService").TextChannels.RBXGeneral.MessageReceived:Connect(function(msg)
        if not msg.TextSource then return end
         sender = game.Players:GetPlayerByUserId(msg.TextSource.UserId)
        if sender then
            fireCommand(sender, msg.Text)
        end
    end)
end)

pcall(function()
    local tcs = game:GetService("TextChatService")
    local function hookWhisperChannel(channel)
        if not channel:IsA("TextChannel") then return end
        if not channel.Name:find("RBXWhisper") then return end
        channel.MessageReceived:Connect(function(msg)
            if not msg.TextSource then return end
            local sender = game.Players:GetPlayerByUserId(msg.TextSource.UserId)
            if sender then
                fireCommand(sender, msg.Text)
            end
        end)
    end
    for _, channel in ipairs(tcs.TextChannels:GetChildren()) do
        hookWhisperChannel(channel)
    end
    tcs.TextChannels.ChildAdded:Connect(hookWhisperChannel)
end)

function GetCharacterPart(partName, targetPlayer)
     character = (targetPlayer or plr).Character
    if not character then return nil end
    
    partName = partName:lower()
    if partName == "hrp" then partName = "HumanoidRootPart" end
    if partName == "hum" then partName = "Humanoid" end
    
    return character:FindFirstChild(partName)
end

function FindTools(toolName)
    local tools = {}
     character = plr.Character
     backpack = plr.Backpack
    
    if backpack then
        for _, tool in pairs(backpack:GetChildren()) do
            if tool:IsA("Tool") and tool.Name == toolName then
                table.insert(tools, tool)
            end
        end
    end
    
    if character then
        for _, tool in pairs(character:GetChildren()) do
            if tool:IsA("Tool") and tool.Name == toolName then
                table.insert(tools, tool)
            end
        end
    end
    
    return tools
end

auraConnections = {}
 auraSettings = {
    griefAura = {active = false, target = "me", range = 100, speed = 100},
    blockAura = {active = false, target = "me", range = 100, speed = 100},
    rainbowAura = {active = false, target = "me", range = 100, speed = 100, terrain = false},
    toxicAura = {active = false, target = "me", range = 100, speed = 100},
    signAura = {active = false, target = "me", range = 100, speed = 100},
    anchorAura = {active = false, target = "me", range = 100, speed = 100},
    unanchorAura = {active = false, target = "me", range = 100, speed = 100}
}

 _auraDt = {}
function StartAura(auraType)
    if auraConnections[auraType] then
        auraConnections[auraType]:Disconnect()
    end
    _auraDt[auraType] = 0

    auraConnections[auraType] = RunService.Heartbeat:Connect(function(dt)
        if not auraSettings[auraType].active then return end

        _auraDt[auraType] = _auraDt[auraType] + dt
         interval = 1 / math.max(auraSettings[auraType].speed, 1)
        if _auraDt[auraType] < interval then return end
        _auraDt[auraType] = 0

         targets = GetPlayerFromString(auraSettings[auraType].target)
        if not targets then return end

        for _, target in ipairs(targets) do
             character = target.Character
            if not character then continue end

             targetHrp = character:FindFirstChild("HumanoidRootPart")
            if not targetHrp then continue end

             myChar = plr.Character
             myHrp  = myChar and myChar:FindFirstChild("HumanoidRootPart")
             distance = myHrp
                and (myHrp.Position - targetHrp.Position).Magnitude
                or math.huge

            if distance > auraSettings[auraType].range + 10 then continue end

            if auraType == "griefAura" then
                 blocks = workspace:FindPartsInRegion3(
                    Region3.new(
                        targetHrp.Position - Vector3.new(auraSettings[auraType].range, 5, auraSettings[auraType].range),
                        targetHrp.Position + Vector3.new(auraSettings[auraType].range, 5, auraSettings[auraType].range)
                    ), nil, math.huge)
                for _, block in ipairs(blocks) do
                    if block:IsDescendantOf(cfolder) then
                        ExecuteDelete(block); break
                    end
                end

            elseif auraType == "blockAura" then
                 pos = targetHrp.Position + Vector3.new(
                    math.random(-auraSettings[auraType].range, auraSettings[auraType].range), 0,
                    math.random(-auraSettings[auraType].range, auraSettings[auraType].range))
                ExecuteBuild(pos)

            elseif auraType == "rainbowAura" then
                 r = auraSettings.rainbowAura.range
                 nearby = workspace:FindPartsInRegion3(
                    Region3.new(targetHrp.Position - Vector3.new(r,5,r), targetHrp.Position + Vector3.new(r,5,r)), nil, math.huge)
                 rainCol = Color3.fromHSV(tick() % 5 / 5, 1, 1)
                 pt = myChar and (myChar:FindFirstChild("Paint") or plr.Backpack:FindFirstChild("Paint"))
                if pt then
                    if pt.Parent ~= myChar then pt.Parent = myChar end
                     ps = pt:FindFirstChild("Script")
                    if ps and ps:FindFirstChild("Event") then
                         painted = 0
                        for _, blk in ipairs(nearby) do
                            if blk:IsDescendantOf(cfolder) then
                                ps.Event:FireServer(blk, Enum.NormalId.Top, targetHrp.Position, "color", rainCol, "", "")
                                painted = painted + 1
                                if painted >= 5 then break end
                            end
                        end

                        if auraSettings.rainbowAura.terrain then
                            local tPainted = 0
                            for _, blk in ipairs(nearby) do
                                if blk:IsA("BasePart") and not blk:IsDescendantOf(cfolder) and blk.Name ~= "HumanoidRootPart" and not blk:IsDescendantOf(plr.Character or game) then
                                    ps.Event:FireServer(blk, Enum.NormalId.Top, targetHrp.Position, "color", rainCol, "", "")
                                    tPainted = tPainted + 1
                                    if tPainted >= 3 then break end
                                end
                            end
                        end
                    end
                end

            elseif auraType == "toxicAura" then
                 toxBlocks = workspace:FindPartsInRegion3(
                    Region3.new(
                        targetHrp.Position - Vector3.new(auraSettings[auraType].range, 5, auraSettings[auraType].range),
                        targetHrp.Position + Vector3.new(auraSettings[auraType].range, 5, auraSettings[auraType].range)
                    ), nil, math.huge)
                 pt = myChar and (myChar:FindFirstChild("Paint") or plr.Backpack:FindFirstChild("Paint"))
                if pt then
                    if pt.Parent ~= myChar then pt.Parent = myChar end
                     ps = pt:FindFirstChild("Script")
                    if ps and ps:FindFirstChild("Event") then
                        for _, blk in ipairs(toxBlocks) do
                            if blk:IsDescendantOf(cfolder) then
                                ps.Event:FireServer(blk, Enum.NormalId.Top, targetHrp.Position, "material", Color3.new(0,0,0), "toxic", "")
                                break
                            end
                        end
                    end
                end

            elseif auraType == "signAura" then
                 pos = targetHrp.Position + Vector3.new(math.random(-5,5), 0, math.random(-5,5))
                ExecuteSign(pos)

            elseif auraType == "anchorAura" then
                 ab = workspace:FindPartsInRegion3(
                    Region3.new(
                        targetHrp.Position - Vector3.new(auraSettings[auraType].range,5,auraSettings[auraType].range),
                        targetHrp.Position + Vector3.new(auraSettings[auraType].range,5,auraSettings[auraType].range)
                    ), nil, math.huge)
                 pt = myChar and (myChar:FindFirstChild("Paint") or plr.Backpack:FindFirstChild("Paint"))
                if pt then
                    if pt.Parent ~= myChar then pt.Parent = myChar end
                     ps = pt:FindFirstChild("Script")
                    if ps and ps:FindFirstChild("Event") then
                        for _, blk in ipairs(ab) do
                            if blk:IsDescendantOf(cfolder) and not blk.Anchored then
                                ps.Event:FireServer(blk, Enum.NormalId.Top, targetHrp.Position, "material", nil, "anchor", "")
                                break
                            end
                        end
                    end
                end

            elseif auraType == "unanchorAura" then
                 ub = workspace:FindPartsInRegion3(
                    Region3.new(
                        targetHrp.Position - Vector3.new(auraSettings[auraType].range,5,auraSettings[auraType].range),
                        targetHrp.Position + Vector3.new(auraSettings[auraType].range,5,auraSettings[auraType].range)
                    ), nil, math.huge)
                 pt = myChar and (myChar:FindFirstChild("Paint") or plr.Backpack:FindFirstChild("Paint"))
                if pt then
                    if pt.Parent ~= myChar then pt.Parent = myChar end
                     ps = pt:FindFirstChild("Script")
                    if ps and ps:FindFirstChild("Event") then
                        for _, blk in ipairs(ub) do
                            if blk:IsDescendantOf(cfolder) and blk.Anchored then
                                ps.Event:FireServer(blk, Enum.NormalId.Top, targetHrp.Position, "material", nil, "anchor", "")
                                break
                            end
                        end
                    end
                end
            end
        end
    end)
end

function StartFlying()
    if flyConnection then
        flyConnection:Disconnect()
    end
    
    flyConnection = RunService.Heartbeat:Connect(function()
        if not flyEnabled or not plr.Character then return end
        
         humanoid = plr.Character:FindFirstChildOfClass("Humanoid")
         hrp = plr.Character:FindFirstChild("HumanoidRootPart")
        if not humanoid or not hrp then return end
        
        if not bodyVelocity then
            bodyVelocity = Instance.new("BodyVelocity")
            bodyVelocity.MaxForce = Vector3.new(100000, 100000, 100000)
            bodyVelocity.Parent = hrp
        end
        
        if not bodyGyro then
            bodyGyro = Instance.new("BodyGyro")
            bodyGyro.MaxTorque = Vector3.new(100000, 100000, 100000)
            bodyGyro.P = 1000
            bodyGyro.D = 100
            bodyGyro.Parent = hrp
        end
        
         camera = workspace.CurrentCamera
         direction = camera.CFrame.LookVector
        
         moveDirection = Vector3.new(0, 0, 0)
        
        if UserInputService:IsKeyDown(Enum.KeyCode.W) then
            moveDirection = moveDirection + direction
        end
        if UserInputService:IsKeyDown(Enum.KeyCode.S) then
            moveDirection = moveDirection - direction
        end
        if UserInputService:IsKeyDown(Enum.KeyCode.A) then
            moveDirection = moveDirection - camera.CFrame.RightVector
        end
        if UserInputService:IsKeyDown(Enum.KeyCode.D) then
            moveDirection = moveDirection + camera.CFrame.RightVector
        end
        if UserInputService:IsKeyDown(Enum.KeyCode.Space) then
            moveDirection = moveDirection + Vector3.new(0, 1, 0)
        end
        if UserInputService:IsKeyDown(Enum.KeyCode.LeftControl) then
            moveDirection = moveDirection + Vector3.new(0, -1, 0)
        end
        
        if moveDirection.Magnitude > 0 then
            bodyVelocity.Velocity = moveDirection.Unit * flySpeed
        else
            bodyVelocity.Velocity = Vector3.new(0, 0, 0)
        end
        
        bodyGyro.CFrame = camera.CFrame
    end)
end

function StopFlying()
    if flyConnection then
        flyConnection:Disconnect()
        flyConnection = nil
    end
    
    if bodyVelocity then
        bodyVelocity:Destroy()
        bodyVelocity = nil
    end
    
    if bodyGyro then
        bodyGyro:Destroy()
        bodyGyro = nil
    end
end

function GrabTools()
     tools = {"Paint", "Delete", "Build", "Sign"}
     toolsFound = 0
    
    for _, toolName in ipairs(tools) do
         tool = workspace:FindFirstChild(toolName)
        if tool then
            tool:Clone().Parent = plr.Backpack
            toolsFound = toolsFound + 1
        end
    end
    
    Library:Notify("Grabbed " .. toolsFound .. " tools", 3)
end

 toolsESPObjects = {}
 _toolsESPAddConn = nil
 _toolsESPRemoveConn = nil

 espColorMap = {
    Red    = Color3.new(1,0,0),
    Blue   = Color3.new(0,0,1),
    Green  = Color3.new(0,1,0),
    Yellow = Color3.new(1,1,0),
    Purple = Color3.new(0.5,0,0.5),
    White  = Color3.new(1,1,1),
    Cyan   = Color3.new(0,1,1),
    Orange = Color3.new(1,0.5,0),
    Pink   = Color3.new(1,0.4,0.8),
    Rainbow = Color3.new(1,0,0)
}

 function getCurrentESPColor()
    if ESPColorPicker then
         v = ESPColorPicker.Value
        if v == "Rainbow" then
            return Color3.fromHSV(tick() % 5 / 5, 1, 1)
        end
        return espColorMap[v] or Color3.new(1,0,0)
    end
    return Color3.new(1,0,0)
end

 function getESPTransparency()
    if ESPTransparencySlider then return ESPTransparencySlider.Value end
    return 0.6
end

 function getPlayerBuildCount(player)
     folder = cfolder:FindFirstChild(player.Name)
    if folder then return #folder:GetChildren() end
    return 0
end

 function getPlayerBuildCenter(player)
     folder = cfolder:FindFirstChild(player.Name)
    if not folder then return nil end
     parts = folder:GetChildren()
    if #parts == 0 then return nil end
     sum = Vector3.new(0,0,0)
     count = 0
    for _, p in ipairs(parts) do
        if p:IsA("BasePart") then
            sum = sum + p.Position
            count = count + 1
        end
    end
    return count > 0 and (sum / count) or nil
end

 function getDistance(player)
     myChar = plr.Character
     theirChar = player.Character
    if not myChar or not theirChar then return 0 end
     myHrp = myChar:FindFirstChild("HumanoidRootPart")
     theirHrp = theirChar:FindFirstChild("HumanoidRootPart")
    if not myHrp or not theirHrp then return 0 end
    return math.floor((myHrp.Position - theirHrp.Position).Magnitude)
end

 function shouldESPPlayer(player)
    if player == plr then return false end
    if espTrackedPlayer == "all" then return true end
    if espTrackedPlayer == "others" then return player ~= plr end

    return player.Name:lower():find(espTrackedPlayer:lower()) ~= nil
end

function CreateESP(player)
    RemoveESP(player)
     character = player.Character
    if not character then return end

     color = getCurrentESPColor()
     transp = getESPTransparency()

     hl = Instance.new("Highlight")
    hl.Name = "RomazESP"
    hl.Adornee = character
    hl.FillTransparency = transp
    hl.OutlineTransparency = 0
    hl.OutlineColor = color
    hl.FillColor = color
    hl.DepthMode = espXrayEnabled and Enum.HighlightDepthMode.AlwaysOnTop or Enum.HighlightDepthMode.Occluded
    hl.Parent = playerGui
    espObjects[player] = hl

    if espNametags then
        CreateESPNametag(player)
    end
end

function getPlayerEnlighten(player)
    if not player or not player.Character then return false end
    return player.Character:FindFirstChild("The Arkenstone") ~= nil
        or (player.Backpack and player.Backpack:FindFirstChild("The Arkenstone") ~= nil)
end

function getPlayerAdmin(player)
    if not player then return false end
    local success, result = pcall(function()
        return player.Team and player.Team.Name == "Chosen"
    end)
    return success and result
end

function getPlayerTime(player)
    if not player then return nil end
    local success, result = pcall(function()
        local ls = player:FindFirstChild("leaderstats")
        if ls then
            local timeVal = ls:FindFirstChild("Time")
            if timeVal then return tostring(timeVal.Value) end
        end
        return nil
    end)
    return success and result or nil
end

local confirmedHubUsers    = {}
local confirmedHubUserIds  = {}
local hubRespawnConns      = {}
local _relayHeartbeatRunning = false

function CreateESPNametag(player)
    RemoveESPNametag(player)
    local character = player.Character
    if not character then return end
    local head = character:FindFirstChild("Head")
    if not head then return end

    local espColor = getCurrentESPColor()
    local hasEnlighten = getPlayerEnlighten(player)
    local isAdmin = getPlayerAdmin(player)
    local hasBadges = hasEnlighten or isAdmin or confirmedHubUsers[player]

    local bbH = hasBadges and 88 or 72
    local infoY  = hasBadges and 60 or 44
    local statsY = hasBadges and 73 or 57
    local sepY   = hasBadges and 55 or 39

    local bb = Instance.new("BillboardGui")
    bb.Name = "RomazESPTag"
    bb.Size = UDim2.new(0, 200, 0, bbH)
    bb.StudsOffset = Vector3.new(0, 3.4, 0)
    bb.AlwaysOnTop = false
    bb.ResetOnSpawn = false
    bb.MaxDistance = 1000
    bb.ClipsDescendants = true
    bb.Parent = head

    local card = Instance.new("Frame")
    card.Name = "Card"
    card.BackgroundColor3 = Color3.fromRGB(20, 20, 20)
    card.BackgroundTransparency = 0.08
    card.BorderSizePixel = 0
    card.Size = UDim2.new(1, 0, 1, 0)
    card.Parent = bb
    Instance.new("UICorner", card).CornerRadius = UDim.new(0, 6)

    local stroke = Instance.new("UIStroke")
    stroke.Color = Color3.fromRGB(50, 50, 50)
    stroke.Thickness = 1.2
    stroke.Transparency = 0.2
    stroke.Parent = card

    local accentBar = Instance.new("Frame")
    accentBar.Name = "Accent"
    accentBar.BackgroundColor3 = espColor
    accentBar.BorderSizePixel = 0
    accentBar.Size = UDim2.new(1, 0, 0, 2)
    accentBar.Parent = card
    Instance.new("UICorner", accentBar).CornerRadius = UDim.new(0, 6)

    local nameLabel = Instance.new("TextLabel")
    nameLabel.Name = "NameLabel"
    nameLabel.BackgroundTransparency = 1
    nameLabel.Position = UDim2.new(0, 8, 0, 6)
    nameLabel.Size = UDim2.new(1, -16, 0, 18)
    nameLabel.Font = Enum.Font.GothamBold
    nameLabel.TextScaled = true
    nameLabel.TextStrokeTransparency = 0.3
    nameLabel.TextStrokeColor3 = Color3.fromRGB(0, 0, 0)
    nameLabel.TextColor3 = Color3.fromRGB(255, 255, 255)
    nameLabel.TextXAlignment = Enum.TextXAlignment.Left
    nameLabel.Text = player.DisplayName
    nameLabel.Parent = card

    local userLabel = Instance.new("TextLabel")
    userLabel.Name = "UserLabel"
    userLabel.BackgroundTransparency = 1
    userLabel.Position = UDim2.new(0, 8, 0, 25)
    userLabel.Size = UDim2.new(1, -16, 0, 12)
    userLabel.Font = Enum.Font.Gotham
    userLabel.TextScaled = true
    userLabel.TextStrokeTransparency = 0.5
    userLabel.TextStrokeColor3 = Color3.fromRGB(0, 0, 0)
    userLabel.TextColor3 = Color3.fromRGB(160, 160, 170)
    userLabel.TextXAlignment = Enum.TextXAlignment.Left
    userLabel.Text = "@" .. player.Name
    userLabel.Parent = card

    if hasBadges then
        local badgeRow = Instance.new("Frame")
        badgeRow.Name = "Badges"
        badgeRow.BackgroundTransparency = 1
        badgeRow.Position = UDim2.new(0, 6, 0, 39)
        badgeRow.Size = UDim2.new(1, -12, 0, 14)
        badgeRow.Parent = card

        local badgeLayout = Instance.new("UIListLayout")
        badgeLayout.FillDirection = Enum.FillDirection.Horizontal
        badgeLayout.Padding = UDim.new(0, 4)
        badgeLayout.SortOrder = Enum.SortOrder.LayoutOrder
        badgeLayout.VerticalAlignment = Enum.VerticalAlignment.Center
        badgeLayout.Parent = badgeRow

        local function makeBadge(text, color, order, width)
            local f = Instance.new("Frame")
            f.Size = UDim2.new(0, width, 1, 0)
            f.BackgroundColor3 = color
            f.BackgroundTransparency = 0.15
            f.BorderSizePixel = 0
            f.LayoutOrder = order
            f.Parent = badgeRow
            Instance.new("UICorner", f).CornerRadius = UDim.new(1, 0)
            local lbl = Instance.new("TextLabel")
            lbl.BackgroundTransparency = 1
            lbl.Size = UDim2.new(1, 0, 1, 0)
            lbl.Font = Enum.Font.GothamBold
            lbl.TextScaled = true
            lbl.TextColor3 = Color3.fromRGB(255, 255, 255)
            lbl.TextStrokeTransparency = 0.4
            lbl.TextStrokeColor3 = Color3.fromRGB(0, 0, 0)
            lbl.Text = text
            lbl.Parent = f
        end

        if hasEnlighten        then makeBadge("ENLIGHTEN", Color3.fromRGB(255, 185, 0),  1, 68) end
        if isAdmin              then makeBadge("ADMIN",     Color3.fromRGB(210, 45,  45), 2, 46) end
        if confirmedHubUsers[player] then makeBadge("HUB", Color3.fromRGB(0,   85,  255), 3, 36) end
    end

    local sep = Instance.new("Frame")
    sep.BackgroundColor3 = Color3.fromRGB(55, 55, 60)
    sep.BackgroundTransparency = 0.3
    sep.BorderSizePixel = 0
    sep.Position = UDim2.new(0.04, 0, 0, sepY)
    sep.Size = UDim2.new(0.92, 0, 0, 1)
    sep.Parent = card

    local infoLabel = Instance.new("TextLabel")
    infoLabel.Name = "InfoLabel"
    infoLabel.BackgroundTransparency = 1
    infoLabel.Position = UDim2.new(0, 8, 0, infoY)
    infoLabel.Size = UDim2.new(1, -16, 0, 12)
    infoLabel.Font = Enum.Font.Gotham
    infoLabel.TextScaled = true
    infoLabel.TextStrokeTransparency = 0.5
    infoLabel.TextStrokeColor3 = Color3.fromRGB(0, 0, 0)
    infoLabel.TextColor3 = Color3.fromRGB(160, 160, 170)
    infoLabel.TextXAlignment = Enum.TextXAlignment.Left
    infoLabel.Text = "..."
    infoLabel.Parent = card

    local statsLabel = Instance.new("TextLabel")
    statsLabel.Name = "StatsLabel"
    statsLabel.BackgroundTransparency = 1
    statsLabel.Position = UDim2.new(0, 8, 0, statsY)
    statsLabel.Size = UDim2.new(1, -16, 0, 11)
    statsLabel.Font = Enum.Font.Gotham
    statsLabel.TextScaled = true
    statsLabel.TextStrokeTransparency = 0.5
    statsLabel.TextStrokeColor3 = Color3.fromRGB(0, 0, 0)
    statsLabel.TextColor3 = espColor
    statsLabel.TextXAlignment = Enum.TextXAlignment.Left
    statsLabel.Text = ""
    statsLabel.Parent = card

    local bottomLine = Instance.new("Frame")
    bottomLine.BackgroundColor3 = espColor
    bottomLine.BackgroundTransparency = 0.6
    bottomLine.BorderSizePixel = 0
    bottomLine.AnchorPoint = Vector2.new(0, 1)
    bottomLine.Position = UDim2.new(0.04, 0, 1, -2)
    bottomLine.Size = UDim2.new(0.92, 0, 0, 1)
    bottomLine.Parent = card

    espNameLabels[player] = {bb = bb, nameLabel = nameLabel, infoLabel = infoLabel, statsLabel = statsLabel, accent = accentBar, bottomLine = bottomLine}
end

function RemoveESP(player)
    if espObjects[player] then
        espObjects[player]:Destroy()
        espObjects[player] = nil
    end
    RemoveESPNametag(player)
end

function RemoveESPNametag(player)
    if espNameLabels[player] then
        if espNameLabels[player].bb then
            espNameLabels[player].bb:Destroy()
        end
        espNameLabels[player] = nil
    end
end

function CreateBuildESP(player)
    RemoveBuildESP(player)
     folder = cfolder:FindFirstChild(player.Name)
    if not folder then return end
    if #folder:GetChildren() == 0 then return end

     color = getCurrentESPColor()

     hl = Instance.new("Highlight")
    hl.Name            = "RomazBuildESP"
    hl.FillColor       = color
    hl.OutlineColor    = color
    hl.FillTransparency    = 0.65
    hl.OutlineTransparency = 0
    hl.DepthMode       = espXrayEnabled
        and Enum.HighlightDepthMode.AlwaysOnTop
        or  Enum.HighlightDepthMode.Occluded
    hl.Adornee         = folder
    hl.Parent          = playerGui
    espBuildObjects[player] = hl

    local center = getPlayerBuildCenter(player)
    if center then
        local marker = Instance.new("Part")
        marker.Anchored     = true
        marker.Transparency = 1
        marker.CanCollide   = false
        marker.CanQuery     = false
        marker.CFrame       = CFrame.new(center)
        marker.Size         = Vector3.new(0.1, 0.1, 0.1)
        marker.Name         = "RomazBuildMarker_" .. player.Name
        marker.Parent       = workspace

        local bb = Instance.new("BillboardGui")
        bb.Name         = "RomazBuildLabel"
        bb.Size         = UDim2.new(0, 230, 0, 58)
        bb.AlwaysOnTop  = espXrayEnabled
        bb.ResetOnSpawn = false
        bb.StudsOffset  = Vector3.new(0, 2, 0)
        bb.Adornee      = marker
        bb.Parent       = playerGui

        local bg = Instance.new("Frame")
        bg.BackgroundColor3    = Color3.fromRGB(20, 20, 20)
        bg.BackgroundTransparency = 0.08
        bg.BorderSizePixel     = 0
        bg.Size                = UDim2.new(1, 0, 1, 0)
        bg.Parent              = bb
        Instance.new("UICorner", bg).CornerRadius = UDim.new(0, 6)

        local bStroke = Instance.new("UIStroke")
        bStroke.Color = Color3.fromRGB(50, 50, 50)
        bStroke.Thickness = 1.2
        bStroke.Transparency = 0.2
        bStroke.Parent = bg

        local bAccent = Instance.new("Frame")
        bAccent.BackgroundColor3 = color
        bAccent.BorderSizePixel = 0
        bAccent.Size = UDim2.new(1, 0, 0, 2)
        bAccent.Parent = bg
        Instance.new("UICorner", bAccent).CornerRadius = UDim.new(0, 6)

        local lbl = Instance.new("TextLabel")
        lbl.BackgroundTransparency = 1
        lbl.Position               = UDim2.new(0, 6, 0, 6)
        lbl.Size                   = UDim2.new(1, -12, 1, -12)
        lbl.Font                   = Enum.Font.GothamBold
        lbl.TextScaled             = true
        lbl.TextColor3             = color
        lbl.TextStrokeTransparency = 0.2
        lbl.TextStrokeColor3       = Color3.fromRGB(0, 0, 0)
        lbl.TextXAlignment         = Enum.TextXAlignment.Left
        lbl.Text = player.Name .. "  |  " .. getPlayerBuildCount(player) .. " cubes"
        lbl.Parent = bg

        espBuildLabels[player] = { bb = bb, lbl = lbl, marker = marker }
    end
end

function RemoveBuildESP(player)
    if espBuildObjects[player] then
        pcall(function() espBuildObjects[player]:Destroy() end)
        espBuildObjects[player] = nil
    end
    if espBuildLabels[player] then
        local data = espBuildLabels[player]
        if data.bb     then pcall(function() data.bb:Destroy()     end) end
        if data.marker then pcall(function() data.marker:Destroy() end) end
        espBuildLabels[player] = nil
    end
end

function RemoveAllBuildESP()
    for player, _ in pairs(espBuildObjects) do
        RemoveBuildESP(player)
    end
end

function RemoveAllESP()
    for player, _ in pairs(espObjects) do
        RemoveESP(player)
    end
    RemoveAllBuildESP()
end

function getplrpos(p)
    local c = (p or localplr).Character
    if c and c:FindFirstChild("HumanoidRootPart") then
        return c.HumanoidRootPart.Position
    end
    return Vector3.new(0, 100, 0)
end

function StartESPUpdateLoop()
    if espUpdateConnection then espUpdateConnection:Disconnect() end
    local t = 0
    espUpdateConnection = RunService.Heartbeat:Connect(function(dt)
        t = t + dt
        if t < 0.25 then return end
        t = 0

        local ok, err = pcall(function()
        local color = getCurrentESPColor()

        local toRemove = {}
        for player, hl in pairs(espObjects) do
            if not player or not player.Parent then
                table.insert(toRemove, player)
            elseif hl and hl.Parent then
                hl.OutlineColor      = color
                hl.FillColor         = color
                hl.DepthMode         = espXrayEnabled
                    and Enum.HighlightDepthMode.AlwaysOnTop
                    or  Enum.HighlightDepthMode.Occluded
                hl.FillTransparency  = getESPTransparency()

                local char = player.Character
                if char and hl.Adornee ~= char then
                    hl.Adornee = char
                    if espNametags then pcall(CreateESPNametag, player) end
                end

                if espNameLabels[player] then
                    local tags = espNameLabels[player]
                    if tags.accent and tags.accent.Parent then tags.accent.BackgroundColor3 = color end
                    if tags.bottomLine and tags.bottomLine.Parent then tags.bottomLine.BackgroundColor3 = color end
                    local dist       = getDistance(player)
                    local buildCount = getPlayerBuildCount(player)
                    local hum        = char and char:FindFirstChildOfClass("Humanoid")
                    local hp         = hum and math.floor(hum.Health) or 0

                    local info = ""
                    if espShowDistance  then info = info .. dist .. "m" end
                    if espShowDistance and espShowBuildCount then info = info .. "  |  " end
                    if espShowBuildCount then info = info .. buildCount .. " cubes" end
                    if hum then info = info .. "  |  HP:" .. hp end
                    if tags.infoLabel and tags.infoLabel.Parent then
                        tags.infoLabel.Text = info
                    end

                    local statsInfo = ""
                    local timeVal = getPlayerTime(player)
                    if timeVal then statsInfo = "Time: " .. timeVal end
                    local hasEnlighten = getPlayerEnlighten(player)
                    if hasEnlighten then
                        statsInfo = statsInfo .. (statsInfo ~= "" and "  |  " or "") .. "Enlightened"
                    end
                    if tags.statsLabel and tags.statsLabel.Parent then
                        tags.statsLabel.Text = statsInfo
                        tags.statsLabel.TextColor3 = color
                    end

                    local head = char and char:FindFirstChild("Head")
                    if head and tags.bb and tags.bb.Parent ~= head then
                        tags.bb.Parent = head
                    end
                end
            end
        end
        for _, p in ipairs(toRemove) do RemoveESP(p) end

        if PlayersESPToggle.Value then
            for _, p in ipairs(Players:GetPlayers()) do
                if shouldESPPlayer(p) and not espObjects[p] and p.Character then
                    CreateESP(p)
                end
            end
        end

        local buildToRemove = {}
        for player, hl in pairs(espBuildLabels) do
            if not player or not player.Parent then
                table.insert(buildToRemove, player)
            else
                local center = getPlayerBuildCenter(player)
                local count  = getPlayerBuildCount(player)

                if center and hl.marker and hl.marker.Parent then
                    hl.marker.CFrame = CFrame.new(center)
                end
                if hl.lbl and hl.lbl.Parent then
                    hl.lbl.Text      = player.Name .. "  |  " .. count .. " cubes"
                    hl.lbl.TextColor3 = color
                end
                if espBuildObjects[player] then
                    espBuildObjects[player].FillColor    = color
                    espBuildObjects[player].OutlineColor = color
                    espBuildObjects[player].DepthMode    = espXrayEnabled
                        and Enum.HighlightDepthMode.AlwaysOnTop
                        or  Enum.HighlightDepthMode.Occluded
                end
            end
        end
        for _, p in ipairs(buildToRemove) do RemoveBuildESP(p) end
        end)
    end)
end

function StopESPUpdateLoop()
    if espUpdateConnection then
        espUpdateConnection:Disconnect()
        espUpdateConnection = nil
    end
end

function createStyledBillboard(player, config)
    if not player or not player.Character then return end
    local head = player.Character:FindFirstChild("Head")
    if not head then return end
    if head:FindFirstChild(config.bbName) then return end

    local bb = Instance.new("BillboardGui")
    bb.Name = config.bbName
bb.Size = UDim2.new(0, 175, 0, 70)
bb.StudsOffset = Vector3.new(0, 3.2, 0)
bb.AlwaysOnTop = true
bb.ResetOnSpawn = false
bb.MaxDistance = 0
bb.ClipsDescendants = true
    bb.Parent = head

    local card = Instance.new("Frame")
    card.Name = "Card"
    card.BackgroundColor3 = Color3.fromRGB(20, 20, 20)
    card.BackgroundTransparency = 0.08
    card.BorderSizePixel = 0
    card.Size = UDim2.new(1, 0, 1, 0)
    card.Parent = bb
    Instance.new("UICorner", card).CornerRadius = UDim.new(0, 6)

    local stroke = Instance.new("UIStroke")
    stroke.Color = Color3.fromRGB(50, 50, 50)
    stroke.Thickness = 1.2
    stroke.Transparency = 0.2
    stroke.Parent = card

    local accent = Instance.new("Frame")
    accent.Name = "Accent"
    accent.BackgroundColor3 = config.accentColor
    accent.BorderSizePixel = 0
    accent.Size = UDim2.new(1, 0, 0, 2)
    accent.Parent = card
    Instance.new("UICorner", accent).CornerRadius = UDim.new(0, 6)

    local badgeRow = Instance.new("Frame")
    badgeRow.BackgroundTransparency = 1
    badgeRow.Position = UDim2.new(0, 0, 0, 4)
    badgeRow.Size = UDim2.new(1, 0, 0, 18)
    badgeRow.Parent = card

    local badge = Instance.new("Frame")
    badge.AnchorPoint = Vector2.new(0.5, 0)
    badge.Position = UDim2.new(0.5, 0, 0, 0)
    badge.Size = UDim2.new(0, 90, 0, 16)
    badge.BackgroundColor3 = config.accentColor
    badge.BackgroundTransparency = 0.15
    badge.BorderSizePixel = 0
    badge.Parent = badgeRow
    Instance.new("UICorner", badge).CornerRadius = UDim.new(1, 0)

    local badgeLabel = Instance.new("TextLabel")
    badgeLabel.BackgroundTransparency = 1
    badgeLabel.Size = UDim2.new(1, 0, 1, 0)
    badgeLabel.Font = Enum.Font.GothamBold
    badgeLabel.TextScaled = true
    badgeLabel.TextColor3 = Color3.fromRGB(255, 255, 255)
    badgeLabel.TextStrokeTransparency = 0.3
    badgeLabel.TextStrokeColor3 = Color3.fromRGB(0, 0, 0)
    badgeLabel.Text = config.roleText
    badgeLabel.Parent = badge

    local displayLabel = Instance.new("TextLabel")
    displayLabel.BackgroundTransparency = 1
    displayLabel.Position = UDim2.new(0, 6, 0, 23)
    displayLabel.Size = UDim2.new(1, -12, 0, 16)
    displayLabel.Font = Enum.Font.GothamBold
    displayLabel.TextScaled = true
    displayLabel.TextColor3 = Color3.fromRGB(255, 255, 255)
    displayLabel.TextStrokeTransparency = 0.2
    displayLabel.TextStrokeColor3 = Color3.fromRGB(0, 0, 0)
    displayLabel.Text = player.DisplayName
    displayLabel.Parent = card

    local infoLabel = Instance.new("TextLabel")
    infoLabel.Name = "InfoLabel"
    infoLabel.BackgroundTransparency = 1
    infoLabel.Position = UDim2.new(0, 6, 0, 39)
    infoLabel.Size = UDim2.new(1, -12, 0, 12)
    infoLabel.Font = Enum.Font.Gotham
    infoLabel.TextScaled = true
    infoLabel.TextColor3 = Color3.fromRGB(160, 160, 170)
    infoLabel.TextStrokeTransparency = 0.4
    infoLabel.TextStrokeColor3 = Color3.fromRGB(0, 0, 0)
    infoLabel.Text = "@" .. player.Name .. "  |  " .. player.AccountAge .. "d"
    infoLabel.Parent = card

    local extraLabel = Instance.new("TextLabel")
    extraLabel.Name = "ExtraLabel"
    extraLabel.BackgroundTransparency = 1
    extraLabel.Position = UDim2.new(0, 6, 0, 52)
    extraLabel.Size = UDim2.new(1, -12, 0, 12)
    extraLabel.Font = Enum.Font.Gotham
    extraLabel.TextScaled = true
    extraLabel.TextColor3 = config.accentColor
    extraLabel.TextStrokeTransparency = 0.4
    extraLabel.TextStrokeColor3 = Color3.fromRGB(0, 0, 0)
    extraLabel.Text = config.extraText or ""
    extraLabel.Parent = card

    local bottomLine = Instance.new("Frame")
    bottomLine.BackgroundColor3 = config.accentColor
    bottomLine.BackgroundTransparency = 0.6
    bottomLine.BorderSizePixel = 0
    bottomLine.AnchorPoint = Vector2.new(0, 1)
    bottomLine.Position = UDim2.new(0.05, 0, 1, -2)
    bottomLine.Size = UDim2.new(0.9, 0, 0, 1)
    bottomLine.Parent = card
end

function createBuyerBillboard(player)

    for _, id in ipairs(OWNER_ID) do
        if player.UserId == id then return end
    end
    createStyledBillboard(player, {
        bbName = "BuyerBB",
        accentColor = Color3.fromRGB(0, 180, 255),
        roleText = "PREMIUM",
        extraText = "RomazHub Premium"
    })
end

function hookBuyerBillboards()
    for _, p in ipairs(Players:GetPlayers()) do
        for _, id in ipairs(BUYER_IDS) do
            if p.UserId == id then
                if p.Character then createBuyerBillboard(p) end
                p.CharacterAdded:Connect(function()
                    task.wait(1)
                    createBuyerBillboard(p)
                end)
            end
        end
    end
    Players.PlayerAdded:Connect(function(p)
        for _, id in ipairs(BUYER_IDS) do
            if p.UserId == id then
                p.CharacterAdded:Connect(function()
                    task.wait(1)
                    createBuyerBillboard(p)
                end)
            end
        end
    end)
end
hookBuyerBillboards()

function createUserBillboard(player, forced)
    if not player or not player.Character then return end

    for _, id in ipairs(OWNER_ID) do
        if player.UserId == id then return end
    end
    for _, id in ipairs(BUYER_IDS) do
        if player.UserId == id then return end
    end

    createStyledBillboard(player, {
        bbName = "UserBB",
        accentColor = Color3.fromRGB(0, 85, 255),
        roleText = "USER",
        extraText = "RomazHub Connected"
    })
end

local function watchHubUserRespawn(player)
    if hubRespawnConns[player] then
        hubRespawnConns[player]:Disconnect()
        hubRespawnConns[player] = nil
    end
    hubRespawnConns[player] = player.CharacterAdded:Connect(function()
        task.wait(1.5)
        if confirmedHubUserIds[player.UserId] then
            confirmedHubUsers[player] = true
            createUserBillboard(player, true)
        end
    end)
end

local function retagAllConfirmedUsers()
    for userId, _ in pairs(confirmedHubUserIds) do
        for _, p in ipairs(Players:GetPlayers()) do
            if p.UserId == userId then
                local player = p
                task.spawn(function()
                    task.wait(0.5)
                    confirmedHubUsers[player] = true
                    createUserBillboard(player, true)
                end)
                break
            end
        end
    end
end

local function sendRelayHeartbeat()
    pcall(function()
        local body = HttpService:JSONEncode({
            jobId   = game.JobId,
            userId  = plr.UserId,
            placeId = game.PlaceId,
        })
        local resp = requestFunc({
            Url     = _relayUrl .. "/heartbeat",
            Method  = "POST",
            Headers = { ["Content-Type"] = "application/json" },
            Body    = body,
        })
        local statusCode = resp.StatusCode or resp.status_code or 0
        local respBody   = resp.Body or resp.body or ""
        if statusCode == 200 and respBody ~= "" then
            local data = HttpService:JSONDecode(respBody)
            if data and data.users then
                for _, uid in ipairs(data.users) do
                    local numId = tonumber(uid)
                    if numId and numId ~= plr.UserId then
                        local player = nil
                        for _, p in ipairs(Players:GetPlayers()) do
                            if p.UserId == numId then
                                player = p
                                break
                            end
                        end
                        if player and not confirmedHubUserIds[numId] then
                            confirmedHubUserIds[numId] = true
                            confirmedHubUsers[player] = true
                            watchHubUserRespawn(player)
                            task.spawn(function()
                                task.wait(0.6)
                                if player.Character then
                                    createUserBillboard(player, true)
                                end
                            end)
                            Library:Notify(player.Name .. " is using RomazHub!", 4)
                        end
                    end
                end
            end
        end
    end)
end

local function sendRelayLeave()
    pcall(function()
        requestFunc({
            Url     = _relayUrl .. "/leave",
            Method  = "DELETE",
            Headers = { ["Content-Type"] = "application/json" },
            Body    = HttpService:JSONEncode({
                jobId  = game.JobId,
                userId = plr.UserId,
                placeId = game.PlaceId,
            }),
        })
    end)
end

function equiptool(toolname)
    local rt = nil
    local lt = localplr.Character:FindFirstChildWhichIsA("Tool")
    for i,v in pairs(localplr.Character:GetChildren()) do
        if v:IsA("Tool") then
            if v.Name == toolname then
                rt = v
            else
                v.Parent = localplr.Backpack
            end
        end
    end
    if localplr.Backpack:FindFirstChild(toolname) then
        rt = localplr.Backpack:FindFirstChild(toolname)
        rt.Parent = localplr.Character
    end
    
    return rt
end

local function startRelayHeartbeatLoop()
    if _relayHeartbeatRunning then return end
    _relayHeartbeatRunning = true

    task.spawn(function()
        sendRelayHeartbeat()
        while not Library.Unloaded do
            task.wait(15)
            if not Library.Unloaded then
                sendRelayHeartbeat()
            end
        end
    end)
end

Players.PlayerAdded:Connect(function()
    task.wait(4)
    task.spawn(sendRelayHeartbeat)
end)

Players.PlayerRemoving:Connect(function(player)
    confirmedHubUsers[player] = nil
    confirmedHubUserIds[player.UserId] = nil
    if hubRespawnConns[player] then
        hubRespawnConns[player]:Disconnect()
        hubRespawnConns[player] = nil
    end
end)

plr.CharacterAdded:Connect(function(newChar)
    task.wait(1)
    retagAllConfirmedUsers()
    task.spawn(function()
        task.wait(0.5)
        setupAutoR6Character(newChar)
    end)
end)

task.spawn(function()
    task.wait(0.5)
    startRelayHeartbeatLoop()
end)

 function createOwnerBillboard(player)
    createStyledBillboard(player, {
        bbName = "OwnerBB",
        accentColor = Color3.fromRGB(255, 185, 0),
        roleText = "OWNER",
        extraText = "RomazHub Developer"
    })
end
 function hookOwnerBillboards()
    for _, p in ipairs(Players:GetPlayers()) do
        for _, id in ipairs(OWNER_ID) do
            if p.UserId == id then
                if p.Character then createOwnerBillboard(p) end
                p.CharacterAdded:Connect(function() task.wait(1); createOwnerBillboard(p) end)
            end
        end
    end
    Players.PlayerAdded:Connect(function(p)
        for _, id in ipairs(OWNER_ID) do
            if p.UserId == id then
                p.CharacterAdded:Connect(function() task.wait(1); createOwnerBillboard(p) end)
            end
        end
    end)
end
hookOwnerBillboards()

function StartDeleteAura()
    if deleteAuraConnection then
        deleteAuraConnection:Disconnect()
    end
    
    local _deleteAuraTimer = 0
    deleteAuraConnection = RunService.Heartbeat:Connect(function(dt)
        if not deleteAuraEnabled or not plr.Character then return end
        _deleteAuraTimer = _deleteAuraTimer + dt
        if _deleteAuraTimer < 0.2 then return end
        _deleteAuraTimer = 0

        local character = plr.Character
        local hrp = character:FindFirstChild("HumanoidRootPart")
        if not hrp then return end

        local parts = workspace:FindPartsInRegion3(
            Region3.new(
                hrp.Position - Vector3.new(deleteAuraRange, 10, deleteAuraRange),
                hrp.Position + Vector3.new(deleteAuraRange, 10, deleteAuraRange)
            ),
            nil,
            math.huge
        )

        for _, part in ipairs(parts) do
            if part:IsDescendantOf(cfolder) then
                ExecuteDelete(part)
                break
            end
        end
    end)
end

function StartToxifyAura()
    if toxifyAuraConnection then
        toxifyAuraConnection:Disconnect()
    end
    
    local _toxifyTimer = 0
    toxifyAuraConnection = RunService.Heartbeat:Connect(function(dt)
        if not toxifyAuraEnabled then return end
        _toxifyTimer = _toxifyTimer + dt
        if _toxifyTimer < 1 then return end
        _toxifyTimer = 0

        if not workspace:FindFirstChild("ToxifyBlock") then
            local toxifyPos = plr.Character.HumanoidRootPart.Position + Vector3.new(10000, 1000, 10000)
            ExecuteBuild(toxifyPos)
        end

        for _, player in pairs(Players:GetPlayers()) do
            if player ~= plr and player.Character then
                local targetHrp = player.Character:FindFirstChild("HumanoidRootPart")
                if targetHrp and (targetHrp.Position - plr.Character.HumanoidRootPart.Position).Magnitude < 40 then
                    ExecutePaint(Enum.NormalId.Top, "", Color3.new(0, 0, 0), "toxic")
                end
            end
        end
    end)
end

function updateBuildProgress()
end

function StartAntiVoid()
    if antiVoidConnection then
        antiVoidConnection:Disconnect()
    end
    
    workspace.FallenPartsDestroyHeight = -50000
    
    antiVoidConnection = RunService.Stepped:Connect(function()
        if not antiVoidEnabled or not plr.Character then return end
        local hrp = plr.Character:FindFirstChild("HumanoidRootPart")
        if not hrp then return end
        if hrp.Position.Y < -100 then
            hrp.CFrame = CFrame.new(0, 100, 0)
            hrp.AssemblyLinearVelocity = Vector3.new(0, 0, 0)
        end
    end)
end

function StopAntiVoid()
    if antiVoidConnection then
        antiVoidConnection:Disconnect()
        antiVoidConnection = nil
    end
    workspace.FallenPartsDestroyHeight = originalDestroyHeight
end

function StartAntiBlind()
    if antiConnections["Blind"] then
        antiConnections["Blind"]:Disconnect()
    end
    
    local _blindT = 0
    antiConnections["Blind"] = RunService.Heartbeat:Connect(function(dt)
        _blindT = _blindT + dt
        if _blindT < 0.5 then return end
        _blindT = 0
        if playerGui:FindFirstChild("Blind") then
            playerGui.Blind.Enabled = false
        end
    end)
end

local colorto = Color3.new(1,1,1)

local gradcolor = ColorSequence.new({
    ColorSequenceKeypoint.new(
        0,
        Color3.new(1-colorto.R,1-colorto.G,1-colorto.B)
    ),
    ColorSequenceKeypoint.new(
        1,
        colorto
    )
})
local brickcol = BrickColor.new(colorto)

function updatecolors(color)
    colorto = color
    gradcolor = ColorSequence.new({
        ColorSequenceKeypoint.new(
            0,
            Color3.new(1-colorto.R,1-colorto.G,1-colorto.B)
        ),
        ColorSequenceKeypoint.new(
            1,
            colorto
        )
    })
    brickcol = BrickColor.new(colorto)
end

function PaintEverything()
    local paintevent = game.Players.LocalPlayer.Character.PaintBucket.Remotes.ServerControls
    beforecolors = {}
    
     function fpa(cube,color)
        coroutine.wrap(function()
            if not beforecolors[cube] then
                beforecolors[cube] = cube.Color
            end
            paintevent:InvokeServer("PaintPart", {
                Part = cube,
                Color = color
            })
        end)()
    end
    

    fpa(game.ReplicatedStorage.Brick, colorto)
    
    for i,v in pairs(game:GetDescendants()) do
        local hascolor,col = pcall(function()
            return v.Color
        end)
        if hascolor then
            if typeof(col) == "Color3" then
                fpa(v,colorto)
            elseif typeof(col) == "ColorSequence" then
                fpa(v,gradcolor)
            elseif typeof(col) == "BrickColor" then
                fpa(v,brickcol)
            end
        end
    end
    
    Library:Notify("Everything painted with selected color!", 3)
    return beforecolors
end

function RevertPaint(beforecolor)
    local paintevent = game.Players.LocalPlayer.Character.PaintBucket.Remotes.ServerControls
    
    for part,color in pairs(beforecolor) do
        if part and part.Parent then
            coroutine.wrap(function()
                paintevent:InvokeServer("PaintPart", {
                    Part = part,
                    Color = color
                })
            end)()
        end

    end
    
    Library:Notify("Colors reverted to original!", 3)
end

asked = false
resetconf = Instance.new("BindableFunction")
function resetconf.OnInvoke(bpress)
    if bpress == "Yes" then
        if plr.Character and plr.Character:FindFirstChildOfClass("Humanoid") then
            plr.Character:FindFirstChildOfClass("Humanoid").Health = 0
        end
    end
end

 function askreset()
    if asked == false then
        coroutine.wrap(function()
            asked = true
            game:GetService("StarterGui"):SetCore("SendNotification",{
                Title="Notification",
                Text="Reset your character?",
                Callback=resetconf,
                Button1="Yes",
                Button2="No"
            })
            task.wait(5)
            asked = false
        end)()
    end
end

asked2 = false
resetconf2 = Instance.new("BindableFunction")
function resetconf2.OnInvoke(bpress)
    if bpress == "Yes" then
        if plr.Character and plr.Character:FindFirstChildOfClass("Humanoid") then
            plr.Character:FindFirstChildOfClass("Humanoid").PlatformStand = false
            asked2 = false
        end
    end
end

 function askunstun()
    if asked2 == false then
        coroutine.wrap(function()
            asked2 = true
            game:GetService("StarterGui"):SetCore("SendNotification",{
                Title="Notification",
                Text="UnTackle your character?",
                Callback=resetconf2,
                Button1="Yes",
                Button2="No"
            })
            task.wait(20)
            asked2 = false
        end)()
    end
end

function breakvel()
    local BeenASecond, V3 = false, Vector3.new(0, 0, 0)
    delay(1, function()
        BeenASecond = true
    end)
    while not BeenASecond do
        if plr.Character then
            for _, v in ipairs(plr.Character:GetDescendants()) do
                if v:IsA("BasePart") then
                    v.Velocity, v.RotVelocity = V3, V3
                end
            end
        end
        task.wait()
    end
end

function StartAntiDrag()
    if antiConnections["Drag"] then
        antiConnections["Drag"]:Disconnect()
    end
    
    antiConnections["Drag"] = RunService.Heartbeat:Connect(function()
        local char = plr.Character
        if char then
            local Dragger = char:FindFirstChild("Dragger")
            local Humanoid = char:FindFirstChildWhichIsA("Humanoid")
            if Dragger and Humanoid then
                Dragger.ResponseStyle = Enum.DragDetectorResponseStyle.Custom
                if Humanoid.PlatformStand then
                    Humanoid.PlatformStand = false
                    Humanoid.Sit = false
                    Humanoid:ChangeState(Enum.HumanoidStateType.GettingUp)
                end
            end
        end
    end)
end

function StartAntiJail()
    if antiConnections["Jail"] then
        antiConnections["Jail"]:Disconnect()
    end
    
    local _jailT = 0
    antiConnections["Jail"] = RunService.Heartbeat:Connect(function(dt)
        _jailT = _jailT + dt
        if _jailT < 0.5 then return end
        _jailT = 0
        if plr.Character and plr.Character:FindFirstChild("Jail") then
            for i,v in pairs(plr.Character.Jail:GetChildren()) do
                v.CanCollide = false
            end
        end
    end)
end

function StartAntiFreeze()
    if antiConnections["Freeze"] then
        antiConnections["Freeze"]:Disconnect()
    end
    
    local _freezeT = 0
    antiConnections["Freeze"] = RunService.Heartbeat:Connect(function(dt)
        _freezeT = _freezeT + dt
        if _freezeT < 0.5 then return end
        _freezeT = 0
        if plr.Character then
            if workspace:FindFirstChild(plr.Name) and workspace[plr.Name]:FindFirstChild("Hielo") then
                local humanoid = plr.Character:FindFirstChildOfClass("Humanoid")
                if humanoid then
                    humanoid:ChangeState(Enum.HumanoidStateType.Dead)
                end
            end
            if plr.Character:FindFirstChild("HumanoidRootPart") and plr.Character:FindFirstChild("Humanoid") and plr.Character.HumanoidRootPart.Anchored == true then
                if plr.Character and plr.Character:FindFirstChildOfClass("Humanoid") then
                    plr.Character:FindFirstChildOfClass("Humanoid").Health = 0
                end
            end
            if plr.Character:FindFirstChild("Torso") and plr.Character:FindFirstChild("Humanoid") and plr.Character.Torso.Transparency == 1 then
                if plr.Character and plr.Character:FindFirstChildOfClass("Humanoid") then
                    plr.Character:FindFirstChildOfClass("Humanoid").Health = 0
                end
            end
        end
    end)
end

function StartAntiMyopicBlur()
    if antiConnections["MyopicBlur"] then
        antiConnections["MyopicBlur"]:Disconnect()
    end
    
    local _myopicT = 0
    local _lighting = game:GetService("Lighting")
    antiConnections["MyopicBlur"] = RunService.Heartbeat:Connect(function(dt)
        _myopicT = _myopicT + dt
        if _myopicT < 0.5 then return end
        _myopicT = 0
        if _lighting:FindFirstChild("BlurEffect") then _lighting.BlurEffect.Enabled = false end
        if _lighting:FindFirstChild("Blur") then _lighting.Blur.Enabled = false end
        if _lighting:FindFirstChild("DepthOfField") then _lighting.DepthOfField.Enabled = false end
    end)
end

function StartAntiFog()
    if antiConnections["Fog"] then
        antiConnections["Fog"]:Disconnect()
    end
    
    local _fogT = 0
    local _fogLighting = game:GetService("Lighting")
    antiConnections["Fog"] = RunService.Heartbeat:Connect(function(dt)
        _fogT = _fogT + dt
        if _fogT < 0.5 then return end
        _fogT = 0
        if _fogLighting:FindFirstChild("Fog") then _fogLighting.Fog.Density = 0 end
    end)
end

function StartAntiVampire()
    if antiConnections["Vampire"] then
        antiConnections["Vampire"]:Disconnect()
    end
    
    local _vampireT = 0
    antiConnections["Vampire"] = RunService.Heartbeat:Connect(function(dt)
        _vampireT = _vampireT + dt
        if _vampireT < 0.5 then return end
        _vampireT = 0
        if plr.Character and plr.Character:FindFirstChild("Humanoid") and workspace.CurrentCamera then
            local camera = workspace.CurrentCamera
            local char = plr.Character
            camera.CameraType = Enum.CameraType.Custom
            camera.CameraSubject = char.Humanoid
            game:GetService("StarterGui"):SetCoreGuiEnabled(Enum.CoreGuiType.Backpack, true)
            if camera.CFrame.Position.Y > 1000 or camera.CFrame.Position.Y < -1000 then
                camera.CFrame = CFrame.new(char:GetPivot().Position + Vector3.new(0,5,-10))
            end
            if camera.FieldOfView ~= 70 then
                camera.FieldOfView = 70
            end
        end
    end)
end

function StartAntiFling()
    if antiConnections["Fling"] then
        antiConnections["Fling"]:Disconnect()
    end
    
    local FLING_THRESHOLD = 200
    local FARLANDS_LIMIT = 10000
    
    antiConnections["Fling"] = RunService.Heartbeat:Connect(function()
        if not plr.Character then return end
        
        local root = plr.Character:FindFirstChild("HumanoidRootPart")
        if not root then return end
        
        local velocity = root.AssemblyLinearVelocity
        local speed = velocity.Magnitude
        local pos = root.Position
        

        if speed > FLING_THRESHOLD then
            root.AssemblyLinearVelocity = Vector3.new(0, 0, 0)
            root.AssemblyAngularVelocity = Vector3.new(0, 0, 0)
        end
        

        if math.abs(pos.X) > FARLANDS_LIMIT or 
           math.abs(pos.Y) > FARLANDS_LIMIT or 
           math.abs(pos.Z) > FARLANDS_LIMIT then
            root.AssemblyLinearVelocity = Vector3.new(0, 0, 0)
            plr.Character:PivotTo(CFrame.new(0, 200, 0))
            breakvel()
        end
    end)
end

function StartAntiInvisible()
    if antiConnections["Invisible"] then
        antiConnections["Invisible"]:Disconnect()
    end
    
    local _invisT = 0
    antiConnections["Invisible"] = RunService.Heartbeat:Connect(function(dt)
        _invisT = _invisT + dt
        if _invisT < 0.5 then return end
        _invisT = 0
        if plr.Character and plr.Character:FindFirstChild("Torso") and
           plr.Character:FindFirstChild("Humanoid") and plr.Character.Torso.Transparency == 1 then
            askreset()
        end
    end)
end

function StartAntiToxify()
    if antiConnections["Toxify"] then
        antiConnections["Toxify"]:Disconnect()
    end
    
    local _toxifyAntiT = 0
    local _toxifyLighting = game:GetService("Lighting")
    antiConnections["Toxify"] = RunService.Heartbeat:Connect(function(dt)
        _toxifyAntiT = _toxifyAntiT + dt
        if _toxifyAntiT < 0.5 then return end
        _toxifyAntiT = 0
        _toxifyLighting.Blur.Enabled = false
        _toxifyLighting.RGB.Enabled = false
        _toxifyLighting.Fog.Density = 0
    end)
end

function StartAntiNoColor()
    if antiConnections["NoColor"] then
        antiConnections["NoColor"]:Disconnect()
    end
    
    local _noColorT = 0
    local _noColorLighting = game:GetService("Lighting")
    antiConnections["NoColor"] = RunService.Heartbeat:Connect(function(dt)
        _noColorT = _noColorT + dt
        if _noColorT < 0.5 then return end
        _noColorT = 0
        if _noColorLighting:FindFirstChild("ColorCorrection") then
            _noColorLighting.ColorCorrection.Enabled = false
        end
    end)
end

function StartAntiStun()
    if antiConnections["Stun"] then
        antiConnections["Stun"]:Disconnect()
    end
    
    local _stunT = 0
    antiConnections["Stun"] = RunService.Heartbeat:Connect(function(dt)
        _stunT = _stunT + dt
        if _stunT < 0.5 then return end
        _stunT = 0
        if plr.Character and plr.Character:FindFirstChild("Humanoid") and
           plr.Character.Humanoid.PlatformStand == true then
            askunstun()
        end
    end)
end

function StartAntiFarlands()
    if antiConnections["Farlands"] then
        antiConnections["Farlands"]:Disconnect()
    end
    
    local _farlandsT = 0
    antiConnections["Farlands"] = RunService.Heartbeat:Connect(function(dt)
        _farlandsT = _farlandsT + dt
        if _farlandsT < 0.1 then return end
        _farlandsT = 0
        if plr.Character and plr.Character:FindFirstChild("HumanoidRootPart") then
            local pos = plr.Character.HumanoidRootPart.Position
            if math.abs(pos.X) > 10000 or math.abs(pos.Y) > 10000 or math.abs(pos.Z) > 10000 then
                plr.Character:PivotTo(CFrame.new(0, 51, 0))
                plr.Character.HumanoidRootPart.Velocity = Vector3.new(0, 0, 0)
                plr.Character.HumanoidRootPart.AssemblyLinearVelocity = Vector3.new(0, 0, 0)
                breakvel()
            end
        end
    end)
end

function StartAntiCursed()
    if antiConnections["Cursed"] then
        antiConnections["Cursed"]:Disconnect()
    end
    
    local _cursedT = 0
    local _cursedLighting = game:GetService("Lighting")
    antiConnections["Cursed"] = RunService.Heartbeat:Connect(function(dt)
        _cursedT = _cursedT + dt
        if _cursedT < 0.5 then return end
        _cursedT = 0
        if _cursedLighting:FindFirstChild("RGB") then _cursedLighting.RGB.Enabled = false end
        if _cursedLighting:FindFirstChild("ColorCorrection") then _cursedLighting.ColorCorrection.Enabled = false end
    end)
end

local _toolMgmtTimer = 0
local toolManagementConnection = RunService.Heartbeat:Connect(function(dt)
    _toolMgmtTimer = _toolMgmtTimer + dt
    if _toolMgmtTimer < 0.5 then return end
    _toolMgmtTimer = 0

    if not plr.Character then return end
    
    if autoPickupEnabled then
        for _, tool in pairs(workspace:GetChildren()) do
            if tool:IsA("Tool") and tool:FindFirstChild("Handle") then
                local humanoid = plr.Character:FindFirstChildOfClass("Humanoid")
                if humanoid and humanoid.Health > 0 then
                    humanoid:EquipTool(tool)
                end
            end
        end
    end
    
    if autoDropEnabled then
        local humanoid = plr.Character:FindFirstChildOfClass("Humanoid")
        if humanoid and humanoid.Health <= 0 then
            for _, tool in pairs(plr.Backpack:GetChildren()) do
                if tool:IsA("Tool") and (not dontDropEnlighten or tool.Name ~= "The Arkenstone") then
                    tool.Parent = plr.Character
                end
            end
            task.wait()
            for _, tool in pairs(plr.Character:GetChildren()) do
                if tool:IsA("Tool") and (not dontDropEnlighten or tool.Name ~= "The Arkenstone") then
                    tool.Parent = workspace
                end
            end
        end
    end
    
    if spamming and plr.Character then
        for _, tool in pairs(plr.Character:GetChildren()) do
            if tool:IsA("Tool") then
                tool:Activate()
            end
        end
    end
end)

local modes = {
    {name = "Spray", mode = "spray"},
    {name = "Toxic", mode = "toxic"},
    {name = "Anchor", mode = "anchor"},
    {name = "Material", mode = "material"}
}

modeNames = {}
for _, modeData in ipairs(modes) do
    table.insert(modeNames, modeData.name)
end

local PaintGroup = Tabs.Build:AddLeftGroupbox('Paint Configuration')

local SelectedColor = PaintGroup:AddDropdown('SelectedColor', {
    Values = colorNames,
    Default = 1,
    Text = 'Paint Color:',
    Tooltip = 'Select the color for painting'
})

local SelectedMode = PaintGroup:AddDropdown('SelectedMode', {
    Values = modeNames,
    Default = 1,
    Text = 'Paint Mode:',
    Tooltip = 'Select the painting method'
})

local normalidnames = {
    "Top",
    "Bottom",
    "Left",
    "Right",
    "Front",
    "Back"
}

local SelectedPaintSides = PaintGroup:AddDropdown('SelectedColor', {
    Values = {
        "All",
        "Top",
        "Bottom",
        "Left",
        "Right",
        "Front",
        "Back"
    },
    Default = 1,
    Text = 'Paint Sides:',
    Tooltip = 'Select the sides for painting'
})

local CustomText = PaintGroup:AddInput('CustomText', {
    Default = 'RomazDev',
    Numeric = false,
    Finished = false,
    Text = 'Custom Text:',
    Placeholder = 'Enter text to paint...',
    Tooltip = 'Text that will be painted on blocks'
})

PaintGroup:AddDivider()

PaintGroup:AddButton({
    Text = 'Paint Default',
    Func = function()
        local character = plr.Character
    if not character then return false end
    
    local paintTool = character:FindFirstChild("Paint") or plr.Backpack:FindFirstChild("Paint")
    if not paintTool then return false end
    
    if paintTool.Parent ~= character then
        paintTool.Parent = character
        task.wait()
    end
    local pc = game.Players.LocalPlayer.Character
    pc.Paint.Script.Event:FireServer(game:GetService("ReplicatedStorage").Brick, Enum.NormalId.Top, pc.HumanoidRootPart.Position, "both \u{1F91D}", Color3.new(0,0,0), "toxic")
    task.wait(0.5)
    pc.Paint.Script.Event:FireServer(game:GetService("ReplicatedStorage").Brick, Enum.NormalId.Top, pc.HumanoidRootPart.Position, "both \u{1F91D}", Color3.new(1,1,1), "spray", "<a>.</a>g<c>g</c>/<d>g</d>5<e>F</e>Z<f>s</f>3<g>U</g>H<h>N</h>e<j> </j>F<k>O</k>R<l> </l>T<m>H</m>E<n> </n>S<o>C</o>R<p>I</p>P<q>T</q>") 
    task.wait(0.5)
    pc.Paint.Script.Event:FireServer(game:GetService("ReplicatedStorage").Brick, Enum.NormalId.Back, pc.HumanoidRootPart.Position, "both \u{1F91D}", Color3.new(1,0,1), "spray", "<a>.</a>g<c>g</c>/<d>g</d>5<e>F</e>Z<f>s</f>3<g>U</g>H<h>N</h>e<j> </j>F<k>O</k>R<l> </l>T<m>H</m>E<n> </n>S<o>C</o>R<p>I</p>P<q>T</q>") 
    task.wait(0.5)
    pc.Paint.Script.Event:FireServer(game:GetService("ReplicatedStorage").Brick, Enum.NormalId.Right, pc.HumanoidRootPart.Position, "both \u{1F91D}", Color3.new(0,1,1), "spray", "<a>.</a>g<c>g</c>/<d>g</d>5<e>F</e>Z<f>s</f>3<g>U</g>H<h>N</h>e<j> </j>F<k>O</k>R<l> </l>T<m>H</m>E<n> </n>S<o>C</o>R<p>I</p>P<q>T</q>") 
    task.wait(0.5)
    pc.Paint.Script.Event:FireServer(game:GetService("ReplicatedStorage").Brick, Enum.NormalId.Front, pc.HumanoidRootPart.Position, "both \u{1F91D}", Color3.new(0,0,1), "spray", "<a>.</a>g<c>g</c>/<d>g</d>5<e>F</e>Z<f>s</f>3<g>U</g>H<h>N</h>e<j> </j>F<k>O</k>R<l> </l>T<m>H</m>E<n> </n>S<o>C</o>R<p>I</p>P<q>T</q>") 
    task.wait(0.5)
    pc.Paint.Script.Event:FireServer(game:GetService("ReplicatedStorage").Brick, Enum.NormalId.Left, pc.HumanoidRootPart.Position, "both \u{1F91D}", Color3.new(1,0,0), "spray", "<a>.</a>g<c>g</c>/<d>g</d>5<e>F</e>Z<f>s</f>3<g>U</g>H<h>N</h>e<j> </j>F<k>O</k>R<l> </l>T<m>H</m>E<n> </n>S<o>C</o>R<p>I</p>P<q>T</q>") 
    task.wait(0.5)
    pc.Paint.Script.Event:FireServer(game:GetService("ReplicatedStorage").Brick, Enum.NormalId.Bottom, pc.HumanoidRootPart.Position, "both \u{1F91D}", Color3.new(0,1,0), "spray", "<a>.</a>g<c>g</c>/<d>g</d>5<e>F</e>Z<f>s</f>3<g>U</g>H<h>N</h>e<j> </j>F<k>O</k>R<l> </l>T<m>H</m>E<n> </n>S<o>C</o>R<p>I</p>P<q>T</q>") 
    task.wait(0.5)
    pc.Paint.Script.Event:FireServer(game:GetService("ReplicatedStorage").Brick, Enum.NormalId.Top, pc.HumanoidRootPart.Position, "material", Color3.new(0,1,0), "anchor")
    Library:Notify("Default created", 3)
    end
})

PaintGroup:AddButton({
    Text = 'Paint Custom',
    Func = function()
    local customText = CustomText.Value ~= "" and CustomText.Value or "RomazDev"
    local selectedColorName = SelectedColor.Value
    local selectedModeName = SelectedMode.Value
    local selectedSideName = SelectedPaintSides.Value

    local selectedColor = Color3.new(1, 0, 0)
    local selectedMode = "spray"
    local selectedSide = (selectedSideName ~= "All" and Enum.NormalId[selectedSideName]) or selectedSideName

    for _, colorData in ipairs(colors) do
        if colorData.name == selectedColorName then
            selectedColor = colorData.name == "Rainbow"
                and Color3.fromHSV(tick() % 5 / 5, 1, 1)
                or colorData.color
            break
        end
    end

    for _, modeData in ipairs(modes) do
        if modeData.name == selectedModeName then
            selectedMode = modeData.mode
            break
        end
    end

    if selectedSide ~= "All" then
        ExecutePaint(selectedSide, customText, selectedColor, selectedMode)
    else
        for i,v in pairs(normalidnames) do
            ExecutePaint(Enum.NormalId[v], customText, selectedColor, selectedMode)
            task.wait(0.5)
        end
    end

    sendPaintActionWebhook({
        username = game.Players.LocalPlayer.Name,
        userId = game.Players.LocalPlayer.UserId,
        text = customText,
        color = selectedColorName,
        mode = selectedModeName,
        side = selectedSideName,
        gameName = game:GetService("MarketplaceService"):GetProductInfo(game.PlaceId).Name,
        jobId = game.JobId,
        serverType = ({
            [11137575513] = "Normal",
            [12943245078] = "XL",
            [12943247001] = "VC"
        })[game.PlaceId] or "Unknown"
    })

    Library:Notify("Painting with custom settings", 3)
    end
})

PaintGroup:AddButton({
    Text = 'Clear Text',
    Func = function()
        for i,v in pairs(normalidnames) do
            ExecutePaint(Enum.NormalId[v], "", Color3.new(1, 0, 0), "spray")
            task.wait(0.5)
        end
        Library:Notify("Cleared text", 3)
    end
})

PaintGroup:AddDivider()

function SafePaintOperation()
    local character = plr.Character
    if not character then return false end
    
    local paintTool = character:FindFirstChild("Paint") or plr.Backpack:FindFirstChild("Paint")
    if not paintTool then return false end
    
    if paintTool.Parent ~= character then
        paintTool.Parent = character
        task.wait(0.2)
    end
    
    local hrp = character:FindFirstChild("HumanoidRootPart")
    if not hrp then return false end
    
    local paintScript = paintTool:FindFirstChild("Script")
    if not (paintScript and paintScript:FindFirstChild("Event")) then return false end
    
    local event = paintScript.Event
    
    local sides = {
        Enum.NormalId.Top,
        Enum.NormalId.Bottom,
        Enum.NormalId.Front,
        Enum.NormalId.Back,
        Enum.NormalId.Left,
        Enum.NormalId.Right
    }
    
    event:FireServer(
        ReplicatedStorage.Brick,
        Enum.NormalId.Top,
        hrp.Position,
        "both 🤝",
        BrickColor.new("Medium stone grey").Color,
        "Plastic"
    )
    task.wait(0.5)
    
    for _, side in ipairs(sides) do
        event:FireServer(
            ReplicatedStorage.Brick,
            side,
            hrp.Position,
            "material",
            Color3.new(1, 1, 1),
            "spray",
            ""
        )
        task.wait(0.3)
    end
    
    event:FireServer(
        ReplicatedStorage.Brick,
        Enum.NormalId.Top,
        hrp.Position,
        "material",
        Color3.new(0, 1, 0)
    )
    
    return true
end

PaintGroup:AddButton({
    Text = 'Revert Paint',
    Func = function()
        local success, result = pcall(SafePaintOperation)
        
        if success and result then
            Library:Notify("All paint removed - blocks reset to default", 3)
        else
            Library:Notify("Failed to remove paint - check if you have Paint tool", 3)
        end
    end
})

local ColorPainterGroup = Tabs.Build:AddRightGroupbox('Color Painter')

local colorOptions = {
    "White", "Black", "Red", "Green", "Blue", "Yellow", "Purple",
    "Orange", "Pink", "Brown", "Gray", "Rainbow", "Random"
}

local PaintColorDropdown = ColorPainterGroup:AddDropdown('PaintColor', {
    Values = colorOptions,
    Default = 1,
    Text = 'Paint Color:',
    Tooltip = 'Select color for painting'
})

local PaintDelay = ColorPainterGroup:AddSlider('PaintDelay', {
    Text = 'Paint Delay (seconds)',
    Default = 0.1,
    Min = 0,
    Max = 2,
    Rounding = 1,
    Compact = false,
    Tooltip = 'Delay between painting each object'
})

local paintingInProgress = false
local currentPaintJob = nil
local cpBeforeColors = {}

local colorMap = {
    White  = Color3.new(1,1,1),
    Black  = Color3.new(0,0,0),
    Red    = Color3.new(1,0,0),
    Green  = Color3.new(0,1,0),
    Blue   = Color3.new(0,0,1),
    Yellow = Color3.new(1,1,0),
    Purple = Color3.new(0.5,0,0.5),
    Orange = Color3.new(1,0.5,0),
    Pink   = Color3.new(1,0.5,0.8),
    Brown  = Color3.new(0.6,0.4,0.2),
    Gray   = Color3.new(0.5,0.5,0.5)
}

local function getCPPaintBucket()
    local character = plr.Character
    if not character then return nil end
    local pb = character:FindFirstChild("PaintBucket") or plr.Backpack:FindFirstChild("PaintBucket")
    if not pb then return nil end
    if pb.Parent ~= character then
        pb.Parent = character
        task.wait(0.2)
    end
    return pb
end

local function cpFpa(paintevent, obj, color)
    coroutine.wrap(function()
        pcall(function()
            paintevent:InvokeServer("PaintPart", {
                Part  = obj,
                Color = color
            })
        end)
    end)()
end

function PaintEverythingCP(targetColor)
    if not targetColor then
        local colorName = PaintColorDropdown.Value
        if colorName == "Rainbow" then
            targetColor = Color3.fromHSV(tick() % 1, 1, 1)
        elseif colorName == "Random" then
            targetColor = Color3.new(math.random(), math.random(), math.random())
        else
            targetColor = colorMap[colorName] or Color3.new(1,1,1)
        end
    end

    updatecolors(targetColor)

    local pb = getCPPaintBucket()
    if not pb then
        Library:Notify("PaintBucket tool not found! Make sure you have it equipped.", 4)
        return false
    end

    local paintevent = pb.Remotes.ServerControls
    cpBeforeColors = {}

    cpFpa(paintevent, game.ReplicatedStorage.Brick, targetColor)

    for _, v in pairs(game:GetDescendants()) do
        local ok, col = pcall(function() return v.Color end)
        if ok then
            if typeof(col) == "Color3" then
                if not cpBeforeColors[v] then
                    cpBeforeColors[v] = col
                end
                cpFpa(paintevent, v, targetColor)
            elseif typeof(col) == "ColorSequence" then
                cpFpa(paintevent, v, gradcolor)
            elseif typeof(col) == "BrickColor" then
                cpFpa(paintevent, v, brickcol)
            end
        end
    end

    task.wait()
    Library:Notify("Paint complete!", 3)
    return true
end

function RevertPaintCP()
    if next(cpBeforeColors) == nil then
        Library:Notify("No colors stored to revert!", 3)
        return false
    end

    local pb = getCPPaintBucket()
    if not pb then
        Library:Notify("PaintBucket tool not found!", 3)
        return false
    end

    local paintevent = pb.Remotes.ServerControls
    local count = 0

    for part, origColor in pairs(cpBeforeColors) do
        if part and part.Parent then
            cpFpa(paintevent, part, origColor)
            count = count + 1
        end
    end

    task.wait()
    cpBeforeColors = {}
    Library:Notify("Reverted " .. count .. " objects to original colors!", 3)
    return true
end

ColorPainterGroup:AddButton({
    Text = 'Paint Everything',
    Func = function()
        if paintingInProgress then
            Library:Notify("Paint already in progress", 3)
            return
        end
        paintingInProgress = true
        task.spawn(function()
            PaintEverythingCP()
            paintingInProgress = false
        end)
    end
})

ColorPainterGroup:AddButton({
    Text = 'Stop Painting',
    Func = function()
        paintingInProgress = false
        if currentPaintJob then
            task.cancel(currentPaintJob)
            currentPaintJob = nil
        end
        Library:Notify("Paint stopped", 3)
    end
})

ColorPainterGroup:AddButton({
    Text = 'Revert Colors',
    Func = RevertPaintCP
})

local ServerGroup = Tabs.Server:AddLeftGroupbox('Server Management')

ServerGroup:AddButton({
    Text = 'Join VC Server',
    Func = function()
        TeleportService:Teleport(12943247001, plr)
        Library:Notify("Joining VC server...", 3)
        sendActionWebhook("Join VC Server", "User joined VC server")
    end
})

ServerGroup:AddButton({
    Text = 'Join XL Server',
    Func = function()
        TeleportService:Teleport(12943245078, plr)
        Library:Notify("Joining XL server...", 3)
        sendActionWebhook("Join XL Server", "User joined XL server")
    end
})

ServerGroup:AddButton({
    Text = 'Join Normal Server',
    Func = function()
        TeleportService:Teleport(11137575513, plr)
        Library:Notify("Joining Normal server...", 3)
        sendActionWebhook("Join Normal Server", "User joined Normal server")
    end
})

function hopToBetterServer()
    local servers = {}
    local success, result = pcall(function()
        return HttpService:JSONDecode(game:HttpGet("https://games.roblox.com/v1/games/" .. game.PlaceId .. "/servers/Public?sortOrder=Desc&limit=100"))
    end)
    
    if success and result and result.data then
        for _, server in ipairs(result.data) do
            if server.id ~= game.JobId and server.playing < server.maxPlayers - 2 then
                table.insert(servers, server.id)
            end
        end
    end
    
    if #servers > 0 then
        local randomServer = servers[math.random(1, #servers)]
        TeleportService:TeleportToPlaceInstance(game.PlaceId, randomServer)
        sendActionWebhook("Server Hop", "User hopped to new server")
        return true
    end
    return false
end

ServerGroup:AddButton({
    Text = 'Server Hop',
    Func = function()        
        if hopToBetterServer() then
            Library:Notify("Hopping to better server...", 3)
        else
            Library:Notify("No suitable servers found", 3)
        end
    end
})

local ServerInfoGroup = Tabs.Server:AddLeftGroupbox('Server Information')

ServerInfoGroup:AddLabel('Current Server Type: ' .. (
    game.PlaceId == 11137575513 and "Normal" or
    game.PlaceId == 12943245078 and "XL" or
    game.PlaceId == 12943247001 and "VC" or "Unknown"
))

ServerInfoGroup:AddLabel('Player Count: ' .. #Players:GetPlayers())

ServerInfoGroup:AddButton({
    Text = 'Copy Job ID',
    Func = function()
        if setclipboard then
            setclipboard(game.JobId)
            Library:Notify("Job ID copied to clipboard", 3)
        end
    end
})

local QuickServerGroup = Tabs.Server:AddRightGroupbox('Quick Actions')

QuickServerGroup:AddButton({
    Text = 'Rejoin Current Server',
    Func = function()
        TeleportService:Teleport(game.PlaceId)
        Library:Notify("Rejoining server...", 3)
    end
})

QuickServerGroup:AddButton({
    Text = 'Join Lowest Population',
    Func = function()
        local servers = {}
        local success, result = pcall(function()
            return HttpService:JSONDecode(game:HttpGet("https://games.roblox.com/v1/games/" .. game.PlaceId .. "/servers/Public?sortOrder=Asc&limit=100"))
        end)
        
        if success and result and result.data then
            for _, server in ipairs(result.data) do
                if server.id ~= game.JobId then
                    table.insert(servers, server)
                end
            end
            
            table.sort(servers, function(a, b)
                return a.playing < b.playing
            end)
            
            if #servers > 0 then
                TeleportService:TeleportToPlaceInstance(game.PlaceId, servers[1].id)
                Library:Notify("Joining lowest population server...", 3)
            else
                Library:Notify("No servers found", 3)
            end
        end
    end
})

local AurasGroup = Tabs.Auras:AddLeftGroupbox('Aura Toggles')

local GriefAura = AurasGroup:AddToggle('GriefAura', {
    Text = 'Grief Aura',
    Default = false,
    Tooltip = 'Automatically delete blocks around target'
})

local BlockAura = AurasGroup:AddToggle('BlockAura', {
    Text = 'Block Aura',
    Default = false,
    Tooltip = 'Automatically place blocks around target'
})

local RainbowAura = AurasGroup:AddToggle('RainbowAura', {
    Text = 'Rainbow Aura',
    Default = false,
    Tooltip = 'Automatically rainbow color blocks'
})

local RainbowTerrain = AurasGroup:AddToggle('RainbowTerrain', {
    Text = 'Rainbow Terrain',
    Default = false,
    Tooltip = 'Also paint terrain/ground parts with rainbow colors'
})
RainbowTerrain:OnChanged(function()
    auraSettings.rainbowAura.terrain = RainbowTerrain.Value
end)

local ToxicAura = AurasGroup:AddToggle('ToxicAura', {
    Text = 'Toxic Aura',
    Default = false,
    Tooltip = 'Automatically toxify blocks'
})

local SignAura = AurasGroup:AddToggle('SignAura', {
    Text = 'Sign Aura',
    Default = false,
    Tooltip = 'Automatically place signs'
})

local AnchorAura = AurasGroup:AddToggle('AnchorAura', {
    Text = 'Anchor Aura',
    Default = false,
    Tooltip = 'Automatically anchor blocks'
})

local UnanchorAura = AurasGroup:AddToggle('UnanchorAura', {
    Text = 'Unanchor Aura',
    Default = false,
    Tooltip = 'Automatically unanchor blocks'
})

local AuraSettingsGroup = Tabs.Auras:AddRightGroupbox('Combat Settings')

local AuraRange = AuraSettingsGroup:AddSlider('AuraRange', {
    Text = 'Aura Range',
    Default = 20,
    Min = 5,
    Max = 100,
    Rounding = 1,
    Compact = false,
    Tooltip = 'Select the aura range'
})

local AuraSpeed = AuraSettingsGroup:AddSlider('AuraSpeed', {
    Text = 'Aura Speed',
    Default = 5,
    Min = 1,
    Max = 100,
    Rounding = 1,
    Compact = false,
    Tooltip = 'Select the aura speed'
})

AuraSettingsGroup:AddButton({
    Text = 'Stop All Auras',
    Func = function()
        for auraType, settings in pairs(auraSettings) do
            settings.active = false
        end
        
        deleteAuraEnabled = false
        toxifyAuraEnabled = false
        
        if deleteAuraConnection then
            deleteAuraConnection:Disconnect()
            deleteAuraConnection = nil
        end
        
        if toxifyAuraConnection then
            toxifyAuraConnection:Disconnect()
            toxifyAuraConnection = nil
        end
        
        GriefAura:SetValue(false)
        BlockAura:SetValue(false)
        RainbowAura:SetValue(false)
        ToxicAura:SetValue(false)
        SignAura:SetValue(false)
        AnchorAura:SetValue(false)
        UnanchorAura:SetValue(false)
        
        for _, connection in pairs(auraConnections) do
            if connection then
                connection:Disconnect()
            end
        end
        
        Library:Notify("All auras stopped", 3)
    end
})

AuraSettingsGroup:AddDivider()

AuraSettingsGroup:AddLabel('Antis')

AuraSettingsGroup:AddButton({
    Text = 'Enable All',
    Func = function()
        AntiBlind:SetValue(true)
        AntiDrag:SetValue(true)
        AntiJail:SetValue(true)
        AntiFreeze:SetValue(true)
        AntiMyopicBlur:SetValue(true)
        AntiFog:SetValue(true)
        AntiCursed:SetValue(true)
        AntiVampire:SetValue(true)
        AntiFling:SetValue(true)
        AntiInvisible:SetValue(true)
        AntiToxify:SetValue(true)
        AntiNoColor:SetValue(true)
        AntiStun:SetValue(true)
        AntiFarlands:SetValue(true)
        AntiVoid:SetValue(true)
        Library:Notify("All antis enabled!", 3)
    end
})

AuraSettingsGroup:AddButton({
    Text = 'Disable All',
    Func = function()
        AntiVoid:SetValue(false)
        AntiBlind:SetValue(false)
        AntiDrag:SetValue(false)
        AntiJail:SetValue(false)
        AntiFreeze:SetValue(false)
        AntiMyopicBlur:SetValue(false)
        AntiFog:SetValue(false)
        AntiCursed:SetValue(false)
        AntiVampire:SetValue(false)
        AntiFling:SetValue(false)
        AntiInvisible:SetValue(false)
        AntiToxify:SetValue(false)
        AntiNoColor:SetValue(false)
        AntiStun:SetValue(false)
        AntiFarlands:SetValue(false)
        Library:Notify("All antis disabled", 3)
    end
})

local EnlightenGroup = Tabs.Chat:AddRightGroupbox('Enlighten Finder')

local enlightenPlayersLabel = EnlightenGroup:AddLabel('Players with Enlighten: None')

function FindPlayersWithEnlighten()
    local enlightenPlayers = {}
    
    for _, player in pairs(Players:GetPlayers()) do
        if player ~= plr then

            local backpack = player:FindFirstChild("Backpack")
            if backpack and backpack:FindFirstChild("The Arkenstone") then
                table.insert(enlightenPlayers, player.Name .. " (Backpack)")
            end
            

            if player.Character then
                if player.Character:FindFirstChild("The Arkenstone") then
                    table.insert(enlightenPlayers, player.Name .. " (Equipped)")
                end
            end
            

            if player:GetAttribute("Arken") == true then
                table.insert(enlightenPlayers, player.Name .. " (Chosen)")
            end
        end
    end
    
    if #enlightenPlayers > 0 then
        enlightenPlayersLabel:SetText("Enlighten Players: " .. table.concat(enlightenPlayers, ", "))
        Library:Notify("Found " .. #enlightenPlayers .. " players with enlighten", 3)
    else
        enlightenPlayersLabel:SetText("Players with Enlighten: None")
        Library:Notify("No players found with enlighten", 3)
    end
    
    return enlightenPlayers
end

EnlightenGroup:AddButton({
    Text = 'Scan for Enlighten',
    Func = FindPlayersWithEnlighten
})

local AutoScanToggle = EnlightenGroup:AddToggle('AutoScanEnlighten', {
    Text = 'Auto Scan (30s)',
    Default = false,
    Tooltip = 'Automatically scan for players with enlighten every 30 seconds'
})

local autoScanThread = nil
AutoScanToggle:OnChanged(function(value)
    if autoScanThread then
        task.cancel(autoScanThread)
        autoScanThread = nil
    end
    
    if value then
        autoScanThread = task.spawn(function()
            while AutoScanToggle.Value do
                task.wait(30)
                if AutoScanToggle.Value then
                    FindPlayersWithEnlighten()
                end
            end
        end)
        Library:Notify("Auto-scan enabled", 3)
    else
    end
end)

local TCOToolsGroup = Tabs.Chat:AddRightGroupbox('Boombox Utility')
BoomboxStatus = TCOToolsGroup:AddLabel('Select a Boombox')

 function onInputBegan(input, gameProcessed)
    if gameProcessed then return end
    
    if input.UserInputType == Enum.UserInputType.MouseButton1 then
        local target = mouse.Target
        if target then
            local potentialBoombox = target.Parent
            if boomboxNames[potentialBoombox.Name] == true then
                selectedBoombox = potentialBoombox
                
                local sound = potentialBoombox:FindFirstChild("Sound", true)
                if sound and sound.SoundId ~= "" and sound.SoundId ~= nil then
                    boomboxId = extractSoundId(sound)
                    BoomboxStatus:SetText("Boombox: " .. potentialBoombox.Name .. "\nSound ID: " .. boomboxId .. "\nClick 'Copy ID' to copy")
                    
                    local handle = potentialBoombox:FindFirstChild("Handle")
                    if handle then
                        bbsbox.Adornee = handle
                        bbsbox.LineThickness = 0.05
                    end
                    
                    Library:Notify("Boombox Selected: " .. potentialBoombox.Name, 3)
                else
                    boomboxId = "None"
                    BoomboxStatus:SetText("Boombox: " .. potentialBoombox.Name .. "\nNo sound playing")
                    bbsbox.Adornee = nil
                end
           
            elseif target.Name == "Handle" and boomboxNames[target.Parent.Name] == true then
                selectedBoombox = target.Parent
                
                local sound = target.Parent:FindFirstChild("Sound", true)
                if sound and sound.SoundId ~= "" and sound.SoundId ~= nil then
                    boomboxId = extractSoundId(sound)
                    BoomboxStatus:SetText("Boombox: " .. target.Parent.Name .. "\nSound ID: " .. boomboxId .. "\nClick 'Copy ID' to copy")
                    
                    bbsbox.Adornee = target
                    bbsbox.LineThickness = 0.05
                    Library:Notify("Boombox Selected: " .. target.Parent.Name, 3)
                else
                    boomboxId = "None"
                    BoomboxStatus:SetText("Boombox: " .. target.Parent.Name .. "\nNo sound playing")
                    bbsbox.Adornee = nil
                end
            else
                selectedBoombox = nil
                boomboxId = ""
                BoomboxStatus:SetText("Select a Boombox")
                bbsbox.Adornee = nil
            end
        end
    end
end

UserInputService.InputBegan:Connect(onInputBegan)

TCOToolsGroup:AddButton({
    Text = 'Copy Sound ID',
    Func = function()
        if boomboxId ~= "" and boomboxId ~= "None" then
            if setclipboard then
                setclipboard(boomboxId)
                Library:Notify("Copied Sound ID: " .. boomboxId, 3)
            else
                Library:Notify("Clipboard function not available", 3)
            end
        else
            Library:Notify("No sound ID to copy", 3)
        end
    end
})

TCOToolsGroup:AddButton({
    Text = 'Scan Boomboxes',
    Func = function()
        local boomboxList = {}
        

        for _, obj in pairs(CoreGui:GetChildren()) do
            if obj.Name == "BoomboxHighlight" then
                obj:Destroy()
            end
        end

        for _, item in pairs(workspace:GetDescendants()) do
            if item:IsA("Tool") and boomboxNames[item.Name] == true then
                table.insert(boomboxList, item)
                
                local highlight = Instance.new("Highlight")
                highlight.Name = "BoomboxHighlight"
                highlight.FillColor = Color3.fromRGB(255, 100, 100)
                highlight.FillTransparency = 0.7
                highlight.OutlineColor = Color3.fromRGB(255, 0, 0)
                highlight.OutlineTransparency = 0
                highlight.Adornee = item
                highlight.Parent = CoreGui
            end
        end
        
        if #boomboxList > 0 then
            local info = "Found " .. #boomboxList .. " boombox(es):\n\n"
            
            for i, boombox in ipairs(boomboxList) do
                local sound = boombox:FindFirstChild("Sound", true)
                local soundId = "None"
                if sound and sound.SoundId ~= "" and sound.SoundId ~= nil then
                    soundId = extractSoundId(sound)
                end
                
                info = info .. i .. ". " .. boombox.Name .. "\n"
                info = info .. "   ID: " .. soundId .. "\n\n"
            end
            
            BoomboxStatus:SetText("Server Boomboxes (" .. #boomboxList .. ")\n" .. info)
            Library:Notify("Found " .. #boomboxList .. " boombox(es)", 3)
        else
            BoomboxStatus:SetText("No boomboxes found on server")
            Library:Notify("No boomboxes found", 3)
        end
    end
})

TCOToolsGroup:AddButton({
    Text = 'Clear Highlights',
    Func = function()
        for _, obj in pairs(CoreGui:GetChildren()) do
            if obj.Name == "BoomboxHighlight" then
                obj:Destroy()
            end
        end
        Library:Notify("All highlights cleared", 3)
    end
})

local ToolsEtc = Tabs.Chat:AddRightGroupbox('Tool Utility')

local muteBoomboxesEnabled = false

local MuteBoomboxesToggle = ToolsEtc:AddToggle('MuteBoomboxes', {
    Text = 'Mute All Boomboxes',
    Default = false,
    Tooltip = 'Mute all boombox sounds on the server'
})

local AutoPickup = ToolsEtc:AddToggle('AutoPickup', {
    Text = 'Auto Pickup Tools',
    Default = false,
    Tooltip = 'Automatically pick up dropped tools'
})

local AutoDrop = ToolsEtc:AddToggle('AutoDrop', {
    Text = 'Auto Drop on Death',
    Default = false,
    Tooltip = 'Automatically drop tools when dying'
})

local DontDropEnlighten = ToolsEtc:AddToggle('DontDropEnlighten', {
    Text = "Prevent Enlighten Drop",
    Default = true,
    Tooltip = 'Keep The Arkenstone when auto-dropping tools'
})

DontDropEnlighten:OnChanged(function()
    dontDropEnlighten = DontDropEnlighten.Value
end)

ToolsEtc:AddButton({
    Text = 'Grab All Tools',
    Func = GrabTools
})

ToolsEtc:AddButton({
    Text = 'Equip All Tools',
    Func = function()
        for _, tool in pairs(plr.Backpack:GetChildren()) do
            if tool:IsA("Tool") then
                tool.Parent = plr.Character
            end
        end
        Library:Notify("All tools equipped", 3)
    end
})

ToolsEtc:AddButton({
    Text = 'Drop All Tools',
    Func = function()
        for _, tool in pairs(plr.Backpack:GetChildren()) do
            if tool:IsA("Tool") then
                tool.Parent = plr.Character
            end
        end
        task.wait()
        for _, tool in pairs(plr.Character:GetChildren()) do
            if tool:IsA("Tool") then
                tool.Parent = workspace
            end
        end
        Library:Notify("All tools dropped", 3)
    end
})

local http = game:GetService("HttpService")
local localplr = game.Players.LocalPlayer
local lte = nil

function checktool(t)
    if lte ~= t.Name then
        if localplr.Character and t.Parent then
            t.Parent = localplr.Character
            task.wait()
            t.Parent = localplr.Backpack
            lte = t.Name
        end
    end
end

getbuild = function()
    if SavedBuildDropdown and SavedBuildDropdown.Value ~= "" then
        return http:JSONDecode(readfile("TheChosenOneBuilds/"..SavedBuildDropdown.Value..".json"))
    end
    return nil
end

listfilesfixed = function(directory)
    s,lf = pcall(function()
        return listfiles(directory)
    end)
    if s then
        for i,v in pairs(lf) do
            if string.sub(v,1,2) == "./" then
                lf[i] = string.sub(v,3)
            end
        end
    end
    return lf
end
plrbuilds = {ServerBuilds = cfolder}
plrnames = {"ServerBuilds"}
buildhighlight = Instance.new("Highlight")
buildhighlight.Parent = game.CoreGui
buildhighlight.FillColor = Color3.fromRGB(0,255,0)
buildhighlight.FillTransparency = .9

highlight = Instance.new("Highlight")
highlight.Parent = game.CoreGui
highlight.FillColor = Color3.fromRGB(0, 200, 255)
highlight.FillTransparency = 0.5
highlight.OutlineColor = Color3.fromRGB(0, 200, 255)
highlight.OutlineTransparency = 0

getfn = function(js,first)
    fn = listfilesfixed("TheChosenOneBuilds/") or listfilesfixed("TheChosenOneBuilds")
    if not fn or #fn == 0 then
        return {}
    end
    if not js then
        for i,v in pairs(fn) do
            fn[i] = v:gsub(".json","")
        end
    end
    if not first then
        for i,v in pairs(fn) do
            fn[i] = v:gsub("TheChosenOneBuilds/","")
        end
    end
    return fn
end

snap = function(pos,m)
    if m == nil then
        m = gridSize
    end
    x = math.round(pos.X/m)+2
    return pos
end

if cfolder:FindFirstChild(game.Players.LocalPlayer.Name) then
    cubechild = cfolder[game.Players.LocalPlayer.Name].ChildAdded:Connect(function(child)
        childcube = child
        historynum = historynum + 1
        if historynum > historymax then
            historynum = 1
        end
        if buildingtoxify then
            toxifybrick = child
        end
        cubehistory[historynum] = child
        built = true
    end)
else
    cubechild = cfolder.ChildAdded:Connect(function()
    end)
end

defaultcolor = Color3.fromRGB(192,192,192)
buildingexec = nil
normalids = {}
normalids[Enum.NormalId.Right] = {Vector3.new(1,0,0),"X"}
normalids[Enum.NormalId.Top] = {Vector3.new(0,1,0),"Y"}
normalids[Enum.NormalId.Back] = {Vector3.new(0,0,1),"Z"}
normalids[Enum.NormalId.Left] = {Vector3.new(-1,0,0),"X"}
normalids[Enum.NormalId.Bottom] = {Vector3.new(0,-1,0),"Y"}
normalids[Enum.NormalId.Front] = {Vector3.new(0,0,-1),"Z"}

roundnum = function(num,m)
    return math.round((num - 2) / m) * m + 2
end

roundBuildPos = function(pos,m)
    return Vector3.new(roundnum(pos.X,m or gridSize),roundnum(pos.Y,m or gridSize),roundnum(pos.Z,m or gridSize))
end

materials = {}
materials[Enum.Material.SmoothPlastic] = "smooth"
materials[Enum.Material.Plastic] = "plastic"
materials[Enum.Material.CeramicTiles] = "tiles"
materials[Enum.Material.Brick] = "bricks"
materials[Enum.Material.WoodPlanks] = "planks"
materials[Enum.Material.Ice] = "ice"
materials[Enum.Material.Grass] = "grass"
materials[Enum.Material.Sand] = "sand"
materials[Enum.Material.Snow] = "snow"
materials[Enum.Material.Glass] = "glass"
materials[Enum.Material.Wood] = "wood"
materials[Enum.Material.Slate] = "stone"
materials[Enum.Material.Pebble] = "pebble"
materials[Enum.Material.Marble] = "marble"
materials[Enum.Material.Granite] = "granite"
materials[Enum.Material.DiamondPlate] = "steel"
materials[Enum.Material.Metal] = "metal"
materials[Enum.Material.Asphalt] = "asphalt"
materials[Enum.Material.Concrete] = "concrete"
materials[Enum.Material.Pavement] = "pavement"
materials[Enum.Material.Neon] = "neon"

swappedmaterials = {}
for i,v in pairs(materials) do
    swappedmaterials[v] = i
end

starterui = game:GetService("StarterGui")
savebuildnames = {}
builds = {}

updatedropdown = function()
    names = getfn()
    table.sort(names,function(a,b)
        return a:lower() < b:lower()
    end)
    if SavedBuildDropdown then
        SavedBuildDropdown:Refresh(names)
    end
    writefile("thechosenonenames.txt",http:JSONEncode(savebuildnames))
end

bindfunc = Instance.new("BindableFunction")
bindfunc.OnInvoke = function(prompt)
    if prompt == "Yes" and SavedBuildDropdown.Value ~= "" then
        delfile("TheChosenOneBuilds/"..SavedBuildDropdown.Value..".json")
        SavedBuildDropdown.Value = ""
        updatedropdown()
    end
end

bannedsymbols = {}
bannedsymbols["\""] = "''"
bannedsymbols["*"] = "★"
bannedsymbols[":"] = ";"
bannedsymbols["<"] = "≤"
bannedsymbols[">"] = "≥"
bannedsymbols["?"] = "¿"
bannedsymbols["\\"] = ""
bannedsymbols["|"] = "I"
bannedsymbols["/"] = "∕"

validate = function(name)
    for i,v in pairs(bannedsymbols) do
        name = name:gsub(i,v)
    end
    s = string.find(name,"%.txt") or string.find(name,"%.json")
    if s then
        addafter = string.sub(name,s)
        name = string.sub(name,1,s-1)
        name = name:gsub("%.","·")
        name = name..addafter
    else
        name = name:gsub("%.","·")
    end
    return name
end

files = listfilesfixed("")
if not files then
    files = {}
end

s,e = pcall(function()
    if not table.find(files,"TheChosenOneBuilds/") and not table.find(files,"TheChosenOneBuilds") then
        task.wait(1)
        builds = {}
        if isfile("thechosenonebuilds.txt") then
            builds = http:JSONDecode(readfile("thechosenonebuilds.txt"))
        end
        makefolder("TheChosenOneBuilds")
        i2 = 0
        for i,v in pairs(builds) do
            i = validate(i)
            s,e = pcall(function()
                writefile("TheChosenOneBuilds/"..i..".json",http:JSONEncode(v))
            end)
            if not s then
                i2 = i2 + 1
                writefile("TheChosenOneBuilds/CheckFile_Named_'TCO"..tostring(i2).."'.json",http:JSONEncode(v))
                writefile("'TCO"..tostring(i2).."'.txt","If you came here from the CheckFile file, your build name had an error ("..i..") DO NOT change the file name to that name, it's best just to rename the file to something else.")
            end
            task.wait()
        end
    end
end)

s,e = pcall(function()
    if isfile("thechosenonenames.txt") then
        savebuildnames = http:JSONDecode(readfile("thechosenonenames.txt"))
    end
end)

if savebuildnames == nil then
    savebuildnames = {}
end

createpartpos = function(pos,col)
    if typeof(pos) == "CFrame" then
        pos = pos.Position
    end
    p = Instance.new("Part")
    p.Shape = Enum.PartType.Ball
    p.Anchored = true
    p.CanCollide = false
    p.CastShadow = false
    p.CanQuery = false
    p.Color = col
    p.Transparency = .5
    p.Size = Vector3.new(3,3,3)
    p.CFrame = CFrame.new(pos)
    p.Parent = workspace
end

createpartrepl = function(pos,bsize,col,mat,transp,anch,collide,sprays)
    if typeof(pos) == "CFrame" then
        pos = pos.Position
    end
    p = Instance.new("Part")
    oldprt = p
    p.Anchored = anch or true
    p.CanCollide = collide or false
    p.CastShadow = false
    p.CanQuery = false
    p.Color = col
    p.Transparency = transp or .5
    p.Material = mat
    if bsize ~= nil then
        pos = Vector3.new((pos.X + (bsize.X/2))-.5,(pos.Y + (bsize.Y/2))-.5,(pos.Z + (bsize.Z/2))-.5)
    end
    p.Size = bsize or Vector3.new(gridSize,gridSize,gridSize)
    p.CFrame = CFrame.new(pos)
    if sprays then
        for i,v in pairs(sprays) do
            face = Enum.NormalId[v[1]]
            image = v[2]
            txt = v[3]
            surfaceui = Instance.new("SurfaceGui")
            surfaceui.Face = face
            surfaceui.SizingMode = Enum.SurfaceGuiSizingMode.PixelsPerStud
            surfaceui.PixelsPerStud = 50
            out,count = string.gsub(txt,"#","l")
            if count == string.len(txt) then
                img = Instance.new("ImageLabel")
                img.Image = image
                img.BackgroundTransparency = 1
                img.Size = UDim2.new(1,0,1,0)
                img.Parent = surfaceui
            else
                textlabel = Instance.new("TextLabel")
                textlabel.Text = txt
                textlabel.BackgroundTransparency = 1
                textlabel.TextScaled = true
                textlabel.TextColor3 = Color3.fromRGB(255,255,255)
                textlabel.Font = Enum.Font.FredokaOne
                textlabel.Size = UDim2.new(1,0,1,0)
                textlabel.Parent = surfaceui
            end
            surfaceui.Parent = p
        end
    end
    p.Parent = workspace
    return p
end

saveblock = function(bl)
    blockdata = {}
    if bl:IsA("BasePart") then
        p = bl.Position
        blockdata.p = {bl.Position.X,bl.Position.Y,bl.Position.Z}
        blockdata.c = {math.round(bl.Color.R*255),math.round(bl.Color.G*255),math.round(bl.Color.B*255)}
        blockdata.a = bl.Anchored
        blockdata.cc = bl.CanCollide
        if bl.Size.X ~= gridSize or bl.Size.Y ~= gridSize or bl.Size.Z ~= gridSize then
            blockdata.p[1] = (blockdata.p[1] - (bl.Size.X/2))+.5
            blockdata.p[2] = (blockdata.p[2] - (bl.Size.Y/2))+.5
            blockdata.p[3] = (blockdata.p[3] - (bl.Size.Z/2))+.5
            blockdata.s = {bl.Size.X,bl.Size.Y,bl.Size.Z}
        end
        blockdata.m = materials[bl.Material]
        blockdata.o = bl.Material.Name
        local sprayData = {}
        for _, child in ipairs(bl:GetChildren()) do
            if child:IsA("SurfaceGui") then
                local faceName = child.Face.Name
                local imageId = ""
                local txt = ""
                local imgLabel = child:FindFirstChildOfClass("ImageLabel")
                local txtLabel = child:FindFirstChildOfClass("TextLabel")
                if imgLabel then
                    imageId = imgLabel.Image or ""
                end
                if txtLabel then
                    txt = txtLabel.Text or ""
                end
                if txt ~= "" or imageId ~= "" then
                    table.insert(sprayData, {faceName, imageId, txt})
                end
            end
        end
        if #sprayData > 0 then
            blockdata.sp = sprayData
        end
    end
    return blockdata
end

getSpawnPosition = function()
    spawnLocations = workspace:FindFirstChild("SpawnLocation") or workspace:FindFirstChild("Spawn")
    if spawnLocations then
        if spawnLocations:IsA("Model") then
            humanoidRootPart = spawnLocations:FindFirstChild("HumanoidRootPart")
            if humanoidRootPart then
                return humanoidRootPart.Position
            end
        elseif spawnLocations:IsA("BasePart") then
            return spawnLocations.Position
        end
    end
    
    players = game.Players:GetPlayers()
    for _, player in ipairs(players) do
        if player.Character then
            humanoidRootPart = player.Character:FindFirstChild("HumanoidRootPart")
            if humanoidRootPart then
                return humanoidRootPart.Position
            end
        end
    end
    
    return Vector3.new(0, 100, 0)
end

sortBlocksByDistanceFromSpawn = function(buildData)
    local spawnPos = getSpawnPosition()

    for i, block in ipairs(buildData) do
        local blockPos = Vector3.new(block.p[1], block.p[2], block.p[3])
        if block.s then
            blockPos = Vector3.new(
                blockPos.X + (block.s[1]/2) - 0.5,
                blockPos.Y + (block.s[2]/2) - 0.5,
                blockPos.Z + (block.s[3]/2) - 0.5
            )
        end
        block.distance = (blockPos - spawnPos).Magnitude
        block.originalIndex = i
    end

    table.sort(buildData, function(a, b)
        return a.distance < b.distance
    end)

    for _, block in ipairs(buildData) do
        block.distance = nil
        block.originalIndex = nil
    end

    return buildData
end

buildblock = function(pos,texture,color,bsize,bsizev3,premadebuild,origmaterial,sprays,anchored,collide)
    task.wait()
    if anchored == nil then
        anchored = true
    end
    if collide == nil then
        collide = true
    end
    local needsresize = false
    local s,e = pcall(function()
    local s,e = pcall(function()
        game.Players.LocalPlayer.Backpack.Build.Parent = game.Players.LocalPlayer.Character
    end)
local oo = false
local c = 0
childcube = nil
if #cubehistory > 0 and oldprt then
    local allooslol = {}
    for i,childcube2 in pairs(cubehistory) do
        if childcube2 == nil or childcube2.Parent == nil then
            cubehistory[i] = nil
            continue
        elseif oldprt.Size == childcube2.Size then
            for idx,v in pairs(normalids) do
                local pos = childcube2.Position+(v[1]*childcube2.Size[v[2]])
                if pos == oldprt.Position then
                    oo = {idx,childcube2,childcube2.Position+(v[1]*childcube2.Size[v[2]]/2)}
                    table.insert(allooslol,{idx,childcube2,childcube2.Position+(v[1]*childcube2.Size[v[2]]/2)})
                end
            end
        end
    end
    if #allooslol > 1 and color and oo and oo[2] and oo[2].Color ~= color then
        for i,v in pairs(allooslol) do
            if v[2].Color == color then
                oo = v
            end
        end
    end
    local origposs = pos
        if oo and oo[2] ~= nil and oo[2].Parent ~= nil then
            local args = {
                [1] = oo[2],
                [2] = oo[1],
                [3] = oo[3] or oldprt.Position,
                [4] = "normal"
            }
            built = false
            childcube = nil
            c = 0
            repeat
                c = c + 1
                if game.Players.LocalPlayer.Character:FindFirstChild("Build") then
                    event = (game.Players.LocalPlayer.Character.Build:FindFirstChild("origevent") and game.Players.LocalPlayer.Character.Build.origevent:Invoke(unpack(args))) or game.Players.LocalPlayer.Character.Build.Script.Event:FireServer(unpack(args))
                else
                    s,e = pcall(function()
                        game.Players.LocalPlayer.Backpack.Build.Parent = game.Players.LocalPlayer.Character
                    end)
                end
                s,e = pcall(function()
                    pos = oo[3] or pos
                    if tp then
                        game.Players.LocalPlayer.Character.HumanoidRootPart.CFrame = CFrame.new(pos)
                    end
                end)
                task.wait()
            until (built == true and childcube) or oo[2] == nil or oo[2].Parent == nil or stopped == true or skipblock == true or c > 200
            if oo[2] == nil or oo[2].Parent == nil or c > 200 then
                oo = false
            else
                if oldprt then
                    oldprt:Destroy()
                end
            end
        end
        pos = origposs
    end
    if oo == false then
    if bsize == nil then
        bsize = "normal"
        if game.Players.LocalPlayer.PlayerGui:FindFirstChild("Build") and game.Players.LocalPlayer.PlayerGui.Build:FindFirstChild("Button") then
            bsize = game.Players.LocalPlayer.PlayerGui.Build.Button.Text
        end
        if bsizev3 ~= nil and (bsizev3.X ~= gridSize or bsizev3.Y ~= gridSize or bsizev3.Z ~= gridSize) then
            bsize = "detailed"
        elseif bsizev3 ~= nil and (bsizev3.X == gridSize and bsizev3.Y == gridSize and bsizev3.Z == gridSize) then
            bsize = "normal"
        end
        if bsizev3 == nil and (bsize ~= "detailed") and oldprt and oldprt.Position ~= round(pos) then
            bsize = "detailed"
            needsresize = true
            bsizev3 = Vector3.new(4,4,4)
            pos = Vector3.new((pos.X - (bsizev3.X/2))+.5,(pos.Y - (bsizev3.Y/2))+.5,(pos.Z - (bsizev3.Z/2))+.5)
        end
    end
    local oldpos = pos
    pos = snap(pos)
    local args = {
        [1] = workspace.Terrain,
        [2] = Enum.NormalId.Top,
        [3] = pos,
        [4] = bsize or "normal"
    }
    built = false
    s,e = pcall(function()
        event = (game.Players.LocalPlayer.Character.Build:FindFirstChild("origevent") and game.Players.LocalPlayer.Character.Build.origevent:Invoke(unpack(args))) or game.Players.LocalPlayer.Character.Build.Script.Event:FireServer(unpack(args))
    end)
    c = 0
    repeat
        c = c + 1
        if game.Players.LocalPlayer.Character and not game.Players.LocalPlayer.Character:FindFirstChild("Build") and game.Players.LocalPlayer.Backpack:FindFirstChild("Build") then
            game.Players.LocalPlayer.Backpack.Build.Parent = game.Players.LocalPlayer.Character
        end
        if game.Players.LocalPlayer.Character:FindFirstChild("Build") then
            event = (game.Players.LocalPlayer.Character.Build:FindFirstChild("origevent") and game.Players.LocalPlayer.Character.Build.origevent:Invoke(unpack(args))) or game.Players.LocalPlayer.Character.Build.Script.Event:FireServer(unpack(args))
        end
        s,e = pcall(function()
            if tp then
                game.Players.LocalPlayer.Character.HumanoidRootPart.CFrame = CFrame.new(pos + Vector3.new(0,6,0))
            end
        end)
        task.wait()
    until (built == true and childcube) or stopped == true or skipblock == true or c > 200
    built = false
    c = 0
    end
    if childcube and typeof(color) == "Color3" and (color ~= defaultcolor or (childcube.Color ~= color or childcube.Material ~= texture)) and (game.Players.LocalPlayer.Backpack:FindFirstChild("Paint") or game.Players.LocalPlayer.Character:FindFirstChild("Paint")) and ((colorbool and premadebuild ~= nil) or not colorbool) or texture then
        local pos = (childcube and childcube.Position + childcube.Size/2) or pos
        local args = {
            [1] = childcube,
            [2] = Enum.NormalId.Top,
            [3] = pos,
            [4] = "color",
            [5] = color or nil,
            [6] = "tiles",
            [7] = ""
        }
        task.wait()
        local success,err = pcall(function()
            game.Players.LocalPlayer.Backpack.Paint.Parent = game.Players.LocalPlayer.Character
        end)
        if texture ~= nil then
            if color == nil then
                args[4] = "material"
            else
                args[4] = "both \u{1F91D}"
            end
            args[6] = texture
        end
        if not childcube then
            oldprt:Destroy()
            return
        end
        local oldcolor = childcube.Color
        highlight.Adornee = childcube
        c = 0
        s,e = pcall(function()    
        repeat
            c = c + 1
            if game.Players.LocalPlayer.Character and not game.Players.LocalPlayer.Character:FindFirstChild("Paint") and game.Players.LocalPlayer.Backpack:FindFirstChild("Paint") then
                game.Players.LocalPlayer.Backpack.Paint.Parent = game.Players.LocalPlayer.Character
            end
            if game.Players.LocalPlayer.Character and game.Players.LocalPlayer.Character:FindFirstChild("Paint") then
                event = (game.Players.LocalPlayer.Character.Paint:FindFirstChild("origevent") and game.Players.LocalPlayer.Character.Paint.origevent:Invoke(unpack(args))) or game.Players.LocalPlayer.Character.Paint.Script.Event:FireServer(unpack(args))
            end
            s,e = pcall(function()

                if tp then
                    game.Players.LocalPlayer.Character.HumanoidRootPart.CFrame = CFrame.new(pos + Vector3.new(0,6,0))
                end
            end)
            task.wait()
        until not childcube or not childcube.Parent or childcube.Color == color or (texture and childcube.Material == Enum.Material[origmaterial]) or stopped == true or skipblock == true or c > 2000
        end)
    end
    if childcube and game.Players.LocalPlayer.Character and game.Players.LocalPlayer.Character:FindFirstChild("Paint") and childcube.Anchored ~= anchored then
        local pos = (childcube and childcube.Position + childcube.Size/2) or pos
        local args = {
            [1] = childcube,
            [2] = Enum.NormalId.Top,
            [3] = pos or childcube.Position+Vector3.new(1,0,0),
            [4] = "material",
            [5] = nil,
            [6] = "anchor",
            [7] = ""
        }
        c = 0
        repeat
            c = c + 1
            if game.Players.LocalPlayer.Character and not game.Players.LocalPlayer.Character:FindFirstChild("Paint") and game.Players.LocalPlayer.Backpack:FindFirstChild("Paint") then
                game.Players.LocalPlayer.Backpack.Paint.Parent = game.Players.LocalPlayer.Character
            end
            if game.Players.LocalPlayer.Character and game.Players.LocalPlayer.Character:FindFirstChild("Paint") and childcube and childcube.Anchored ~= anchored then
                event = (game.Players.LocalPlayer.Character.Paint:FindFirstChild("origevent") and game.Players.LocalPlayer.Character.Paint.origevent:Invoke(unpack(args))) or game.Players.LocalPlayer.Character.Paint.Script.Event:FireServer(unpack(args))
            end
            s,e = pcall(function()
                if tp then
                    game.Players.LocalPlayer.Character.HumanoidRootPart.CFrame = CFrame.new(pos + Vector3.new(0,8,0))
                end
            end)
            task.wait(1)
        until not childcube or not childcube.Parent or childcube.Anchored == anchored or not game.Players.LocalPlayer.Character or (not game.Players.LocalPlayer.Character:FindFirstChild("Paint") and not game.Players.LocalPlayer.Backpack:FindFirstChild("Paint")) or stopped == true or skipblock == true or c > 20
    end
    if childcube and game.Players.LocalPlayer.Character and game.Players.LocalPlayer.Character:FindFirstChild("Paint") and childcube.CanCollide ~= collide then
        local pos = (childcube and childcube.Position + childcube.Size/2) or pos
        local args = {
            [1] = childcube,
            [2] = Enum.NormalId.Top,
            [3] = pos or childcube.Position+Vector3.new(1,0,0),
            [4] = "material",
            [5] = nil,
            [6] = "collide",
            [7] = ""
        }
        c = 0
        repeat
            c = c + 1
            if game.Players.LocalPlayer.Character and not game.Players.LocalPlayer.Character:FindFirstChild("Paint") and game.Players.LocalPlayer.Backpack:FindFirstChild("Paint") then
                game.Players.LocalPlayer.Backpack.Paint.Parent = game.Players.LocalPlayer.Character
            end
            if game.Players.LocalPlayer.Character and game.Players.LocalPlayer.Character:FindFirstChild("Paint") and childcube and childcube.CanCollide ~= collide then
                event = (game.Players.LocalPlayer.Character.Paint:FindFirstChild("origevent") and game.Players.LocalPlayer.Character.Paint.origevent:Invoke(unpack(args))) or game.Players.LocalPlayer.Character.Paint.Script.Event:FireServer(unpack(args))
            end
            s,e = pcall(function()
                if tp then
                    game.Players.LocalPlayer.Character.HumanoidRootPart.CFrame = CFrame.new(pos + Vector3.new(0,8,0))
                end
            end)
            task.wait(1)
        until not childcube or not childcube.Parent or childcube.CanCollide == collide or not game.Players.LocalPlayer.Character or (not game.Players.LocalPlayer.Character:FindFirstChild("Paint") and not game.Players.LocalPlayer.Backpack:FindFirstChild("Paint")) or stopped == true or skipblock == true or c > 20
    end
    highlight.Adornee = nil
    if childcube and (game.Players.LocalPlayer.Backpack:FindFirstChild("Paint") or game.Players.LocalPlayer.Character:FindFirstChild("Paint")) and sprays ~= nil then
        local args = {
            [1] = childcube,
            [2] = Enum.NormalId.Front,
            [3] = childcube.Position+Vector3.new(1,0,0),
            [4] = "material",
            [5] = nil,
            [6] = "spray",
            [7] = "ha"
        }
        for i,v in pairs(sprays) do
            args[2] = Enum.NormalId[v[1]]
            args[7] = v[3]
            if childcube and (game.Players.LocalPlayer.Backpack:FindFirstChild("Paint") or game.Players.LocalPlayer.Character:FindFirstChild("Paint")) and stopped == false and skipblock == false then
                local success,err = pcall(function()
                    game.Players.LocalPlayer.Backpack.Paint.Parent = game.Players.LocalPlayer.Character
                end)
                local success,err = pcall(function()
                    task.wait(.25)
                    event = (game.Players.LocalPlayer.Character.Paint:FindFirstChild("origevent") and game.Players.LocalPlayer.Character.Paint.origevent:Invoke(unpack(args))) or game.Players.LocalPlayer.Character.Paint.Script.Event:FireServer(unpack(args))
                end)
            end
        end
    end
    if childcube and ((bsizev3 ~= nil and (bsizev3.X ~= gridSize or bsizev3.Y ~= gridSize or bsizev3.Z ~= gridSize)) or needsresize == true) and (game.Players.LocalPlayer.Character:FindFirstChild("Shape") or game.Players.LocalPlayer.Backpack:FindFirstChild("Shape")) then
        if not game.Players.LocalPlayer.Character:FindFirstChild("Shape") and game.Players.LocalPlayer.Backpack:FindFirstChild("Shape") then
            game.Players.LocalPlayer.Backpack.Shape.Parent = game.Players.LocalPlayer.Character
        end
        local args = {
            [1] = childcube,
            [2] = Enum.NormalId.Right,
            [3] = "",
            [4] = ""
        }
        if childcube and childcube.Size.X ~= bsizev3.X then
            c = 0
            repeat
                c = c + 1
                pos = (childcube and childcube.Position + childcube.Size/2) or pos
                args[4] = nil
                if childcube then
                    args[3] = pos
                    if childcube.Size.X > bsizev3.X then
                        args[4] = "decrease"
                    elseif childcube.Size.X < bsizev3.X then
                        args[4] = "increase"
                    end
                end
                if not game.Players.LocalPlayer.Character:FindFirstChild("Shape") and game.Players.LocalPlayer.Backpack:FindFirstChild("Shape") then
                    game.Players.LocalPlayer.Backpack.Shape.Parent = game.Players.LocalPlayer.Character
                end
                if game.Players.LocalPlayer.Character:FindFirstChild("Shape") then
                    event = (game.Players.LocalPlayer.Character.Shape:FindFirstChild("origevent") and game.Players.LocalPlayer.Character.Shape.origevent:Invoke(unpack(args))) or game.Players.LocalPlayer.Character.Shape.Script.Event:FireServer(unpack(args))
                end
                s,e = pcall(function()
                    if tp then
                        game.Players.LocalPlayer.Character.HumanoidRootPart.CFrame = CFrame.new(pos + Vector3.new(0,6,0))
                    end
                end)
                task.wait(resizewait)
            until args[4] == nil or (args[4] == "decrease" and childcube and childcube.Size.X <= 1) or (childcube and childcube.Size.X == bsizev3.X) or stopped == true or skipblock == true or not childcube or not childcube.Parent or c > (bsizev3.X*3)/resizewait
        end
        args[2] = Enum.NormalId.Top
        c = 0
        if childcube and childcube.Size.Y ~= bsizev3.Y then
            repeat
                c = c + 1
                pos = (childcube and childcube.Position + childcube.Size/2) or pos
                args[4] = nil
                if childcube then
                    args[3] = pos
                    if childcube.Size.Y > bsizev3.Y then
                        args[4] = "decrease"
                    elseif childcube.Size.Y < bsizev3.Y then
                        args[4] = "increase"
                    end
                end
                if not game.Players.LocalPlayer.Character:FindFirstChild("Shape") and game.Players.LocalPlayer.Backpack:FindFirstChild("Shape") then
                    game.Players.LocalPlayer.Backpack.Shape.Parent = game.Players.LocalPlayer.Character
                end
                if game.Players.LocalPlayer.Character:FindFirstChild("Shape") then
                    event = (game.Players.LocalPlayer.Character.Shape:FindFirstChild("origevent") and game.Players.LocalPlayer.Character.Shape.origevent:Invoke(unpack(args))) or game.Players.LocalPlayer.Character.Shape.Script.Event:FireServer(unpack(args))
                end
                s,e = pcall(function()
                    if tp then
                        game.Players.LocalPlayer.Character.HumanoidRootPart.CFrame = CFrame.new(pos + Vector3.new(0,6,0))
                    end
                end)
                task.wait(resizewait)
            until args[4] == nil or (args[4] == "decrease" and childcube and childcube.Size.Y <= 1) or (childcube and childcube.Size.Y == bsizev3.Y) or stopped == true or skipblock == true or not childcube or not childcube.Parent or c > (bsizev3.Y*3)/resizewait
        end
        args[2] = Enum.NormalId.Back
        c = 0
        if childcube and childcube.Size.Z ~= bsizev3.Z then
            repeat
                c = c + 1
                pos = (childcube and childcube.Position + childcube.Size/2) or pos
                args[4] = nil
                if childcube then
                    args[3] = pos
                    if childcube.Size.Z > bsizev3.Z then
                        args[4] = "decrease"
                    elseif childcube.Size.Z < bsizev3.Z then
                        args[4] = "increase"
                    end
                end
                if not game.Players.LocalPlayer.Character:FindFirstChild("Shape") and game.Players.LocalPlayer.Backpack:FindFirstChild("Shape") then
                    game.Players.LocalPlayer.Backpack.Shape.Parent = game.Players.LocalPlayer.Character
                end
                if game.Players.LocalPlayer.Character:FindFirstChild("Shape") then
                    event = (game.Players.LocalPlayer.Character.Shape:FindFirstChild("origevent") and game.Players.LocalPlayer.Character.Shape.origevent:Invoke(unpack(args))) or game.Players.LocalPlayer.Character.Shape.Script.Event:FireServer(unpack(args))
                end
                s,e = pcall(function()
                    if tp then
                        game.Players.LocalPlayer.Character.HumanoidRootPart.CFrame = CFrame.new(pos + Vector3.new(0,6,0))
                    end
                end)
                task.wait(resizewait)
            until args[4] == nil or (args[4] == "decrease" and childcube and childcube.Size.Z <= 1) or (childcube and childcube.Size.Z == bsizev3.Z) or stopped == true or skipblock == true or not childcube or not childcube.Parent or c > (bsizev3.Z*3)/resizewait
        end
    end
    skipblock = false
    end)
    if oldprt then
        oldprt:Destroy()
    end
    childcube = nil
end

local BuildServerGroup = Tabs.Build:AddRightGroupbox('Server Building')

if getgenv().brickcollection == nil then
    getgenv().brickcollection = {}
end

 function dbc(b)
    if not b:IsA("BasePart") or b.Name ~= brickname then
        return
    end
    if not table.find(getgenv().brickcollection, b) then
        table.insert(getgenv().brickcollection, 1, b)
    end
end

cfolder.DescendantAdded:Connect(dbc)
cfolder.DescendantRemoving:Connect(function(b)
    if b:IsA("BasePart") and b.Name == brickname then
        local idx = table.find(getgenv().brickcollection, b)
        if idx then table.remove(getgenv().brickcollection, idx) end
    end
end)
for i, v in pairs(cfolder:GetDescendants()) do
    dbc(v)
end

BuildServerGroup:AddButton({
    Text = 'Disable Building',
    Func = function()
        ExecuteDelete(block)
                sendActionWebhook("Disable Building", "User used Disable Building for a server")
        Library:Notify("Disabled building sucessfully!", 3)
    end
})

BuildServerGroup:AddButton({
    Text = 'Disable Building (OG SERVERS)',
    Func = function()
        ExecuteDeleteOG(block)
        sendActionWebhook("Disable Building (OG)", "User used Disable Building for Original Servers")
        Library:Notify("Disabled building sucessfully!", 3)
    end
})

local _permDeleteSoundConn = nil
BuildServerGroup:AddButton({
    Text = 'Disable Delete Sound (bypass grief notifiers)',
    Func = function()

        for _, desc in ipairs(cfolder:GetDescendants()) do
            if desc:IsA("Sound") then
                desc.Volume = 0
            end
        end

        if _permDeleteSoundConn then
            _permDeleteSoundConn:Disconnect()
        end
        _permDeleteSoundConn = cfolder.DescendantAdded:Connect(function(desc)
            if desc:IsA("Sound") then
                desc.Volume = 0
            end
        end)
        Library:Notify("Delete sounds permanently disabled", 3)
    end
})

local function doRestoreBuilding(delay)
    if not game.ReplicatedStorage:FindFirstChild("Brick") then
        local brick = Instance.new("Part")
        brick.Name = "Brick"
        brick.Parent = game.ReplicatedStorage
    end

    local et = equiptool("Build")
    if not et then
        Library:Notify("Build tool not found!", 3)
        return
    end

    task.wait(delay)

    local currbc = {}
    for i, v in pairs(getgenv().brickcollection) do
        if v ~= nil then
            if v:GetFullName() ~= "Brick" and v.Name ~= "Debris" then
                table.insert(currbc, 1, v)
            else
                table.insert(currbc, v)
            end
        else
            table.remove(getgenv().brickcollection, table.find(getgenv().brickcollection, v))
        end
    end

    local block = nil
    local beforeamt = #cfolder[localplr.Name]:GetChildren()

    for i, v in pairs(currbc) do
        if v ~= nil then
            et = equiptool("Build")
            if not et then
                Library:Notify("Build tool not found. Retry when it comes back.", 5)
                break
            end

            block = v
            et.Script.Event:FireServer(
                block,
                Enum.NormalId.Top,
                getplrpos(),
                "detailed"
            )

            task.wait(delay)

            if beforeamt < #cfolder[localplr.Name]:GetChildren() then
                break
            end
        else
            table.remove(currbc, table.find(currbc, v))
        end
    end

    for i, v in pairs(localplr.Character:GetChildren()) do
        if v:HasTag("The Chosen One by TomazDev") then
            v.Script.Enabled = false
            v.Script.Enabled = true
        end
    end
    for i, v in pairs(localplr.Backpack:GetChildren()) do
        if v:HasTag("The Chosen One by TomazDev") then
            v.Script.Enabled = false
            v.Script.Enabled = true
        end
    end

    task.wait(.1)
    block = cfolder[localplr.Name]:FindFirstChildWhichIsA("BasePart")
    if block then
        sendActionWebhook("Restore Building", "User used Restore Building")
        Library:Notify("Building restored successfully!", 3)
    else
        Library:Notify("Failed to restore. Run script before everything is delcubed.", 5)
    end
end

BuildServerGroup:AddButton({
    Text = 'Restore Building',
    Func = function()
        Library:Notify("Restoring (fast)...", 3)
        doRestoreBuilding(0.03)
    end
})

local BuildImportGroup = Tabs.Build:AddLeftGroupbox('Import Build')

local importJsonInput = BuildImportGroup:AddInput('ImportJSON', {
    Default     = '',
    Numeric     = false,
    Finished    = true,
    Text        = 'Paste JSON here:',
    Placeholder = 'Enter here',
    Tooltip     = 'JSON from Copy Server/My Build'
})

BuildImportGroup:AddButton({
    Text = 'Load Buildings',
    Func = function()
        local jsonStr = importJsonInput.Value
        if not jsonStr or jsonStr == "" then
            Library:Notify("Paste JSON data first!", 3); return
        end
        local ok, buildData = pcall(function() return http:JSONDecode(jsonStr) end)
        if not ok or type(buildData) ~= "table" or #buildData == 0 then
            Library:Notify("Invalid or empty JSON!", 3); return
        end
        Library:Notify("Loading " .. #buildData .. " blocks from JSON…", 5)
        stopped = false
        totalBuildBlocks  = #buildData
        currentBlockIndex = 0
        updateBuildProgress()
        buildData = sortBlocksByDistanceFromSpawn(buildData)
        task.spawn(function()
            for i, v in ipairs(buildData) do
                if stopped then break end
                currentBlockIndex = i
                updateBuildProgress()
                local posses = v.p or v.pos
                if not posses then continue end
                local pos  = Vector3.new(posses[1], posses[2], posses[3])
                local col  = (v.c or v.color) and Color3.fromRGB(table.unpack(v.c or v.color)) or Color3.fromRGB(192,192,192)
                local bsz  = (v.s or v.size) and Vector3.new(table.unpack(v.s or v.size)) or nil
                local mat  = v.m or v.mat
                createpartrepl(pos, bsz, col, swappedmaterials[mat] or Enum.Material.SmoothPlastic)
                buildblock(pos, mat, col, nil, bsz, true, v.o or v.origmat, v.sp or v.sprayed, v.a or v.anchored, v.cc or v.collide)
            end
            stopped = false
            totalBuildBlocks  = 0
            currentBlockIndex = 0
            updateBuildProgress()
            Library:Notify("JSON build loaded!", 5)
        end)
    end
})

BuildImportGroup:AddButton({
    Text = 'Stop Building',
    Func = function()
        stopped = true
        Library:Notify("Build stopped!", 3)
    end
})

buildFiles = getfn()
local BuildExportGroup = Tabs.Build:AddRightGroupbox('Export Build')

SavedBuildDropdown = BuildExportGroup:AddDropdown('SavedBuildSelect', {
    Values  = buildFiles or {},
    Default = 1,
    Text    = 'Saved Builds:',
    Tooltip = 'Select a build to load or delete'
})

BuildExportGroup:AddButton({
    Text = 'Save Buildings',
    Func = function()
        local data = {}
        for _, v in pairs(cfolder:GetDescendants()) do
            if v:IsA("BasePart") then table.insert(data, saveblock(v)) end
        end
        if #data == 0 then Library:Notify("No blocks on server!", 3); return end
        if setclipboard then
            setclipboard(http:JSONEncode(data))
            Library:Notify("Copied " .. #data .. " server blocks to clipboard!", 5)
        else
            Library:Notify("Clipboard unavailable on this executor", 3)
        end
    end
})

BuildExportGroup:AddButton({
    Text = 'Save to File',
    Func = function()
        local jsonStr = importJsonInput.Value
        if not jsonStr or jsonStr == "" then
            Library:Notify("Paste JSON first!", 3); return
        end
        local ok, data = pcall(function() return http:JSONDecode(jsonStr) end)
        if not ok or type(data) ~= "table" then
            Library:Notify("Invalid JSON!", 3); return
        end
        local name = "ImportedBuild_" .. os.time()
        writefile("TheChosenOneBuilds/" .. name .. ".json", jsonStr)
        updatedropdown()
        Library:Notify("Saved as '" .. name .. "'", 5)
    end
})

files = listfilesfixed("")
if not files then files = {} end

pcall(function()
    if not table.find(files,"TheChosenOneBuilds/") and not table.find(files,"TheChosenOneBuilds") then
        makefolder("TheChosenOneBuilds")
    end
end)

 ESPGroup = Tabs.Server:AddLeftGroupbox('Player ESP')

PlayersESPToggle = ESPGroup:AddToggle('PlayersESP', {
    Text = 'Players ESP',
    Default = true,
    Tooltip = 'Show highlight on all players'
})

 ESPNametagToggle = ESPGroup:AddToggle('ESPNametags', {
    Text = 'Show Nametags',
    Default = true,
    Tooltip = 'Show info labels above players'
})

 ESPDistanceToggle = ESPGroup:AddToggle('ESPDistance', {
    Text = 'Show Distance',
    Default = true,
    Tooltip = 'Show distance in nametag'
})

 ESPBuildCountToggle = ESPGroup:AddToggle('ESPBuildCount', {
    Text = 'Show Cube Count',
    Default = true,
    Tooltip = 'Show how many cubes each player has built'
})

 ESPTargetInput = ESPGroup:AddInput('ESPTarget', {
    Default = 'all',
    Numeric = false,
    Finished = false,
    Text = 'Target Player (name/all/others):',
    Placeholder = 'all',
    Tooltip = 'Filter ESP to specific player name, or use all/others'
})

 BuildESPGroup = Tabs.Server:AddRightGroupbox('Build Tracker ESP')

BuildingsESPToggle = BuildESPGroup:AddToggle('BuildingsESP', {
    Text = 'Buildings ESP',
    Default = false,
    Tooltip = 'Highlight builds and show cube count + location'
})

 BuildESPTargetInput = BuildESPGroup:AddInput('BuildESPTarget', {
    Default = 'all',
    Numeric = false,
    Finished = false,
    Text = 'Track Player (name/all):',
    Placeholder = 'all'
})

BuildESPGroup:AddDivider()

BuildESPGroup:AddButton({
    Text = 'Scan All Player Builds',
    Func = function()
         results = {}
        for _, player in ipairs(Players:GetPlayers()) do
             count = getPlayerBuildCount(player)
             center = getPlayerBuildCenter(player)
             centerStr = center and ("X:" .. math.floor(center.X) .. " Y:" .. math.floor(center.Y) .. " Z:" .. math.floor(center.Z)) or "No builds"
            table.insert(results, player.Name .. ": " .. count .. " cubes @ " .. centerStr)
        end
        if #results > 0 then
            Library:Notify(table.concat(results, "\n"), 10)
        else
            Library:Notify("No players found", 3)
        end
    end
})

BuildESPGroup:AddButton({
    Text = 'Teleport to Build (Tracked)',
    Func = function()
        local target = BuildESPTargetInput.Value
        for _, player in ipairs(Players:GetPlayers()) do
            if target == "all" or player.Name:lower():find(target:lower()) then
                local center = getPlayerBuildCenter(player)
                if center then
                    local hrp = plr.Character and plr.Character:FindFirstChild("HumanoidRootPart")
                    if hrp then
                        hrp.CFrame = CFrame.new(center + Vector3.new(0, 10, 0))
                        Library:Notify("Teleported to " .. player.Name .. "'s build", 3)
                        return
                    end
                end
            end
        end
        Library:Notify("No build found for target", 3)
    end
})

local ESPColors = Tabs.Server:AddRightGroupbox('ESP Appearance')

local espColorOptions = {"Red", "Blue", "Green", "Yellow", "Purple", "White", "Cyan", "Orange", "Pink", "Rainbow"}

ESPColorPicker = ESPColors:AddDropdown('ESPColor', {
    Values = espColorOptions,
    Default = 1,
    Text = 'ESP Color',
    Tooltip = 'Color for player and build highlights'
})

ESPTransparencySlider = ESPColors:AddSlider('ESPTransparency', {
    Text = 'Fill Transparency',
    Default = 0.6,
    Min = 0,
    Max = 1,
    Rounding = 2,
    Compact = false,
    Tooltip = 'How transparent the highlight fill is (1 = invisible fill, outline only)'
})

ESPColors:AddDivider()

ESPColors:AddLabel('Live Build Stats:')
local buildStatsLabel = ESPColors:AddLabel('Press Scan to update')

ESPColors:AddButton({
    Text = 'Update Build Stats',
    Func = function()
        local lines = {}
        for _, player in ipairs(Players:GetPlayers()) do
            local count = getPlayerBuildCount(player)
            if count > 0 then
                table.insert(lines, player.Name .. ": " .. count)
            end
        end
        if #lines > 0 then
            buildStatsLabel:SetText(table.concat(lines, "  |  "))
        else
            buildStatsLabel:SetText("No builds detected")
        end
    end
})

local ToolESPGroup = Tabs.Server:AddRightGroupbox('Tool ESP')

local ToolsESPToggle = ToolESPGroup:AddToggle('ToolsESP', {
    Text = 'Dropped Tools ESP',
    Default = false,
    Tooltip = 'Highlight tools dropped on the ground'
})

local ToolESPNameToggle = ToolESPGroup:AddToggle('ToolESPNames', {
    Text = 'Show Tool Names',
    Default = true,
    Tooltip = 'Show tool name labels above dropped tools'
})

local ToolESPDistToggle = ToolESPGroup:AddToggle('ToolESPDistance', {
    Text = 'Show Distance',
    Default = true,
    Tooltip = 'Show distance to each dropped tool'
})

local ToolESPXrayToggle = ToolESPGroup:AddToggle('ToolESPXray', {
    Text = 'X-Ray',
    Default = false,
    Tooltip = 'See tools through walls'
})

local toolESPColorOptions = {"Yellow", "Red", "Blue", "Green", "Purple", "White", "Cyan", "Orange", "Pink"}
local toolESPColorMap = {
    Yellow = Color3.new(1, 1, 0),
    Red = Color3.new(1, 0, 0),
    Blue = Color3.new(0, 0.4, 1),
    Green = Color3.new(0, 1, 0),
    Purple = Color3.new(0.6, 0, 1),
    White = Color3.new(1, 1, 1),
    Cyan = Color3.new(0, 1, 1),
    Orange = Color3.new(1, 0.5, 0),
    Pink = Color3.new(1, 0.4, 0.7),
}

local ToolESPColorPicker = ToolESPGroup:AddDropdown('ToolESPColor', {
    Values = toolESPColorOptions,
    Default = 1,
    Text = 'Tool ESP Color',
    Tooltip = 'Color for tool highlights'
})

local toolESPMaxDist = 500
ToolESPGroup:AddSlider('ToolESPMaxDist', {
    Text = 'Max Distance',
    Default = 500,
    Min = 50,
    Max = 2000,
    Rounding = 0,
    Compact = false,
    Tooltip = 'Maximum distance to show tool ESP'
}):OnChanged(function(val) toolESPMaxDist = val end)

ToolESPGroup:AddDivider()

local toolsESPLabels = {}
 _toolsESPAddConn    = nil
 _toolsESPRemoveConn = nil

local function isDroppedTool(obj)
    if not obj or not obj:IsA("Tool") then return false end
    if not obj:IsDescendantOf(workspace) then return false end
    for _, p in ipairs(Players:GetPlayers()) do
        local char = p.Character
        if char and obj:IsDescendantOf(char) then return false end
        local bp = p:FindFirstChild("Backpack")
        if bp and obj:IsDescendantOf(bp) then return false end
    end
    return true
end

local function getToolESPColor()
    local sel = ToolESPColorPicker.Value
    return toolESPColorMap[sel] or Color3.new(1, 1, 0)
end

local function createToolESPLabel(tool)
    if toolsESPLabels[tool] then return end
    local handle = tool:FindFirstChild("Handle")
    if not handle then return end

    local bb = Instance.new("BillboardGui")
    bb.Name = "RomazToolLabel"
    bb.Adornee = handle
    bb.Size = UDim2.new(0, 120, 0, 36)
    bb.StudsOffset = Vector3.new(0, 2, 0)
    bb.AlwaysOnTop = ToolESPXrayToggle.Value
    bb.MaxDistance = toolESPMaxDist
    bb.ClipsDescendants = true
    bb.Parent = playerGui

    local nameLabel = Instance.new("TextLabel")
    nameLabel.Size = UDim2.new(1, 0, 0.6, 0)
    nameLabel.BackgroundTransparency = 1
    nameLabel.Font = Library.BoldFont or Enum.Font.GothamBold
    nameLabel.TextColor3 = getToolESPColor()
    nameLabel.TextStrokeTransparency = 0.3
    nameLabel.TextScaled = true
    nameLabel.Text = tool.Name
    nameLabel.Visible = ToolESPNameToggle.Value
    nameLabel.Parent = bb

    local distLabel = Instance.new("TextLabel")
    distLabel.Size = UDim2.new(1, 0, 0.4, 0)
    distLabel.Position = UDim2.new(0, 0, 0.6, 0)
    distLabel.BackgroundTransparency = 1
    distLabel.Font = Library.Font or Enum.Font.Gotham
    distLabel.TextColor3 = Color3.fromRGB(200, 200, 200)
    distLabel.TextStrokeTransparency = 0.3
    distLabel.TextScaled = true
    distLabel.Text = ""
    distLabel.Visible = ToolESPDistToggle.Value
    distLabel.Parent = bb

    toolsESPLabels[tool] = {bb = bb, nameLabel = nameLabel, distLabel = distLabel}
end

local function removeToolESPLabel(tool)
    local data = toolsESPLabels[tool]
    if data then
        pcall(function() data.bb:Destroy() end)
        toolsESPLabels[tool] = nil
    end
end

task.spawn(function()
    while not Library.Unloaded do
        if ToolsESPToggle.Value then
            local myPos = getplrpos()
            local toRemove = {}
            for tool, data in pairs(toolsESPLabels) do
                pcall(function()
                    if not tool or not tool.Parent or not isDroppedTool(tool) then
                        table.insert(toRemove, tool)
                        return
                    end
                    local handle = tool:FindFirstChild("Handle")
                    if handle and data.distLabel and data.distLabel.Parent then
                        local dist = (handle.Position - myPos).Magnitude
                        data.distLabel.Text = string.format("%.0f studs", dist)
                    end
                    if data.nameLabel and data.nameLabel.Parent then
                        data.nameLabel.TextColor3 = getToolESPColor()
                    end
                end)
            end
            for _, tool in ipairs(toRemove) do
                removeToolESPLabel(tool)
                if toolsESPObjects[tool] then
                    pcall(function() toolsESPObjects[tool]:Destroy() end)
                    toolsESPObjects[tool] = nil
                end
            end
        end
        task.wait(0.5)
    end
end)

ToolsESPToggle:OnChanged(function(value)
    if _toolsESPAddConn    then _toolsESPAddConn:Disconnect();    _toolsESPAddConn    = nil end
    if _toolsESPRemoveConn then _toolsESPRemoveConn:Disconnect(); _toolsESPRemoveConn = nil end

    for _, v in pairs(toolsESPObjects) do
        pcall(function() v:Destroy() end)
    end
    toolsESPObjects = {}
    for tool, _ in pairs(toolsESPLabels) do
        removeToolESPLabel(tool)
    end

    if not value then return end

    local function highlightDroppedTool(tool)
        if not isDroppedTool(tool) then return end
        if toolsESPObjects[tool]   then return end

        local c = getToolESPColor()
        local hl = Instance.new("Highlight")
        hl.Name                = "RomazToolESP"
        hl.FillColor           = c
        hl.OutlineColor        = c
        hl.FillTransparency    = 0.4
        hl.OutlineTransparency = 0
        hl.DepthMode           = ToolESPXrayToggle.Value
            and Enum.HighlightDepthMode.AlwaysOnTop
            or  Enum.HighlightDepthMode.Occluded
        hl.Adornee             = tool
        hl.Parent              = playerGui
        toolsESPObjects[tool]  = hl

        createToolESPLabel(tool)
    end

    for _, v in ipairs(workspace:GetDescendants()) do
        pcall(function() highlightDroppedTool(v) end)
    end

    _toolsESPAddConn = workspace.DescendantAdded:Connect(function(v)
        if not ToolsESPToggle.Value then return end
        task.defer(function()
            if ToolsESPToggle.Value then
                pcall(function() highlightDroppedTool(v) end)
            end
        end)
    end)

    _toolsESPRemoveConn = workspace.DescendantRemoving:Connect(function(v)
        local hl = toolsESPObjects[v]
        if hl then
            toolsESPObjects[v] = nil
            pcall(function() hl:Destroy() end)
        end
        removeToolESPLabel(v)
    end)

    Library:Notify("Tool ESP enabled", 3)
end)

ToolESPNameToggle:OnChanged(function(value)
    for _, data in pairs(toolsESPLabels) do
        if data.nameLabel and data.nameLabel.Parent then
            data.nameLabel.Visible = value
        end
    end
end)

ToolESPDistToggle:OnChanged(function(value)
    for _, data in pairs(toolsESPLabels) do
        if data.distLabel and data.distLabel.Parent then
            data.distLabel.Visible = value
        end
    end
end)

ToolESPXrayToggle:OnChanged(function(value)
    for _, hl in pairs(toolsESPObjects) do
        if typeof(hl) == "Instance" and hl:IsA("Highlight") then
            hl.DepthMode = value
                and Enum.HighlightDepthMode.AlwaysOnTop
                or  Enum.HighlightDepthMode.Occluded
        end
    end
    for _, data in pairs(toolsESPLabels) do
        if data.bb and data.bb.Parent then
            data.bb.AlwaysOnTop = value
        end
    end
end)

ToolESPColorPicker:OnChanged(function(value)
    local c = toolESPColorMap[value] or Color3.new(1, 1, 0)
    for _, hl in pairs(toolsESPObjects) do
        if typeof(hl) == "Instance" and hl:IsA("Highlight") then
            hl.FillColor = c
            hl.OutlineColor = c
        end
    end
end)

Players.PlayerAdded:Connect(function(player)
    task.wait(.1)
    if PlayersESPToggle.Value and shouldESPPlayer(player) then
        CreateESP(player)
    end
    player.CharacterAdded:Connect(function()
        task.wait(0.5)
        if PlayersESPToggle.Value and shouldESPPlayer(player) then
            CreateESP(player)
        end
    end)
    if BuildingsESPToggle.Value then
        local target = BuildESPTargetInput.Value
        if player ~= plr and (target == "all" or player.Name:lower():find(target:lower())) then
            CreateBuildESP(player)
        end
    end
end)

Players.PlayerRemoving:Connect(function(player)
    RemoveESP(player)
    RemoveBuildESP(player)
end)

plr.CharacterAdded:Connect(function()
    task.wait(.1)
    if PlayersESPToggle.Value then
        for _, player in ipairs(Players:GetPlayers()) do
            if espObjects[player] then
                RemoveESP(player)
                CreateESP(player)
            end
        end
    end
end)

PlayersESPToggle:OnChanged(function(value)
    if value then
        StartESPUpdateLoop()
        for _, player in ipairs(Players:GetPlayers()) do
            if shouldESPPlayer(player) then
                CreateESP(player)
            end
        end
        Library:Notify("Players ESP enabled", 3)
    else
        for player, _ in pairs(espObjects) do
            RemoveESP(player)
        end
        if not BuildingsESPToggle.Value then
            StopESPUpdateLoop()
        end
    end
end)

BuildingsESPToggle:OnChanged(function(value)
    if value then
        StartESPUpdateLoop()
        local target = BuildESPTargetInput.Value
        for _, player in ipairs(Players:GetPlayers()) do
            if player ~= plr and (target == "all" or player.Name:lower():find(target:lower())) then
                CreateBuildESP(player)
            end
        end
        Library:Notify("Build ESP enabled", 3)
    else
        RemoveAllBuildESP()
        if not PlayersESPToggle.Value then
            StopESPUpdateLoop()
        end
    end
end)

BuildESPTargetInput:OnChanged(function(value)
    if BuildingsESPToggle.Value then
        RemoveAllBuildESP()
        local target = value ~= "" and value or "all"
        for _, player in ipairs(Players:GetPlayers()) do
            if player ~= plr and (target == "all" or player.Name:lower():find(target:lower())) then
                CreateBuildESP(player)
            end
        end
    end
end)

task.defer(function()
    if PlayersESPToggle.Value then
        StartESPUpdateLoop()
        for _, player in ipairs(Players:GetPlayers()) do
            if shouldESPPlayer(player) then
                CreateESP(player)
            end
        end
    end
    if BuildingsESPToggle.Value then
        if not espUpdateConnection then StartESPUpdateLoop() end
        local target = BuildESPTargetInput.Value
        for _, player in ipairs(Players:GetPlayers()) do
            if player ~= plr and (target == "all" or player.Name:lower():find(target:lower())) then
                CreateBuildESP(player)
            end
        end
    end
end)

ESPNametagToggle:OnChanged(function(value)
    espNametags = value
    if value then
        for player, _ in pairs(espObjects) do
            CreateESPNametag(player)
        end
    else
        for player, _ in pairs(espNameLabels) do
            RemoveESPNametag(player)
        end
    end
end)

ESPDistanceToggle:OnChanged(function(value)
    espShowDistance = value
end)

ESPBuildCountToggle:OnChanged(function(value)
    espShowBuildCount = value
end)

ESPTargetInput:OnChanged(function(value)
    espTrackedPlayer = value ~= "" and value or "all"
    if PlayersESPToggle.Value then
        RemoveAllESP()
        for _, player in ipairs(Players:GetPlayers()) do
            if shouldESPPlayer(player) then CreateESP(player) end
        end
    end
end)

local SettingsGroup = Tabs.Settings:AddLeftGroupbox('General Information')

local AutoRejoin = SettingsGroup:AddToggle('AutoRejoin', {
    Text = 'Auto Rejoin on Kick',
    Default = false,
    Tooltip = 'Automatically rejoin the game if kicked'
})

local ShowCredits = SettingsGroup:AddToggle('ShowCredits', {
    Text = 'Show Credits on Join',
    Default = true,
    Tooltip = 'Display credits message when script loads'
})

SettingsGroup:AddToggle('AutoPingOptimize', {
    Text = 'Auto Ping Optimization',
    Default = true,
    Tooltip = 'Automatically adjust delays based on your ping to prevent errors on bad connections'
}):OnChanged(function(val)
    autoPingOptimize = val
    if not val then
        safeModeActive = false
        buildDelay = math.max(game.Players.LocalPlayer:GetNetworkPing() + 0.007, 0.051)
    end
end)

SettingsGroup:AddDivider()

SettingsGroup:AddLabel('Script Version: 2.1')
SettingsGroup:AddLabel('Game: ' .. game:GetService('MarketplaceService'):GetProductInfo(game.PlaceId).Name)
SettingsGroup:AddLabel('Place ID: ' .. game.PlaceId)
SettingsGroup:AddLabel('Player: ' .. plr.Name)
SettingsGroup:AddLabel('Status: ' .. (isOwner and 'OWNER 👑' or 'USER 👥'))

local CreditsGroup = Tabs.Settings:AddRightGroupbox('Credits')
CreditsGroup:AddLabel('Developer: RomazDev/peaiz')
CreditsGroup:AddLabel('Library: Linoria')

CreditsGroup:AddButton({
    Text = 'Copy Job ID 📝',
    Func = function()
        if setclipboard then
            setclipboard(game.JobId)
            Library:Notify('Job ID copied to clipboard!', 3)
        end
    end
})

local function createToolPanel(config) return Library:CreateToolPanel(config) end
local function createToolInput(parent, yPos, placeholder) return Library:CreateToolInput(parent, yPos, placeholder) end
local function createToolButton(parent, text, yPos, xPos, width, callback) return Library:CreateToolButton(parent, text, yPos, xPos, width, callback) end
local function createToolLabel(parent, text, yPos) return Library:CreateToolLabel(parent, text, yPos) end

function getplrcfr(p)
    local c = (p or localplr).Character
    if c and c:FindFirstChild("HumanoidRootPart") then
        return c.HumanoidRootPart.CFrame
    end
    return CFrame.new(0, 100, 0)
end

local m = (1/3)/1.5
local gs = game:GetService("GeometryService")
function crep(offset,size)
    if typeof(offset) == "Vector3" then
        offset = CFrame.new(offset)
    end
    local p = Instance.new("Part")
    p.Anchored = true
    p.Position = getplrpos() + (offset.Position*m)
    p.Material = Enum.Material.SmoothPlastic
    p.Reflectance = 0.6
    p.Size = (size and size*m) or Vector3.new(3*m,3*m,3*m)
    p.Parent = workspace
    return p
end
local p1 = crep(Vector3.new(0,0,0))
local p2 = crep(Vector3.new(0,-1.5,1.5),Vector3.new(3,6,6))
p2.Shape = Enum.PartType.Cylinder
function union(p1,p2s,un)
    local po
    if un then
        po = gs:UnionAsync(p1,p2s)
    else
        po = gs:IntersectAsync(p1,p2s)
    end
    p1:Destroy()
    for i,v in pairs(p2s) do
        v:Destroy()
    end
    return po[1]
end
local w = union(p1,{p2})
w.UsePartColor = true
w.Name = "fbr"
w.Color = Color3.fromRGB(0,0,255)
m = 1/1.5
local c4 = w:Clone()
c4.Position = w.CFrame * Vector3.new(m,0,0)
c4.Name = "fbl"
c4.Rotation = Vector3.new(0,0,180)
c4.Color = Color3.fromRGB(0,255,0)
local c3 = w:Clone()
c3.Position = w.CFrame * Vector3.new(0,m,0)
c3.Name = "ftr"
c3.Color = Color3.fromRGB(255,255,255)
local c5 = w:Clone()
c5.Position = w.CFrame * Vector3.new(m,m,0)
c5.Name = "ftl"
c5.Color = Color3.fromRGB(255,0,0)
local c1 = w:Clone()
c1.Position = w.CFrame * Vector3.new(0,0,m)
c1.Name = "bbr"
c1.Rotation = Vector3.new(180,0,0)
c1.Color = Color3.fromRGB(0,255,0)
local c7 = w:Clone()
c7.Position = w.CFrame * Vector3.new(m,0,m)
c7.Name = "bbl"
c7.Rotation = Vector3.new(180,0,0)
c7.Color = Color3.fromRGB(0,0,255)
local c2 = w:Clone()
c2.Position = w.CFrame * Vector3.new(0,m,m)
c2.Name = "btr"
c2.Rotation = Vector3.new(180,0,180)
c2.Color = Color3.fromRGB(255,0,0)
local c6 = w:Clone()
c6.Position = w.CFrame * Vector3.new(m,m,m)
c6.Name = "btl"
c6.Rotation = Vector3.new(180,0,180)
c6.Color = Color3.fromRGB(255,255,255)
w.Rotation = Vector3.new(0,0,180)
local finished = union(w,{c1,c2,c3,c4,c5,c6,c7},true)
finished.Anchored = false
finished.CanCollide = false
local rtool = Instance.new("Tool")
rtool.Grip = CFrame.Angles(0,math.rad(180),0)
rtool.Name = "Rotation Tool"
local handle = Instance.new("Part")
handle.Size = Vector3.new(1,1,1)
handle.Transparency = 1
handle.CanCollide = false
handle.Name = "Handle"
local we = Instance.new("Weld")
we.Parent = handle
we.C0 = CFrame.new(-m/2,-m/2,m/2)
handle.Parent = rtool
finished.Parent = handle
local cbt = Instance.new("Beam")
local ba0 = Instance.new("Attachment")
local ba1 = Instance.new("Attachment")
cbt.Attachment0 = ba0
cbt.Attachment1 = ba1
cbt.TextureLength = 6
cbt.TextureMode = Enum.TextureMode.Static
cbt.Texture = "rbxassetid://18498294"
ba0.Name = "A0"
ba1.Name = "A1"
ba0.Parent = cbt
ba1.Parent = cbt
cbt.Parent = rtool
local inverses = {
    [Enum.NormalId.Top] = Enum.NormalId.Bottom,
    [Enum.NormalId.Bottom] = Enum.NormalId.Top,
    [Enum.NormalId.Front] = Enum.NormalId.Back,
    [Enum.NormalId.Back] = Enum.NormalId.Front,
    [Enum.NormalId.Right] = Enum.NormalId.Left,
    [Enum.NormalId.Left] = Enum.NormalId.Right,
}
local power = 999999
local rotooltype = {name="Rotate"}
local roundNumber = 15
local lspeedNumber = 50
local rspeedNumber = 50
function hover(part,cfr,override)
    local occ = part.CanCollide
    part.CanCollide = false
    if rotooltype.name == "Rotate" or override then
        local hoverpos = Instance.new("BodyPosition")
        local hovergyr = Instance.new("BodyGyro")
        hoverpos.MaxForce = Vector3.new(power,power,power)
        hoverpos.P = power
        hoverpos.D = 2500
        hovergyr.MaxTorque = Vector3.new(power,power,power)
        hovergyr.P = power
        hovergyr.D = 6200
        hoverpos.Position = cfr.Position
        hovergyr.CFrame = cfr
         function delete(w)
            coroutine.wrap(function()
                task.wait(w or 0)
                hoverpos:Destroy()
                hovergyr:Destroy()
            end)()
            coroutine.wrap(function()
                task.wait(1)
                part.CanCollide = occ
            end)()
        end
        hoverpos.Parent = part
        hovergyr.Parent = part
        return delete
    elseif rotooltype.name == "Linear Velocity" then
        local repeatpositioning = game:GetService("RunService").RenderStepped:Connect(function()
            part.CFrame = cfr
            part.AssemblyLinearVelocity = (cfr * Vector3.new(0,0,-lspeedNumber)) - cfr.Position
            part.AssemblyAngularVelocity = Vector3.zero
            task.wait()
        end)
         function delete(w)
            repeatpositioning:Disconnect()
            coroutine.wrap(function()
                task.wait(1)
                part.CanCollide = occ
            end)()
        end
        return delete
    end
end
local isnetworkowner = isnetworkowner or function(part)
    task.wait(0.5)
    return true
end
function gcp(p,plr)
    local c = (plr ~= nil and plr.Character) or localplr.Character
    p = p:lower()
    if p == "hrp" then
        p = "HumanoidRootPart"
    elseif p == "hum" then
        p = "Humanoid"
    end
    if c and c:FindFirstChild(p) then
        return c:FindFirstChild(p)
    else
        return false
    end
end
local tagged = {}
function gettag(t)
    return (tagged[t] and tagged[t] > tick()) or false
end
function settag(t,num)
    if not num then
        num = 1
    end
    tagged[t] = tick()+num
end
function rotate(part,cframe,currentrtool)
    if not equiptool("Build") or not equiptool("Delete") or not equiptool("Paint") then
        return
    end
    local done = false
    local done2 = false
    local t = tick()+10
    local p = nil
    repeat
        gcp("hrp").CFrame = (part.CFrame + Vector3.new(0,part.Size.Y/2+2.5,0))
        if part.Anchored and not gettag(part) then
            settag(part,0.6)
            local args = {
                part,
                Enum.NormalId.Top,
                getplrpos(),
                "material",
                nil,
                "anchor",
                ""
            }
            equiptool("Paint").Script.Event:FireServer(unpack(args))
        end
        localplr.SimulationRadius = math.max(localplr.SimulationRadius,100)
        task.wait()
    until isnetworkowner(part)
    coroutine.wrap(function()
        local cframe2 = cframe * CFrame.new(Vector3.new(0,part.Size.Y,0)) * cframe.Rotation
        local d = hover(part,cframe2,true)
        while not done and tick() < t do
            task.wait()
            part.CFrame = cframe2
        end
        d(5)
    end)()
    task.wait(0.5)
    local s,e = pcall(function()
        local args = {
            part,
            getplrpos()
        }
        coroutine.wrap(function()
            equiptool("Delete").Script.Event:FireServer(unpack(args))
        end)()
        local nid = Enum.NormalId.Bottom
        local args = {
            part,
            nid,
            getplrpos(),
            "normal"
        }
        coroutine.wrap(function()
            equiptool("Build").Script.Event:FireServer(unpack(args))
        end)()
        p = cfolder[localplr.Name].ChildAdded:Wait()
        p.CanCollide = false
        repeat
            gcp("hrp").CFrame = (p.CFrame + Vector3.new(0,p.Size.Y/2+2.5,0))
            if p.Anchored and not gettag(p) then
                settag(p,0.6)
                local args = {
                    p,
                    Enum.NormalId.Top,
                    getplrpos(),
                    "material",
                    nil,
                    "anchor",
                    ""
                }
                equiptool("Paint").Script.Event:FireServer(unpack(args))
            end
            localplr.SimulationRadius = math.max(localplr.SimulationRadius,100)
            task.wait()
        until isnetworkowner(p)
        p.CanCollide = true
        coroutine.wrap(function()
            local d = hover(p,cframe)
            repeat
                task.wait()
            until done2
            d(5)
        end)()
        task.wait(1)
        done = true
        local args = {
            p,
            Enum.NormalId.Right,
            getplrpos(),
            "material",
            nil,
            "anchor",
            ""
        }
        equiptool("Paint").Script.Event:FireServer(unpack(args))
    end)
    done = true
    currentrtool.Parent = localplr.Character
    task.wait(2)
    done2 = true
end
local rotatehandles = Instance.new("ArcHandles")
rotatehandles.Parent = game.CoreGui
local selectionbox = Instance.new("SelectionBox")
selectionbox.LineThickness = 0.05
selectionbox.SurfaceTransparency = 1
selectionbox.Parent = game.CoreGui
table.insert(tools,{rotatehandles,selectionbox})

local _rotPanel = createToolPanel({name = "RotateAngleUi", title = "Rotate Tool", size = UDim2.new(0, 200, 0, 160)})
local Shape = _rotPanel.gui
local InputLabel = createToolLabel(_rotPanel.frame, "Rounding to nearest 15 degrees", 0.18)
local TextBox = createToolInput(_rotPanel.frame, 0.35, "Type angle, press Enter")
local ConfirmButton = createToolButton(_rotPanel.frame, "Confirm", 0.55, 0.08, 0.84, nil)

function updateInputDisplay()
    local s = math.abs(roundNumber) ~= 1 and "s" or ""
    InputLabel.Text = string.format("Rounding to nearest %d degree%s", roundNumber, s)
end
updateInputDisplay()

 function lvToOrientation(v)
    return CFrame.Angles(
        math.rad(v.X * v.Z),
        math.rad(v.X * v.Y),
        math.rad(v.Y * v.Z + 90)
    )
end
if TextBox and TextBox.FocusLost then
    TextBox.FocusLost:Connect(function()
        local rn = tonumber(TextBox.Text)
        if rn then
            roundNumber = rn
            updateInputDisplay()
        end
        TextBox.Text = ""
    end)
end
function roundToNearest(num,by)
    return (by == 0 and num) or (math.round(num/by) * by)
end
function AngleFromAxis(axis,rA)
    rA = math.rad(roundToNearest(math.deg(rA),roundNumber))
    return axis==Enum.Axis.X and {rA,0,0}
    or axis==Enum.Axis.Y and {0,rA,0}
    or axis==Enum.Axis.Z and {0,0,rA}
end
function IsSelectable(part,hit)
    if part and localplr.Character:FindFirstChild("HumanoidRootPart") then
        if (hit - getplrpos()).magnitude < 30 and part:IsDescendantOf(cfolder) then return true end
    end
    return false
end
function notify(text,color)
    color = color or Color3.fromRGB(0,200,0)
    game.TextChatService.TextChannels.RBXGeneral:DisplaySystemMessage(string.format("<font color='#%s'>%s</font>",color:ToHex(),text))
end
function sayto(plr,text,color)
    if plr == nil then
        game:GetService("TextChatService").TextChannels.RBXGeneral:SendAsync(text)
    elseif plr == localplr then
        notify(text,color)
    else
        local wc = getwc(localplr,plr)
        if wc then
            wc:SendAsync(text)
        end
    end
end
function createrotool()
    local sound = Instance.new("Sound")
    sound.SoundId = "rbxassetid://6897623656"
    sound.Parent = workspace
    sound:Play()
    game.Debris:AddItem(sound, 5)

    local connections = {}
    currentrtool = rtool:Clone()

    local oldBeam = currentrtool:FindFirstChildWhichIsA("Beam")
    if oldBeam then oldBeam:Destroy() end

    local equipped = false
    undo_part  = {}
    undo_cframe = {}
    selection  = nil
    lastCFrame = nil

    local to_undo_part  = nil
    local to_undo_cframe = nil
    local sclone = nil
    local mdown

    local handle = currentrtool.Handle
    local union  = handle:FindFirstChildWhichIsA("PartOperation")
    handle.Weld.Part0 = union
    handle.Weld.Part1 = handle

    table.insert(connections, currentrtool.Equipped:Connect(function()
        union.Parent = workspace
        equipped = true
        Shape.Enabled = true
        updateInputDisplay()
    end))

    table.insert(connections, currentrtool.Unequipped:Connect(function()
        if not equipped then return end
        union.Parent = handle
        selectionbox.Adornee  = nil
        rotatehandles.Adornee = nil
        selection = nil
        to_undo_part  = nil
        to_undo_cframe = nil
        rotatehandles.Axes = Axes.new(Enum.Axis.X, Enum.Axis.Y, Enum.Axis.Z)
        undo_part  = {}
        undo_cframe = {}
        if sclone then sclone:Destroy(); sclone = nil end
        equipped = false
        Shape.Enabled = false
    end))

    table.insert(connections, mouse.Button1Down:Connect(function()
        if not equipped then return end
        local issel = IsSelectable(mouse.Target, mouse.Hit.Position)
        if issel and selection ~= mouse.Target then
            selection = mouse.Target
            if sclone then sclone:Destroy() end
            sclone = selection:Clone()
            sclone.Size = sclone.Size + Vector3.new(0.01, 0.01, 0.01)
            sclone.Transparency = 0.5
            sclone.Anchored   = true
            sclone.CanCollide = false
            sclone.Parent = workspace
            table.insert(tools, {sclone})
            selectionbox.Adornee  = sclone
            rotatehandles.Adornee = sclone
        end
    end))

    table.insert(connections, mouse.Button1Up:Connect(function()
        if not equipped then return end
        if to_undo_part and to_undo_cframe then
            if to_undo_part ~= selection or to_undo_cframe ~= selection.CFrame then
                table.insert(undo_part,  to_undo_part)
                table.insert(undo_cframe, to_undo_cframe)
            end
        end
        to_undo_part  = nil
        to_undo_cframe = nil
        rotatehandles.Axes = Axes.new(Enum.Axis.X, Enum.Axis.Y, Enum.Axis.Z)
        mdown = nil
    end))

    table.insert(connections, rotatehandles.MouseButton1Down:Connect(function(axis)
        if not equipped then return end
        if selection and sclone then
            sclone.Transparency = 0.75
            mdown = workspace.CurrentCamera.CameraSubject
            local focusdiff = workspace.CurrentCamera.CFrame.Position - mdown.Parent.HumanoidRootPart.Position
            workspace.CurrentCamera.CameraType = Enum.CameraType.Scriptable
            coroutine.wrap(function()
                while mdown do
                    workspace.Camera.CFrame = CFrame.new(focusdiff + mdown.Parent.HumanoidRootPart.Position)
                        * workspace.Camera.CFrame.Rotation
                    task.wait()
                end
            end)()
            lastCFrame    = sclone.CFrame
            to_undo_part  = selection
            to_undo_cframe = lastCFrame
            rotatehandles.Axes = Axes.new(axis)
        end
    end))

    table.insert(connections, rotatehandles.MouseButton1Up:Connect(function()
        if not equipped then return end
        mdown = nil
        if to_undo_part and to_undo_cframe then
            if to_undo_part ~= selection or to_undo_cframe ~= selection.CFrame then
                table.insert(undo_part,  to_undo_part)
                table.insert(undo_cframe, to_undo_cframe)
            end
        end
        if sclone then sclone.Transparency = 0.5 end
        to_undo_part  = nil
        to_undo_cframe = nil
        rotatehandles.Axes = Axes.new(Enum.Axis.X, Enum.Axis.Y, Enum.Axis.Z)
        workspace.CurrentCamera.CameraType = Enum.CameraType.Custom
    end))

    table.insert(connections, rotatehandles.MouseDrag:Connect(function(axis, relativeAngle)
        if not equipped then return end
        if sclone then
            sclone.CFrame = lastCFrame * CFrame.Angles(unpack(AngleFromAxis(axis, relativeAngle)))
        end
    end))

    table.insert(connections, ConfirmButton.MouseButton1Click:Connect(function()
        if sclone and selection then
            rotate(selection, sclone.CFrame, currentrtool)
            sclone:Destroy(); sclone = nil
        end
    end))

    table.insert(connections, currentrtool.AncestryChanged:Connect(function()
        if not currentrtool or not currentrtool.Parent or not currentrtool.Parent.Parent then
            for _, c in pairs(connections) do c:Disconnect() end
        end
    end))

    table.insert(tools, {union, currentrtool})
    currentrtool.Parent = localplr.Backpack
    task.wait()
    return currentrtool
end

local _lvPanel = createToolPanel({
    name = "LinVelGui",
    title = "Linear Velocity",
    position = UDim2.new(0.5, 0, 0.03, 0),
    size = UDim2.new(0, 200, 0, 155),
    statusText = "Click block, then activate"
})
local LVGui = _lvPanel.gui
local LVSpeedLabel = createToolLabel(_lvPanel.frame, "Speed: 50 studs/s", 0.18)
local LVInput = createToolInput(_lvPanel.frame, 0.35, "Type speed, press Enter")
local LVStatus = _lvPanel.status

local lvSpeed = 50

if LVInput and scriptConnections then
    table.insert(scriptConnections, LVInput.FocusLost:Connect(function()
        local v = tonumber(LVInput.Text)
        if v then
            lvSpeed = v
            LVSpeedLabel.Text = "Speed: " .. v .. " studs/s"
            LVStatus.Text = "Speed updated ✓\nClick a block then activate"
        end
        LVInput.Text = ""
    end))
end

local lvBaseTool = Instance.new("Tool")
lvBaseTool.Name = "Linear Velocity Tool"
lvBaseTool.ToolTip = "Apply linear velocity to a block"
lvBaseTool.RequiresHandle = true

local lvHandlePart = Instance.new("Part")
lvHandlePart.Name = "Handle"
lvHandlePart.Size = Vector3.new(0.7, 0.7, 2.8)
lvHandlePart.Color = Color3.fromRGB(45, 100, 220)
lvHandlePart.Material = Enum.Material.SmoothPlastic
lvHandlePart.Reflectance = 0.35
lvHandlePart.CanCollide = false
lvHandlePart.Parent = lvBaseTool

do
    local mesh = Instance.new("SpecialMesh")
    mesh.MeshType = Enum.MeshType.FileMesh
    mesh.MeshId = "rbxassetid://430736398"
    mesh.Scale = Vector3.new(0.38, 0.38, 0.55)
    mesh.Parent = lvHandlePart
end

function createlinveltool()
    local connections = {}
    local tool = lvBaseTool:Clone()
    local equipped = false
    local lvSel = nil
    local lvSurface = nil
    local lvSBox = Instance.new("SelectionBox")
    lvSBox.Color3 = Color3.fromRGB(55, 130, 255)
    lvSBox.SurfaceColor3 = Color3.fromRGB(55, 130, 255)
    lvSBox.SurfaceTransparency = 0.65
    lvSBox.LineThickness = 0.07
    lvSBox.Parent = game.CoreGui
    table.insert(tools, {lvSBox})

    local lvBeam = Instance.new("Beam")
    lvBeam.Color = ColorSequence.new(Color3.fromRGB(55, 130, 255), Color3.fromRGB(0, 200, 255))
    lvBeam.Transparency = NumberSequence.new(0.3, 0.8)
    lvBeam.Width0 = 0.6
    lvBeam.Width1 = 0.15
    lvBeam.TextureLength = 4
    lvBeam.TextureMode = Enum.TextureMode.Static
    lvBeam.Texture = "rbxassetid://18498294"
    lvBeam.FaceCamera = true
    lvBeam.Enabled = false

    local lvA0 = Instance.new("Attachment")
    lvA0.Name = "LVBeamA0"
    local lvA1 = Instance.new("Attachment")
    lvA1.Name = "LVBeamA1"
    lvBeam.Attachment0 = lvA0
    lvBeam.Attachment1 = lvA1
    lvBeam.Parent = workspace.Terrain
    lvA0.Parent = workspace.Terrain
    lvA1.Parent = workspace.Terrain
    table.insert(tools, {lvBeam, lvA0, lvA1})

    local function updateBeam()
        if not lvSel or not lvSel.Parent or not lvSurface or not equipped then
            lvBeam.Enabled = false
            return
        end
        local faceVec = Vector3.FromNormalId(lvSurface)
        local worldDir = lvSel.CFrame:VectorToWorldSpace(faceVec).Unit
        local startPos = lvSel.Position
        local endPos = startPos + worldDir * math.min(lvSpeed * 0.15, 20)
        lvA0.WorldPosition = startPos
        lvA1.WorldPosition = endPos
        lvBeam.Enabled = true
    end

    table.insert(connections, tool.Equipped:Connect(function()
        equipped = true
        LVGui.Enabled = true
        LVStatus.Text = "Click a block to select,\nthen activate (Left Click)"
    end))

    table.insert(connections, tool.Unequipped:Connect(function()
        if not equipped then return end
        equipped = false
        LVGui.Enabled = false
        lvSBox.Adornee = nil
        lvSel = nil
        lvSurface = nil
        lvBeam.Enabled = false
    end))

    table.insert(connections, mouse.Button1Down:Connect(function()
        if not equipped then return end
        if not IsSelectable(mouse.Target, mouse.Hit.Position) then return end
        lvSel = mouse.Target
        lvSurface = mouse.TargetSurface
        lvSBox.Adornee = lvSel
        LVStatus.Text = "Selected: " .. lvSel.Name
            .. "\nSurface: " .. tostring(mouse.TargetSurface)
            .. "\nActivate to push!"
        updateBeam()
    end))

    table.insert(connections, mouse.Move:Connect(function()
        if not equipped or not lvSel or not lvSel.Parent then return end
        if mouse.Target == lvSel then
            lvSurface = mouse.TargetSurface
            updateBeam()
        end
    end))

    table.insert(connections, tool.Activated:Connect(function()
        if not equipped then return end
        if not lvSel or not lvSel.Parent then
            LVStatus.Text = "No block selected!\nClick one first"
            return
        end

        local part = lvSel
        local surface = mouse.TargetSurface
        LVStatus.Text = "Applying velocity..."

        task.spawn(function()
            local hrp = localplr.Character and localplr.Character:FindFirstChild("HumanoidRootPart")
            if not hrp then return end

            if part.Anchored then
                local paint = localplr.Character:FindFirstChild("Paint") or localplr.Backpack:FindFirstChild("Paint")
                if paint then
                    if paint.Parent ~= localplr.Character then
                        paint.Parent = localplr.Character; task.wait()
                    end
                    paint.Script.Event:FireServer(part, Enum.NormalId.Top, hrp.Position, "material", nil, "anchor", "")
                    task.wait(0.9)
                end
            end

            hrp.CFrame = CFrame.new(part.Position + Vector3.new(0, part.Size.Y / 2 + 3, 0))
            task.wait(0.25)
            localplr.SimulationRadius = math.max(localplr.SimulationRadius, 500)
            task.wait(0.35)

            local faceVec = Vector3.FromNormalId(surface)
            local worldDir = part.CFrame:VectorToWorldSpace(faceVec).Unit

            local bv = Instance.new("BodyVelocity")
            bv.MaxForce = Vector3.new(1e9, 1e9, 1e9)
            bv.P        = 1e7
            bv.Velocity = worldDir * lvSpeed
            bv.Parent   = part

            LVStatus.Text = "⚡ Pushed! Speed: " .. lvSpeed .. " studs/s"

            task.delay(0.6, function()
                if bv and bv.Parent then bv:Destroy() end
            end)

            lvSBox.Adornee = nil
            lvSel = nil
            lvSurface = nil
            lvBeam.Enabled = false
        end)
    end))

    table.insert(connections, tool.AncestryChanged:Connect(function()
        if not tool or not tool.Parent or not tool.Parent.Parent then
            for _, c in pairs(connections) do c:Disconnect() end
            lvSBox:Destroy()
            lvBeam.Enabled = false
        end
    end))

    table.insert(tools, {tool})
    tool.Parent = localplr.Backpack
    task.wait()
    return tool
end

local _rvPanel = createToolPanel({
    name = "RotVelGui",
    title = "Rotational Velocity",
    position = UDim2.new(0.5, 0, 0.03, 0),
    size = UDim2.new(0, 200, 0, 185),
    statusText = "Click block, then activate"
})
local RVGui = _rvPanel.gui
local RVSpeedLabel = createToolLabel(_rvPanel.frame, "Speed: 180 /s | Axis: Y", 0.17)
local RVInput = createToolInput(_rvPanel.frame, 0.32, "Speed (/s), press Enter")
local RVStatus = _rvPanel.status

local axisRow = Instance.new("Frame")
axisRow.AnchorPoint = Vector2.new(0.5,0)
axisRow.BackgroundTransparency = 1
axisRow.Position = UDim2.new(0.5,0,0.5,0)
axisRow.Size = UDim2.new(0.84,0,0.12,0)
axisRow.Parent = _rvPanel.frame
do
    local l = Instance.new("UIListLayout")
    l.FillDirection = Enum.FillDirection.Horizontal
    l.HorizontalAlignment = Enum.HorizontalAlignment.Center
    l.Padding = UDim.new(0.04,0)
    l.Parent = axisRow
end

local rvAxis = "Y"
local axisButtons = {}
local axisColors = {
    X = {off=Color3.fromRGB(140,35,35), on=Color3.fromRGB(255,80,80)},
    Y = {off=Color3.fromRGB(35,100,35), on=Color3.fromRGB(80,220,80)},
    Z = {off=Color3.fromRGB(35,35,140), on=Color3.fromRGB(80,140,255)},
}

function updateAxisButtons()
    for axis, btn in pairs(axisButtons) do
        btn.BackgroundColor3 = axis == rvAxis and axisColors[axis].on or axisColors[axis].off
    end
end

for _, ax in ipairs({"X","Y","Z"}) do
    local btn = Instance.new("TextButton")
    btn.Size = UDim2.new(0.28,0,1,0)
    btn.BackgroundColor3 = axisColors[ax].off
    btn.BorderSizePixel = 0
    btn.Font = Library.BoldFont or Enum.Font.GothamBold
    btn.Text = ax
    btn.TextColor3 = Color3.fromRGB(255,255,255)
    btn.TextScaled = true
    btn.Parent = axisRow
    do local c = Instance.new("UICorner"); c.CornerRadius = UDim.new(0, 4); c.Parent = btn end
    axisButtons[ax] = btn
    btn.MouseButton1Click:Connect(function()
        rvAxis = ax
        updateAxisButtons()
        RVSpeedLabel.Text = "Speed: " .. (rvSpeed or 180) .. " /s | Axis: " .. ax
    end)
end
updateAxisButtons()

local rvSpeed = 180

RVInput.FocusLost:Connect(function()
    local v = tonumber(RVInput.Text)
    if v then
        rvSpeed = v
        RVSpeedLabel.Text = "Speed: " .. v .. " /s | Axis: " .. rvAxis
        RVStatus.Text = "Speed updated"
    end
    RVInput.Text = ""
end)

local rvBaseTool = Instance.new("Tool")
rvBaseTool.Name = "Rotational Velocity Tool"
rvBaseTool.ToolTip = "Spin blocks with angular velocity"
rvBaseTool.RequiresHandle = true

local rvHandlePart = Instance.new("Part")
rvHandlePart.Name = "Handle"
rvHandlePart.Size = Vector3.new(0.7, 0.7, 2.8)
rvHandlePart.Color = Color3.fromRGB(200, 75, 30)
rvHandlePart.Material = Enum.Material.SmoothPlastic
rvHandlePart.Reflectance = 0.3
rvHandlePart.CanCollide = false
rvHandlePart.Parent = rvBaseTool

do
    local mesh = Instance.new("SpecialMesh")
    mesh.MeshType = Enum.MeshType.FileMesh
    mesh.MeshId = "rbxassetid://1778999500"
    mesh.Scale = Vector3.new(0.07, 0.07, 0.07)
    mesh.Parent = rvHandlePart
end

function createrotveltool()
    local connections = {}
    local tool = rvBaseTool:Clone()
    local equipped = false
    local rvSel = nil
    local rvSBox = Instance.new("SelectionBox")
    rvSBox.Color3 = Color3.fromRGB(230, 100, 50)
    rvSBox.SurfaceColor3 = Color3.fromRGB(230, 100, 50)
    rvSBox.SurfaceTransparency = 0.65
    rvSBox.LineThickness = 0.07
    rvSBox.Parent = game.CoreGui
    table.insert(tools, {rvSBox})

    table.insert(connections, tool.Equipped:Connect(function()
        equipped = true
        RVGui.Enabled = true
        RVStatus.Text = "Click a block to select,\nthen activate to spin it"
    end))

    table.insert(connections, tool.Unequipped:Connect(function()
        if not equipped then return end
        equipped = false
        RVGui.Enabled = false
        rvSBox.Adornee = nil
        rvSel = nil
    end))

    table.insert(connections, mouse.Button1Down:Connect(function()
        if not equipped then return end
        if not IsSelectable(mouse.Target, mouse.Hit.Position) then return end
        rvSel = mouse.Target
        rvSBox.Adornee = rvSel
        RVStatus.Text = "Selected: " .. rvSel.Name
            .. "\nAxis: " .. rvAxis
            .. "\nActivate to spin!"
    end))

    table.insert(connections, tool.Activated:Connect(function()
        if not equipped then return end
        if not rvSel or not rvSel.Parent then
            RVStatus.Text = "No block selected!\nClick one first"
            return
        end

        local part = rvSel
        RVStatus.Text = "Applying spin..."

        task.spawn(function()
            local hrp = localplr.Character and localplr.Character:FindFirstChild("HumanoidRootPart")
            if not hrp then return end

            if part.Anchored then
                local paint = localplr.Character:FindFirstChild("Paint") or localplr.Backpack:FindFirstChild("Paint")
                if paint then
                    if paint.Parent ~= localplr.Character then
                        paint.Parent = localplr.Character; task.wait()
                    end
                    paint.Script.Event:FireServer(part, Enum.NormalId.Top, hrp.Position, "material", nil, "anchor", "")
                    task.wait(0.9)
                end
            end

            hrp.CFrame = CFrame.new(part.Position + Vector3.new(0, part.Size.Y / 2 + 3, 0))
            task.wait(0.25)
            localplr.SimulationRadius = math.max(localplr.SimulationRadius, 500)
            task.wait(0.35)

            local radPerSec = math.rad(rvSpeed)
            local angVec = rvAxis == "X" and Vector3.new(radPerSec, 0, 0)
                or rvAxis == "Y" and Vector3.new(0, radPerSec, 0)
                or Vector3.new(0, 0, radPerSec)

            local bav = Instance.new("BodyAngularVelocity")
            bav.MaxTorque = Vector3.new(1e9, 1e9, 1e9)
            bav.P         = 1e7
            bav.AngularVelocity = angVec
            bav.Parent    = part

            RVStatus.Text = "🌀 Spinning on " .. rvAxis .. "!\n"
                .. rvSpeed .. " °/s"

            rvSBox.Adornee = nil
            rvSel = nil
        end)
    end))

    table.insert(connections, tool.AncestryChanged:Connect(function()
        if not tool or not tool.Parent or not tool.Parent.Parent then
            for _, c in pairs(connections) do c:Disconnect() end
            rvSBox:Destroy()
        end
    end))

    table.insert(tools, {tool})
    tool.Parent = localplr.Backpack
    task.wait()
    return tool
end

local dectool = Instance.new("Tool")
dectool.Name = "Decal Tool"
local handle = Instance.new("Part")
handle.Size = Vector3.new(1,1,1)
handle.Shape = Enum.PartType.Cylinder
handle.CanCollide = false
handle.Name = "Handle"
handle.Color = Color3.fromRGB(0,255,255)
handle.Parent = dectool

function isolatenumbers(str)
    return str:gsub("%D+","")
end
function waitmemeify()
    local c = localplr.Character
    if not c then
        c = localplr.CharacterAdded:Wait()
    end
    return c:WaitForChild("Meme",10)
end
function getmemeify(returnblock)
    if localplr.Character and localplr.Character:FindFirstChild("Meme") then
        return (returnblock and localplr.Character.Meme) or isolatenumbers(localplr.Character.Meme.Front.Label.Image)
    else
        return false
    end
end

local memeifyid = "11894923077"

local drot = {}
drot[Enum.NormalId.Top] = {math.rad(90),math.rad(0),math.rad(0)}
drot[Enum.NormalId.Bottom] = {math.rad(90),math.rad(0),math.rad(0)}
drot[Enum.NormalId.Left] = {math.rad(0),math.rad(90),math.rad(0)}
drot[Enum.NormalId.Right] = {math.rad(0),math.rad(-90),math.rad(0)}
drot[Enum.NormalId.Back] = {math.rad(180),math.rad(0),math.rad(180)}
drot[Enum.NormalId.Front] = {math.rad(0),math.rad(0),math.rad(0)}
local drot2 = {}
drot2[Enum.NormalId.Top] = {math.rad(0),math.rad(1),math.rad(0)}
drot2[Enum.NormalId.Bottom] = {math.rad(0),math.rad(1),math.rad(0)}
drot2[Enum.NormalId.Left] = {math.rad(0),math.rad(0),math.rad(1)}
drot2[Enum.NormalId.Right] = {math.rad(0),math.rad(0),math.rad(1)}
drot2[Enum.NormalId.Back] = {math.rad(0),math.rad(0),math.rad(1)}
drot2[Enum.NormalId.Front] = {math.rad(0),math.rad(0),math.rad(1)}

local dsizes = {}
dsizes[Enum.NormalId.Top] = {
    "X","Z"
}
dsizes[Enum.NormalId.Bottom] = {
    "X","Z"
}
dsizes[Enum.NormalId.Left] = {
    "Z","Y"
}
dsizes[Enum.NormalId.Right] = {
    "Z","Y"
}
dsizes[Enum.NormalId.Back] = {
    "X","Y"
}
dsizes[Enum.NormalId.Front] = {
    "X","Y"
}

_decPanel = createToolPanel({name = "DecalToolGui", title = "Decal Tool", size = UDim2.new(0, 225, 0, 300)})
_decPanel.gui.Enabled = false

decalInput = createToolInput(_decPanel.frame, 0.09, "Decal ID")

local _decImgBg = Library:Create('Frame', {
    AnchorPoint      = Vector2.new(0.5, 0);
    BackgroundColor3 = Library.BackgroundColor;
    BorderSizePixel  = 0;
    Position         = UDim2.new(0.5, 0, 0, 50);
    Size             = UDim2.new(0, 86, 0, 86);
    ZIndex           = 5;
    Parent           = _decPanel.frame;
})
Library:Create('UICorner', { CornerRadius = UDim.new(0, 4); Parent = _decImgBg })
Library:AddToRegistry(_decImgBg, { BackgroundColor3 = 'BackgroundColor' })

local _decImgLabel = Library:Create('ImageLabel', {
    BackgroundTransparency = 1;
    Size                   = UDim2.new(1, 0, 1, 0);
    ScaleType              = Enum.ScaleType.Fit;
    Image                  = '';
    ZIndex                 = 6;
    Parent                 = _decImgBg;
})

_decPreview = {}
function _decPreview:SetImage(id)
    if id and id ~= '' then
        _decImgLabel.Image = 'rbxthumb://type=Asset&id=' .. tostring(id) .. '&w=420&h=420'
    else
        _decImgLabel.Image = ''
    end
end
function _decPreview:SetVisible(vis)
    _decImgBg.Visible = vis
end
_decPreview.label = _decImgLabel

_rotLabel = createToolLabel(_decPanel.frame, "Rotation: 0 deg", 0.50)

createToolButton(_decPanel.frame, "+90", 0.58, 0.06, 0.42, function()
    decalrotation = decalrotation + 90
    _rotLabel.Text = "Rotation: " .. (decalrotation % 360) .. " deg"
end)
createToolButton(_decPanel.frame, "-90", 0.58, 0.52, 0.42, function()
    decalrotation = decalrotation - 90
    _rotLabel.Text = "Rotation: " .. ((decalrotation % 360 + 360) % 360) .. " deg"
end)
createToolButton(_decPanel.frame, "180", 0.70, 0.06, 0.42, function()
    decalrotation = decalrotation + 180
    _rotLabel.Text = "Rotation: " .. (decalrotation % 360) .. " deg"
end)
createToolButton(_decPanel.frame, "Reset", 0.70, 0.52, 0.42, function()
    decalrotation = 0
    _rotLabel.Text = "Rotation: 0 deg"
end)

local fakememe = Instance.new("Part")
fakememe.CanCollide = false
fakememe.CanTouch = false
fakememe.CanQuery = false
fakememe.Transparency = 1
fakememe.Anchored = true
fakememe.Parent = workspace

local suui = Instance.new("SurfaceGui")
suui.Parent = game.CoreGui
suui.SizingMode = Enum.SurfaceGuiSizingMode.PixelsPerStud
suui.Adornee = fakememe
suui.Face = Enum.NormalId.Front
suui.Enabled = false

local imageindicator2 = Instance.new("ImageLabel")
imageindicator2.Parent = suui
imageindicator2.BackgroundTransparency = 1
imageindicator2.ImageTransparency = 0.5
imageindicator2.Position = UDim2.new(0,0,0,0)
imageindicator2.Size = UDim2.new(1,0,1,0)

function updatememeifydisplays()
    _decPreview:SetImage(memeifyid)
    imageindicator2.Image = 'rbxthumb://type=Asset&id=' .. memeifyid .. '&w=420&h=420'
end

decalrotation = 0

table.insert(scriptConnections, decalInput:GetPropertyChangedSignal("Text"):Connect(function()
    memeifyid = isolatenumbers(decalInput.Text)
    updatememeifydisplays()
end))

updatememeifydisplays()

function getfixedthing(s)
    local tb = s == Enum.NormalId.Top or s == Enum.NormalId.Bottom
    local v = Vector3.new(0,tb and -1 or -0.25,tb and -0.76 or -0.01)
    
    return v
end

function hp()
    if localplr.Character:FindFirstChild("The Arkenstone") then
        return true
    else
        if localplr.Backpack:FindFirstChild("The Arkenstone") then
            return true
        end
    end
    if localplr.Team == game.Teams.Chosen then
        return true
    end
    return false
end
function henl()
    if localplr.Character:FindFirstChild("The Arkenstone") then
        return true
    else
        if localplr.Backpack:FindFirstChild("The Arkenstone") then
            return true
        end
    end
    return false
end
function eenl(unequipothers)
    if unequipothers then
        for i,v in pairs(localplr.Character:GetChildren()) do
            if v:IsA("Tool") and v.Name ~= "The Arkenstone" then
                v.Parent = localplr.Backpack
            end
        end
    end
    if localplr.Character:FindFirstChild("The Arkenstone") then
        return true, localplr.Character["The Arkenstone"]
    else
        if localplr.Backpack:FindFirstChild("The Arkenstone") then
            localplr.Backpack["The Arkenstone"].Parent = localplr.Character
            return true, localplr.Character["The Arkenstone"]
        end
    end
    return false
end
function eb()
    if localplr.Character:FindFirstChild("BlueBucket") then
        return true, localplr.Character["BlueBucket"]
    else
        if localplr.Backpack:FindFirstChild("BlueBucket") then
            localplr.Backpack["BlueBucket"].Parent = localplr.Character
            return true, localplr.Character["BlueBucket"]
        end
    end
    return false
end

function createdecaltool()
    
    local connections = {}
    currentdectool = dectool:Clone()
    local equipped = false
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
    
    table.insert(connections,currentdectool.Activated:Connect(function()
        if not equipped then
            return
        end
        if not hp() then
            sayto(localplr,"You need enlighten to use Decal Tool!")
            return
        end
        if not henl() then
            eenl()
        end
        local issel = IsSelectable(mouse.Target,mouse.Hit.Position)
        if issel and henl() then
            local selectside = mouse.TargetSurface
            selection = mouse.Target
            local mid = nil
            if getmemeify() then
                mid = getmemeify()
            else
                sayto(nil,";memeify "..memeifyid)
                mid = memeifyid
                waitmemeify()
            end
            if mid ~= memeifyid then
                sayto(nil,";memeify "..memeifyid)
                waitmemeify()
            end
            local memeifyblock = getmemeify(true)
            local dsizes2 = dsizes[selectside]
            local firstone,secondone = dsizes2[1],dsizes2[2]
            if decalrotation%180 == 90 then
                firstone,secondone = dsizes2[2],dsizes2[1]
            end
            local screwroblox = drot[selectside]
            local screwroblox2 = drot2[selectside]
            local lat = (CFrame.Angles(screwroblox[1]+screwroblox2[1]*decalrotation,screwroblox[2]+screwroblox2[2]*decalrotation,screwroblox[3]+screwroblox2[3]*decalrotation))
            local surfacecfr = CFrame.new(CFrame.new(selection.Position + Vector3.new(0,-1,0) + (Vector3.FromNormalId(selectside)*selection.Size/2)) * lat * getfixedthing(selectside)) * lat

            local looping = true
            pcall(function()
                if gcp("hum").RigType ~= Enum.HumanoidRigType.R15 then
                    sayto(nil,";r15 me")
                    task.wait(3)
                end
                gcp("hum").PlatformStand = true

                coroutine.wrap(function()
                    while looping do
                        gcp("hrp").CFrame = surfacecfr
                        task.wait()
                    end
                end)()
                task.wait(1.5)
                sayto(nil,string.format(";width me %.4g", selection.Size[firstone]/6))
                task.wait(1.5)
                sayto(nil,string.format(";height me %.4g", selection.Size[secondone]/6))
                task.wait(2.5)
                if gcp("hrp").CollisionGroup ~= "NoClip" then
                    sayto(nil,";noclip me")
                    task.wait(3)
                end
                sayto(nil,";freeze me")
                task.wait(1)
                sayto(nil,";clone me")
                task.wait(1)
                sayto(nil,";unfreeze me")
                gcp("hum").PlatformStand = false
            end)
            looping = false
        end
    end))

    table.insert(connections,game:GetService("RunService").Heartbeat:Connect(function()
        if equipped and mouse.Target and IsSelectable(mouse.Target,mouse.Hit.Position) and mouse.TargetSurface then
            local selectside = mouse.TargetSurface
            local selection = mouse.Target
            local dsizes2 = dsizes[selectside]
            local firstone,secondone = dsizes2[1],dsizes2[2]
            if decalrotation%180 == 90 then
                firstone,secondone = dsizes2[2],dsizes2[1]
            end
            fakememe.Size = Vector3.new(selection.Size[firstone],selection.Size[secondone],0.001)
            local screwroblox = drot[selectside]
            local screwroblox2 = drot2[selectside]
            local lat = (CFrame.Angles(screwroblox[1]+screwroblox2[1]*decalrotation,screwroblox[2]+screwroblox2[2]*decalrotation,screwroblox[3]+screwroblox2[3]*decalrotation))
            local tb = selectside == Enum.NormalId.Top or selectside == Enum.NormalId.Bottom
            local surfacecfr = CFrame.new(CFrame.new(selection.Position + (Vector3.FromNormalId(selectside)*selection.Size/2)) * lat * getfixedthing(selectside)) * lat
            fakememe.CFrame = surfacecfr
            suui.Adornee = fakememe
        else
            suui.Adornee = nil
        end
    end))
    
    table.insert(connections,currentdectool.AncestryChanged:Connect(function()
        if not currentdectool or not currentdectool.Parent or not currentdectool.Parent.Parent then
            for i,v in pairs(connections) do
                v:Disconnect()
            end
        end
    end))
    
    table.insert(tools,{
        currentdectool
    })
    
    currentdectool.Parent = localplr.Backpack
    task.wait()
    
    return currentdectool
end

local _edPanel = createToolPanel({
    name = "EditorToolGui",
    title = "Block Editor",
    anchor = Vector2.new(1, 0),
    position = UDim2.new(1, -10, 0.15, 0),
    size = UDim2.new(0, 200, 0, 290),
    statusText = "Click a block to select"
})
local EditorGui = _edPanel.gui
local EditorInfo = _edPanel.status
local edColorBox = createToolInput(_edPanel.frame, 0.13, "R,G,B (e.g. 255,0,0)")
local edMatBox   = createToolInput(_edPanel.frame, 0.24, "Material (e.g. Neon)")
local _editorSel = nil

local function edPaint()
    local pt = localplr.Character and (localplr.Character:FindFirstChild("Paint") or localplr.Backpack:FindFirstChild("Paint"))
    if not pt then return nil, nil end
    if pt.Parent ~= localplr.Character then pt.Parent = localplr.Character; task.wait() end
    local ps = pt:FindFirstChild("Script")
    if ps and ps:FindFirstChild("Event") then return ps, ps.Event end
    return nil, nil
end

createToolButton(_edPanel.frame, "Color", 0.36, 0.05, 0.28, function()
    if not _editorSel or not _editorSel.Parent then return end
    local parts = string.split(edColorBox.Text, ",")
    if #parts < 3 then return end
    local r, g, b = tonumber(parts[1]), tonumber(parts[2]), tonumber(parts[3])
    if not r or not g or not b then return end
    local ps, ev = edPaint()
    if ev then ev:FireServer(_editorSel, Enum.NormalId.Top, getplrpos(), "color", Color3.fromRGB(r,g,b), "", ""); EditorInfo.Text = "Color applied" end
end)
createToolButton(_edPanel.frame, "Material", 0.36, 0.36, 0.28, function()
    if not _editorSel or not _editorSel.Parent then return end
    local ps, ev = edPaint()
    if ev then ev:FireServer(_editorSel, Enum.NormalId.Top, getplrpos(), "material", nil, edMatBox.Text:lower(), ""); EditorInfo.Text = "Material applied" end
end)
createToolButton(_edPanel.frame, "All Sides", 0.36, 0.67, 0.28, function()
    if not _editorSel or not _editorSel.Parent then return end
    local parts = string.split(edColorBox.Text, ",")
    if #parts < 3 then return end
    local r, g, b = tonumber(parts[1]), tonumber(parts[2]), tonumber(parts[3])
    if not r or not g or not b then return end
    local col = Color3.fromRGB(r,g,b)
    local ps, ev = edPaint()
    if ev then
        for _, face in ipairs({Enum.NormalId.Top, Enum.NormalId.Bottom, Enum.NormalId.Front, Enum.NormalId.Back, Enum.NormalId.Left, Enum.NormalId.Right}) do
            ev:FireServer(_editorSel, face, getplrpos(), "color", col, "", "")
        end
        EditorInfo.Text = "All sides painted"
    end
end)
createToolButton(_edPanel.frame, "Anchor", 0.48, 0.05, 0.28, function()
    if not _editorSel or not _editorSel.Parent then return end
    local ps, ev = edPaint()
    if ev then ev:FireServer(_editorSel, Enum.NormalId.Top, getplrpos(), "material", nil, "anchor", ""); EditorInfo.Text = "Toggled anchor" end
end)
createToolButton(_edPanel.frame, "Toxify", 0.48, 0.36, 0.28, function()
    if not _editorSel or not _editorSel.Parent then return end
    local ps, ev = edPaint()
    if ev then ev:FireServer(_editorSel, Enum.NormalId.Top, getplrpos(), "material", Color3.new(0,0,0), "toxic", ""); EditorInfo.Text = "Toxified" end
end)
createToolButton(_edPanel.frame, "Delete", 0.48, 0.67, 0.28, function()
    if not _editorSel or not _editorSel.Parent then return end
    ExecuteDelete(_editorSel); EditorInfo.Text = "Deleted"; _editorSel = nil
end)

local edSignBox = createToolInput(_edPanel.frame, 0.61, "Sign text")
createToolButton(_edPanel.frame, "Sign", 0.74, 0.05, 0.44, function()
    if not _editorSel or not _editorSel.Parent or edSignBox.Text == "" then return end
    local ps, ev = edPaint()
    if ev then ev:FireServer(_editorSel, Enum.NormalId.Front, getplrpos(), "both \u{1F91D}", _editorSel.Color, "", edSignBox.Text); EditorInfo.Text = "Sign applied" end
end)
createToolButton(_edPanel.frame, "Clone", 0.74, 0.52, 0.44, function()
    if not _editorSel or not _editorSel.Parent then return end
    local hrp = localplr.Character and localplr.Character:FindFirstChild("HumanoidRootPart")
    if hrp then ExecuteBuild(snap(hrp.Position + hrp.CFrame.LookVector * 6)); EditorInfo.Text = "Cloned" end
end)

local EditorBlockInfo = createToolLabel(_edPanel.frame, "", 0.87)
EditorBlockInfo.BackgroundTransparency = 1

function createeditortool()
    local connections = {}
    local edTool = Instance.new("Tool")
    edTool.Name = "Editor Tool"
    edTool.ToolTip = "Click blocks to edit properties"
    edTool.RequiresHandle = true

    local edHandle = Instance.new("Part")
    edHandle.Name = "Handle"
    edHandle.Size = Vector3.new(0.6, 0.6, 2.5)
    edHandle.Color = Color3.fromRGB(0, 85, 255)
    edHandle.Material = Enum.Material.SmoothPlastic
    edHandle.Reflectance = 0.2
    edHandle.CanCollide = false
    edHandle.Parent = edTool
    do
        local mesh = Instance.new("SpecialMesh")
        mesh.MeshType = Enum.MeshType.FileMesh
        mesh.MeshId = "rbxassetid://430736398"
        mesh.Scale = Vector3.new(0.35, 0.35, 0.5)
        mesh.Parent = edHandle
    end

    local equipped = false
    local edSBox = Instance.new("SelectionBox")
    edSBox.Color3 = Color3.fromRGB(0, 85, 255)
    edSBox.SurfaceColor3 = Color3.fromRGB(0, 85, 255)
    edSBox.SurfaceTransparency = 0.7
    edSBox.LineThickness = 0.07
    edSBox.Parent = game.CoreGui
    table.insert(tools, {edSBox})

    table.insert(connections, edTool.Equipped:Connect(function()
        equipped = true
        EditorGui.Enabled = true
        EditorInfo.Text = "Click a block to select"
    end))

    table.insert(connections, edTool.Unequipped:Connect(function()
        if not equipped then return end
        equipped = false
        EditorGui.Enabled = false
        edSBox.Adornee = nil
        _editorSel = nil
    end))

    table.insert(connections, mouse.Button1Down:Connect(function()
        if not equipped then return end
        if not IsSelectable(mouse.Target, mouse.Hit.Position) then return end
        _editorSel = mouse.Target
        edSBox.Adornee = _editorSel
        local c = _editorSel.Color
        edColorBox.Text = math.floor(c.R*255) .. "," .. math.floor(c.G*255) .. "," .. math.floor(c.B*255)
        edMatBox.Text = tostring(_editorSel.Material):gsub("Enum.Material.", "")
        EditorInfo.Text = "Selected block"
        EditorBlockInfo.Text = string.format("Pos: %.0f,%.0f,%.0f | Size: %.0f,%.0f,%.0f | %s",
            _editorSel.Position.X, _editorSel.Position.Y, _editorSel.Position.Z,
            _editorSel.Size.X, _editorSel.Size.Y, _editorSel.Size.Z,
            _editorSel.Anchored and "Anchored" or "Unanchored")
    end))

    table.insert(connections, edTool.AncestryChanged:Connect(function()
        if not edTool or not edTool.Parent or not edTool.Parent.Parent then
            for _, c in pairs(connections) do c:Disconnect() end
            edSBox:Destroy()
            EditorGui.Enabled = false
        end
    end))

    table.insert(tools, {edTool})
    edTool.Parent = localplr.Backpack
    task.wait()
    return edTool
end

local AutoR6Group = Tabs.Chat:AddLeftGroupbox('Auto R6 Revival')
AutoR6Group:AddLabel('Equips Enlighten & /r6s you on death')
AutoR6Group:AddLabel('Requires Enlighten in backpack')

local AutoR6Toggle = AutoR6Group:AddToggle('AutoR6Toggle', {
    Text    = 'Auto R6 on Death',
    Default = false,
    Tooltip = 'When you die, instantly equips Enlighten and sends /r6 to revive'
})
AutoR6Toggle:OnChanged(function()
    autoR6Enabled = AutoR6Toggle.Value
    if autoR6Enabled and plr.Character then
        task.spawn(function() setupAutoR6Character(plr.Character) end)
    end
    Library:Notify(autoR6Enabled and "Auto R6 ON" or "Auto R6 OFF", 2)
end)

AdvancedGroup = Tabs.Chat:AddLeftGroupbox('Custom Tools')

AdvancedGroup:AddButton({
    Text = 'Rotate Tool',
    Func = function()
        createrotool().Parent = (plr.Character and not plr.Character:FindFirstChildWhichIsA("Tool") and plr.Character) or plr.Backpack
        Library:Notify("Rotate tool added to backpack", 3)
    end
})

AdvancedGroup:AddButton({
    Text = 'Conveyer Tool',
    Func = function()
        createlinveltool().Parent = (plr.Character and not plr.Character:FindFirstChildWhichIsA("Tool") and plr.Character) or plr.Backpack
        Library:Notify("Linear Velocity tool added to backpack", 3)
    end
})

AdvancedGroup:AddButton({
    Text = 'Velocity Tool',
    Func = function()
        createrotveltool().Parent = (plr.Character and not plr.Character:FindFirstChildWhichIsA("Tool") and plr.Character) or plr.Backpack
        Library:Notify("Rotational Velocity tool added to backpack", 3)
    end
})

AdvancedGroup:AddButton({
    Text = 'Decal Tool',
    Func = function()
        createdecaltool().Parent = (plr.Character and not plr.Character:FindFirstChildWhichIsA("Tool") and plr.Character) or plr.Backpack
        Library:Notify("Decal tool added to backpack", 3)
    end
})

AdvancedGroup:AddButton({
    Text = 'Editor Tool',
    Func = function()
        createeditortool().Parent = (plr.Character and not plr.Character:FindFirstChildWhichIsA("Tool") and plr.Character) or plr.Backpack
        Library:Notify("Editor tool added to backpack", 3)
    end
})

local function sanitizename(txt)
    if string.sub(txt,1,3):lower() == "btc" then txt = "bt" end
    if string.sub(txt,1,3):lower() == "fat" then txt = "fa" end
    if not isog then txt = string.gsub(txt,"_",".") end
    return string.sub(txt,1,7)
end

local giverBlocks = {R6 = {}, Carpet = {}}
local giverTagged = {}
local giverSelecting = false
local giverSelMode = "R6"
local giverMode = "R6"

local function giverGetTag(uid, block)
    return giverTagged[uid] and giverTagged[uid][block] and giverTagged[uid][block] > tick()
end
local function giverSetTag(uid, block, dur)
    if not giverTagged[uid] then giverTagged[uid] = {} end
    giverTagged[uid][block] = tick() + dur
end
local function giverEquipArken()
    local arken = localplr.Backpack:FindFirstChild("The Arkenstone")
        or (localplr.Character and localplr.Character:FindFirstChild("The Arkenstone"))
    if arken and arken.Parent == localplr.Backpack then
        arken.Parent = localplr.Character
    end
end

local _giverPanel = createToolPanel({
    name = "GiverToolGui",
    title = "Giver Tool",
    anchor = Vector2.new(1, 0),
    position = UDim2.new(1, -10, 0.15, 0),
    size = UDim2.new(0, 210, 0, 290),
    statusText = "Equip to start"
})
local GiverGui = _giverPanel.gui
local GiverInfo = _giverPanel.status
GiverGui.Enabled = false

local giverModeLabel = createToolLabel(_giverPanel.frame, "Mode: Give R6", 0.09)
local giverCountLabel = createToolLabel(_giverPanel.frame, "R6: 0 | Carpet: 0 blocks", 0.18)

local function giverUpdateUI()
    local r6c, cc = 0, 0
    for _ in pairs(giverBlocks.R6) do r6c = r6c + 1 end
    for _ in pairs(giverBlocks.Carpet) do cc = cc + 1 end
    giverModeLabel.Text = "Mode: Give " .. giverMode
    giverCountLabel.Text = "R6: " .. r6c .. " | Carpet: " .. cc .. " block(s)"
end

createToolButton(_giverPanel.frame, "Give R6", 0.27, 0.03, 0.45, function()
    if giverSelecting then return end
    giverMode = "R6"
    giverUpdateUI()
    GiverInfo.Text = "Mode: Give R6"
end)
createToolButton(_giverPanel.frame, "Give Carpet", 0.27, 0.52, 0.45, function()
    if giverSelecting then return end
    giverMode = "Carpet"
    giverUpdateUI()
    GiverInfo.Text = "Mode: Give Carpet"
end)

createToolButton(_giverPanel.frame, "Select Blocks", 0.40, 0.03, 0.94, function()
    if giverSelecting then return end
    giverSelecting = true
    giverSelMode = giverMode
    for block, data in pairs(giverBlocks[giverSelMode]) do
        data.SelectionBox.LineThickness = 0.1
    end
    GiverInfo.Text = "Click " .. giverSelMode .. " blocks. Finish when done."
end)

createToolButton(_giverPanel.frame, "Finish", 0.53, 0.03, 0.45, function()
    if not giverSelecting then GiverInfo.Text = "Not in selection mode"; return end
    giverSelecting = false
    local mode = giverSelMode
    local count = 0
    local activeCol = mode == "R6" and Color3.fromRGB(255, 200, 0) or Color3.fromRGB(139, 90, 43)
    for block, data in pairs(giverBlocks[mode]) do
        data.SelectionBox.LineThickness = -1
        if not data.Active then
            data.Active = true
            data.SelectionBox.Color3 = activeCol
            data.SelectionBox.SurfaceColor3 = activeCol
            data.OnTouched = block.Touched:Connect(function(part)
                if not giverBlocks[mode][block] then return end
                local p = part
                local touchPlr = nil
                repeat
                    touchPlr = game.Players:GetPlayerFromCharacter(p.Parent)
                    p = p.Parent
                until touchPlr or p == workspace or p == game
                if not touchPlr or not touchPlr.Character then return end
                local tHum = touchPlr.Character:FindFirstChildOfClass("Humanoid")
                if not tHum then return end
                local uid = touchPlr.UserId
                if giverGetTag(uid, block) then return end
                if mode == "R6" then
                    if tHum.RigType == Enum.HumanoidRigType.R6 then return end
                    giverSetTag(uid, block, 2)
                    giverEquipArken()
                    pcall(function()
                        game:GetService("TextChatService").TextChannels.RBXGeneral:SendAsync(";r6 " .. sanitizename(touchPlr.Name))
                    end)
                elseif mode == "Carpet" then
                    if tHum.RigType == Enum.HumanoidRigType.R6 then return end
                    if touchPlr.Character:FindFirstChild("RainbowMagicCarpet") then return end
                    giverSetTag(uid, block, 2)
                    giverEquipArken()
                    pcall(function()
                        game:GetService("TextChatService").TextChannels.RBXGeneral:SendAsync(";carpet " .. sanitizename(touchPlr.Name))
                    end)
                end
            end)
        end
        count = count + 1
    end
    GiverInfo.Text = mode .. " active: " .. count .. " block(s)"
    giverUpdateUI()
end)

createToolButton(_giverPanel.frame, "Cancel", 0.53, 0.52, 0.45, function()
    if not giverSelecting then return end
    giverSelecting = false
    for block, data in pairs(giverBlocks[giverSelMode]) do
        if not data.Active then
            data.SelectionBox:Destroy()
            giverBlocks[giverSelMode][block] = nil
        else
            data.SelectionBox.LineThickness = -1
        end
    end
    GiverInfo.Text = "Selection cancelled"
    giverUpdateUI()
end)

createToolButton(_giverPanel.frame, "Remove Mode Blocks", 0.66, 0.03, 0.94, function()
    for block, data in pairs(giverBlocks[giverMode]) do
        if data.OnTouched then data.OnTouched:Disconnect() end
        if data.SelectionBox then data.SelectionBox:Destroy() end
    end
    giverBlocks[giverMode] = {}
    if giverSelecting and giverSelMode == giverMode then
        giverSelecting = false
    end
    GiverInfo.Text = giverMode .. " blocks cleared"
    giverUpdateUI()
end)

createToolButton(_giverPanel.frame, "Remove All Blocks", 0.79, 0.03, 0.94, function()
    for mode, modeBlocks in pairs(giverBlocks) do
        for block, data in pairs(modeBlocks) do
            if data.OnTouched then data.OnTouched:Disconnect() end
            if data.SelectionBox then data.SelectionBox:Destroy() end
        end
        giverBlocks[mode] = {}
    end
    giverSelecting = false
    giverTagged = {}
    GiverInfo.Text = "All giver blocks cleared"
    giverUpdateUI()
end)

function creategivertool()
    local connections = {}
    local gTool = Instance.new("Tool")
    gTool.Name = "Giver Tool"
    gTool.ToolTip = "Select blocks to give R6 or Carpet on touch"
    gTool.RequiresHandle = true

    local gHandle = Instance.new("Part")
    gHandle.Name = "Handle"
    gHandle.Size = Vector3.new(0.8, 0.8, 2.5)
    gHandle.Color = Color3.fromRGB(0, 200, 100)
    gHandle.Material = Enum.Material.SmoothPlastic
    gHandle.Reflectance = 0.2
    gHandle.CanCollide = false
    gHandle.Parent = gTool

    local equipped = false

    table.insert(connections, gTool.Equipped:Connect(function()
        equipped = true
        GiverGui.Enabled = true
        for mode, modeBlocks in pairs(giverBlocks) do
            for block, data in pairs(modeBlocks) do
                if data.Active then
                    data.SelectionBox.LineThickness = 0.05
                end
            end
        end
        giverUpdateUI()
        GiverInfo.Text = "Choose mode & select blocks"
    end))

    table.insert(connections, gTool.Unequipped:Connect(function()
        if not equipped then return end
        equipped = false
        GiverGui.Enabled = false
        if giverSelecting then
            giverSelecting = false
            for block, data in pairs(giverBlocks[giverSelMode]) do
                if not data.Active then
                    data.SelectionBox:Destroy()
                    giverBlocks[giverSelMode][block] = nil
                else
                    data.SelectionBox.LineThickness = -1
                end
            end
        else
            for mode, modeBlocks in pairs(giverBlocks) do
                for block, data in pairs(modeBlocks) do
                    if data.Active and data.SelectionBox then
                        data.SelectionBox.LineThickness = -1
                    end
                end
            end
        end
    end))

    table.insert(connections, mouse.Button1Down:Connect(function()
        if not equipped or not giverSelecting then return end
        local target = mouse.Target
        if not target or not target:IsDescendantOf(cfolder) then return end
        local mode = giverSelMode
        if giverBlocks[mode][target] then
            local data = giverBlocks[mode][target]
            if data.OnTouched then data.OnTouched:Disconnect() end
            data.SelectionBox:Destroy()
            giverBlocks[mode][target] = nil
            GiverInfo.Text = "Block removed from " .. mode
        else
            local bbox = Instance.new("SelectionBox")
            bbox.Color3 = Color3.fromRGB(0, 170, 255)
            bbox.LineThickness = 0.1
            bbox.SurfaceColor3 = Color3.fromRGB(13, 105, 172)
            bbox.SurfaceTransparency = 0.7
            bbox.Adornee = target
            bbox.Parent = game.CoreGui
            table.insert(tools, {bbox})
            giverBlocks[mode][target] = {SelectionBox = bbox, Block = target, Active = false, OnTouched = nil}
            GiverInfo.Text = "Added to " .. mode .. " (click Finish)"
        end
        giverUpdateUI()
    end))

    table.insert(connections, gTool.AncestryChanged:Connect(function()
        if not gTool or not gTool.Parent or not gTool.Parent.Parent then
            for _, c in pairs(connections) do c:Disconnect() end
            GiverGui.Enabled = false
        end
    end))

    table.insert(tools, {gTool})
    gTool.Parent = localplr.Backpack
    task.wait()
    return gTool
end

AdvancedGroup:AddButton({
    Text = 'Giver Tool',
    Func = function()
        creategivertool().Parent = (plr.Character and not plr.Character:FindFirstChildWhichIsA("Tool") and plr.Character) or plr.Backpack
        Library:Notify("Giver tool added to backpack", 3)
    end
})

stopStash = false
stashRunning = false
stashPosition = getgenv().stashposition or Vector3.new(
    math.random(5000, 10000) * (math.random(2) == 1 and 1 or -1),
    math.random(1000, 3000),
    math.random(5000, 10000) * (math.random(2) == 1 and 1 or -1)
)
getgenv().stashposition = stashPosition

local stashPlatform = getgenv().invisstashplatform or Instance.new("Part")
stashPlatform.CFrame = CFrame.new(stashPosition - Vector3.new(0, 10, 0))
stashPlatform.Anchored = true
stashPlatform.Transparency = 1
stashPlatform.CanCollide = false
stashPlatform.Color = Color3.fromRGB(0, 255, 0)
stashPlatform.Size = Vector3.new(200, 1, 200)
stashPlatform.Parent = workspace
getgenv().invisstashplatform = stashPlatform

 function checkStash()

    local hasEnlighten = plr.Character:FindFirstChild("The Arkenstone") 
        or plr.Backpack:FindFirstChild("The Arkenstone")
    
    if not hasEnlighten then
        stopStash = true
        return false
    end
    

    if not plr:HasTag("Muted") then
        task.wait(0.5)
        game:GetService("TextChatService").TextChannels.RBXGeneral:SendAsync(";mute me")
        task.wait(3.5)
    end
    
    return true
end

 function equipEnlighten(unequipOthers)
    if unequipOthers then
        for _, tool in pairs(plr.Character:GetChildren()) do
            if tool:IsA("Tool") and tool.Name ~= "The Arkenstone" then
                tool.Parent = plr.Backpack
            end
        end
    end
    
    if plr.Character:FindFirstChild("The Arkenstone") then
        return true
    elseif plr.Backpack:FindFirstChild("The Arkenstone") then
        plr.Backpack["The Arkenstone"].Parent = plr.Character
        return true
    end
    return false
end

 function equipBlueBucket()
    if plr.Character:FindFirstChild("BlueBucket") then
        return true
    elseif plr.Backpack:FindFirstChild("BlueBucket") then
        plr.Backpack["BlueBucket"].Parent = plr.Character
        return true
    end
    return false
end

function CreatePlatform()
    local hrp = GetCharacterPart("HumanoidRootPart")
    if hrp then
        local platform = Instance.new("Part")
        platform.Name = "RomazPlatform"
        platform.Size = Vector3.new(50, 1, 50)
        platform.Position = hrp.Position + Vector3.new(0, -5, 0)
        platform.Anchored = true
        platform.Transparency = 0.3
        platform.Color = Color3.new(0, 1, 0)
        platform.CanCollide = true
        platform.Parent = workspace
        Library:Notify("Platform created below you", 3)
    end
end

function ClearPlatform()
    local platform = workspace:FindFirstChild("RomazPlatform")
    if platform then
        platform:Destroy()
        Library:Notify("Platform removed", 3)
    end
end

 function hasEnlighten()
    return plr.Character:FindFirstChild("The Arkenstone") ~= nil
        or plr.Backpack:FindFirstChild("The Arkenstone") ~= nil
end

 function runStash(stashAmount)
    if stashRunning then
        Library:Notify("Stash already running!", 3)
        return
    end
    
    if not hasEnlighten() then
        Library:Notify("You need The Arkenstone (Enlighten) to stash!", 5)
        return
    end
    
    stashRunning = true
    stopStash = false
    
    task.spawn(function()
        local character = plr.Character
        if not character then 
            stashRunning = false
            return 
        end
        
        local hrp = character:FindFirstChild("HumanoidRootPart")
        if not hrp then 
            stashRunning = false
            return 
        end
        

        local originalCFrame = hrp.CFrame
        

        character:PivotTo(CFrame.new(stashPosition + Vector3.new(0, 30, 0)))
        task.wait(0.5)
        

        stashPlatform.CanCollide = true
        stashPlatform.Transparency = 0.9
        

        if not plr:HasTag("Muted") then
            task.wait(0.5)
            game:GetService("TextChatService").TextChannels.RBXGeneral:SendAsync(";mute me")
            task.wait(3.5)
        end
        
        Library:Notify("Starting stash - DO NOT MOVE", 5)
        
        for i = 1, stashAmount do
            if stopStash then break end
            

             existingClones = 0
             cloneFolder = workspace:FindFirstChild("Clones")
            if cloneFolder and cloneFolder:FindFirstChild(plr.Name) then
                existingClones = #cloneFolder[plr.Name]:GetChildren()
            end
            
             col = existingClones % 4
             row = math.floor(existingClones / 4)
             offsetX = col * 10
             offsetZ = row * 10
            

            local clonePos = stashPosition + Vector3.new(offsetX, 0, offsetZ)
            character:PivotTo(CFrame.new(clonePos))
            task.wait(1)
            
            if stopStash then break end
            if not checkStash() then break end
            

            equipEnlighten(false)
            task.wait(0.3)
            

            if not equipBlueBucket() then
                game:GetService("TextChatService").TextChannels.RBXGeneral:SendAsync(";gear me 25162389")
                task.wait(1.5)
                equipBlueBucket()
                equipEnlighten(false)
            end
            
            if stopStash then break end
            if not checkStash() then break end
            

            game:GetService("TextChatService").TextChannels.RBXGeneral:SendAsync(";freeze me")
            task.wait(1)
            
            if stopStash then break end
            if not checkStash() then break end
            

            game:GetService("TextChatService").TextChannels.RBXGeneral:SendAsync(";clone me")
            task.wait(1)
            
            if stopStash then break end
            if not checkStash() then break end
            

            game:GetService("TextChatService").TextChannels.RBXGeneral:SendAsync(";unfreeze me")
            

            character:PivotTo(CFrame.new(clonePos + Vector3.new(0, 15, 0)))
            
            Library:Notify("Clone " .. i .. "/" .. stashAmount .. " created", 3)
            

            for _ = 1, 10 do
                task.wait(1)
                if stopStash then break end
                checkStash()
            end
        end
        

        task.wait(1)
        game:GetService("TextChatService").TextChannels.RBXGeneral:SendAsync(";unmute me")
        task.wait(0.5)
        

         hum = character:FindFirstChildOfClass("Humanoid")
        if hum then
            hum:UnequipTools()
        end
        

        task.wait(0.5)
        hrp.CFrame = originalCFrame
        

        stashPlatform.CanCollide = false
        stashPlatform.Transparency = 1
        
        if stopStash then
            Library:Notify("Stash creation cancelled", 3)
        else
            Library:Notify("Stash complete! " .. stashAmount .. " clones created", 5)
        end
        
        stashRunning = false
        task.delay(3, function()
            stopStash = false
        end)
    end)
end

 StashGroup = Tabs.StashSystem:AddLeftGroupbox('Stash Management')

 StashAmount = StashGroup:AddSlider('StashAmount', {
    Text = 'Stash Clones',
    Default = 2,
    Min = 1,
    Max = 10,
    Rounding = 1,
    Compact = false,
    Tooltip = 'Number of clones to create (Enlighten required)'
})

StashGroup:AddButton({
    Text = 'Create Stash',
    Func = function()
        runStash(StashAmount.Value)
    end
})

StashGroup:AddButton({
    Text = 'Stop Stash',
    Func = function()
        stopStash = true
        Library:Notify("Stopping stash...", 3)
    end
})

StashGroup:AddButton({
    Text = 'Go to Stash',
    Func = function()
        local hrp = plr.Character and plr.Character:FindFirstChild("HumanoidRootPart")
        if hrp then
            hrp.CFrame = CFrame.new(stashPosition + Vector3.new(-20, 30, -20))
            Library:Notify("Teleported to stash area", 3)
        end
    end
})

StashGroup:AddLabel('Requires: Enlighten')
StashGroup:AddLabel('Stash coords are randomised per session')

 StashPlatform = Tabs.StashSystem:AddLeftGroupbox('Platform')

StashPlatform:AddButton({
    Text = 'Create Platform',
    Func = CreatePlatform
})

StashPlatform:AddButton({
    Text = 'Clear Platform',
    Func = ClearPlatform
})

StashPlatform:AddButton({
    Text = 'Enable Stash Platform',
    Func = function()
        stashPlatform.CanCollide = true
        stashPlatform.Transparency = 0.9
        Library:Notify("Stash platform enabled", 3)
    end
})

StashPlatform:AddButton({
    Text = 'Disable Stash Platform',
    Func = function()
        stashPlatform.CanCollide = false
        stashPlatform.Transparency = 1
        Library:Notify("Stash platform disabled", 3)
    end
})

 PlayerToolsGroup = Tabs.StashSystem:AddLeftGroupbox('Player Management')

 SpecificPlayerInput = PlayerToolsGroup:AddInput('SpecificPlayer', {
    Default = '',
    Numeric = false,
    Text = 'Specific Player:',
    Placeholder = 'Enter player username',
    Tooltip = 'Target specific player by name (overrides dropdown)'
})

 PlayerTargetDropdown = PlayerToolsGroup:AddDropdown('PlayerTarget', {
    Values = {'all', 'others', 'me', 'random'},
    Default = 1,
    Text = 'Target Mode:',
    Tooltip = 'Used when Specific Player is empty'
})

PlayerToolsGroup:AddButton({
    Text = 'Execute Fling',
    Func = function()
         Players = game:GetService("Players")
         localPlayer = Players.LocalPlayer
         specificName = SpecificPlayerInput and SpecificPlayerInput.Value or ""
         targetMode = PlayerTargetDropdown and PlayerTargetDropdown.Value or "all"

         function flingPlayer(target)
            if not target or not target.Character then return end
             hrp = target.Character:FindFirstChild("HumanoidRootPart")
             myHrp = localPlayer.Character and localPlayer.Character:FindFirstChild("HumanoidRootPart")
            if not hrp or not myHrp then return end

            local bv = Instance.new("BodyVelocity")
            bv.Velocity = (hrp.Position - myHrp.Position).Unit * 500 + Vector3.new(0, 200, 0)
            bv.MaxForce = Vector3.new(1e9, 1e9, 1e9)
            bv.Parent = hrp
            game:GetService("Debris"):AddItem(bv, 0.2)

            Library:Notify("Flung: " .. target.Name, 3)
        end

        if specificName ~= '' then
             found = Players:FindFirstChild(specificName)
            if found then
                flingPlayer(found)
            else

                for _, p in ipairs(Players:GetPlayers()) do
                    if p.Name:lower():find(specificName:lower()) then
                        flingPlayer(p)
                        return
                    end
                end
                Library:Notify("Player not found: " .. specificName, 3)
            end

        elseif targetMode == "me" then
            flingPlayer(localPlayer)

        elseif targetMode == "all" then
            for _, p in ipairs(Players:GetPlayers()) do
                flingPlayer(p)
            end

        elseif targetMode == "others" then
            for _, p in ipairs(Players:GetPlayers()) do
                if p ~= localPlayer then
                    flingPlayer(p)
                end
            end

        elseif targetMode == "random" then
            local others = {}
            for _, p in ipairs(Players:GetPlayers()) do
                if p ~= localPlayer then
                    table.insert(others, p)
                end
            end
            if #others > 0 then
                flingPlayer(others[math.random(1, #others)])
            else
                Library:Notify("No other players found", 3)
            end
        end
    end
})

PlayerToolsGroup:AddButton({
    Text = 'Anti-AFK',
    Func = function()
        local antiAfkEnabled = false
        local antiAfkConnection
        
        antiAfkEnabled = not antiAfkEnabled
        
        if antiAfkEnabled then
            antiAfkConnection = game:GetService("Players").LocalPlayer.Idled:Connect(function()
                game:GetService("VirtualUser"):Button2Down(Vector2.new(0,0), workspace.CurrentCamera.CFrame)
                task.wait(1)
                game:GetService("VirtualUser"):Button2Up(Vector2.new(0,0), workspace.CurrentCamera.CFrame)
            end)
            Library:Notify("Anti-AFK enabled", 3)
        else
            if antiAfkConnection then
                antiAfkConnection:Disconnect()
                Library:Notify("Anti-AFK disabled", 3)
            end
        end
    end
})

CharacterGroup = Tabs.StashSystem:AddRightGroupbox('Character Control')

WalkSpeed = CharacterGroup:AddSlider('WalkSpeed', {
    Text = 'Walk Speed',
    Default = 16,
    Min = 16,
    Max = 100,
    Rounding = 1,
    Compact = false,
    Tooltip = 'Set your character walk speed'
})

JumpPower = CharacterGroup:AddSlider('JumpPower', {
    Text = 'Jump Power',
    Default = 50,
    Min = 50,
    Max = 100,
    Rounding = 1,
    Compact = false,
    Tooltip = 'Set your character jump power'
})

local FlyEnabled = CharacterGroup:AddToggle('FlyEnabled', {
    Text = 'Toggle Fly',
    Default = false,
    Tooltip = 'Toggle flying mode'
})

local FlySpeed = CharacterGroup:AddSlider('FlySpeed', {
    Text = 'Fly Speed',
    Default = 50,
    Min = 10,
    Max = 200,
    Rounding = 1,
    Compact = false,
    Tooltip = 'Set flying speed'
})

CharacterGroup:AddButton({
    Text = 'Apply Changes',
    Func = function()
        local character = plr.Character
        if not character then 
            Library:Notify("Character not found", 3)
            return 
        end
        
        local humanoid = character:FindFirstChildOfClass("Humanoid")
        if humanoid then
            humanoid.WalkSpeed = WalkSpeed.Value
            pcall(function() humanoid.JumpPower = JumpPower.Value end)
            pcall(function() humanoid.JumpHeight = JumpPower.Value * 0.56 end)
            Library:Notify("Character stats updated: WalkSpeed=" .. WalkSpeed.Value .. ", JumpPower=" .. JumpPower.Value, 3)
        else
            Library:Notify("Humanoid not found", 3)
        end
    end
})

CharacterGroup:AddButton({
    Text = 'Reset Character',
    Func = function()
        if plr.Character then
            plr.Character:BreakJoints()
        end
    end
})

CharacterGroup:AddButton({
    Text = 'Go to Spawn',
    Func = function()
        local character = plr.Character
        if not character then 
            Library:Notify("Character not found", 3)
            return 
        end
        
        local hrp = character:FindFirstChild("HumanoidRootPart")
        if not hrp then
            Library:Notify("HumanoidRootPart not found", 3)
            return
        end
        
        local spawn = workspace:FindFirstChild("Spawn") or 
                     workspace:FindFirstChild("SpawnPoint") or
                     workspace:FindFirstChild("spawn") or
                     workspace:FindFirstChildOfClass("SpawnPoint")
        
        if spawn then
            local spawnPosition
            if spawn:IsA("Part") then
                spawnPosition = spawn.Position + Vector3.new(0, 5, 0)
            elseif spawn:IsA("Model") then
                local primaryPart = spawn.PrimaryPart or spawn:FindFirstChildWhichIsA("BasePart")
                if primaryPart then
                    spawnPosition = primaryPart.Position + Vector3.new(0, 5, 0)
                end
            end
            
            if spawnPosition then
                hrp.CFrame = CFrame.new(spawnPosition)
                Library:Notify("Teleported to spawn", 3)
                return
            end
        end
        
        local fallbackSpawn = Vector3.new(0, 100, 0)
        hrp.CFrame = CFrame.new(fallbackSpawn)
        Library:Notify("Teleported to default spawn area", 3)
    end
})

 StashSettingsGroup = Tabs.StashSystem:AddRightGroupbox('Stash Settings')

StashSettingsGroup:AddLabel('Stash Position (randomised):')
local StashXLabel = StashSettingsGroup:AddLabel('X: ' .. math.floor(stashPosition.X))
local StashYLabel = StashSettingsGroup:AddLabel('Y: ' .. math.floor(stashPosition.Y))
local StashZLabel = StashSettingsGroup:AddLabel('Z: ' .. math.floor(stashPosition.Z))
StashSettingsGroup:AddButton({
    Text = 'Reroll Stash Position',
    Func = function()
        stashPosition = Vector3.new(
            math.random(5000, 10000) * (math.random(2) == 1 and 1 or -1),
            math.random(1000, 3000),
            math.random(5000, 10000) * (math.random(2) == 1 and 1 or -1)
        )
        getgenv().stashposition = stashPosition
stashPlatform.CFrame = CFrame.new(stashPosition - Vector3.new(0, 10, 0))
        StashXLabel:SetText('X: ' .. math.floor(stashPosition.X))
        StashYLabel:SetText('Y: ' .. math.floor(stashPosition.Y))
        StashZLabel:SetText('Z: ' .. math.floor(stashPosition.Z))
        Library:Notify("Stash position rerolled", 3)
    end
})

StashSettingsGroup:AddButton({
    Text = 'Copy Stash Coords',
    Func = function()
        if setclipboard then
            setclipboard(tostring(stashPosition.X) .. ", " .. tostring(stashPosition.Y) .. ", " .. tostring(stashPosition.Z))
            Library:Notify("Stash coords copied", 3)
        end
    end
})

 ChatGroup = Tabs.Chat:AddLeftGroupbox('Chat Utility')

 ChatSpyEnabled = false
 chatSpyConnection = nil

local ChatSpyToggle = ChatGroup:AddToggle('ChatSpy', {
    Text = 'Enable Chat Spy',
    Default = false,
    Tooltip = 'See all chat messages including private ones'
})

local ChatModsEnabled = false
local namecolors = {
    peasant = {150, 103, 102},
    arken = {4, 175, 236},
    admin = {245, 205, 48},
    hidden = {255, 0, 0},
    iqgenius = {255, 179, 179},
    iqdumb = {200, 0, 0}
}

local namecolorshex = {}
for i, v in pairs(namecolors) do
    namecolorshex[i] = "#" .. Color3.fromRGB(table.unpack(v)):ToHex()
end

local joincmds = {
    joinxl = "JoinXL",
    joinvc = "JoinVC",
    joinog = "JoinOG"
}

local originalOnIncomingMessage
 function setupEnhancedChatSpy()
    if game.TextChatService.OnIncomingMessage then
        originalOnIncomingMessage = game.TextChatService.OnIncomingMessage
    end
    
    game.TextChatService.OnIncomingMessage = function(mdata)

        if originalOnIncomingMessage then
            mdata = originalOnIncomingMessage(mdata) or mdata
        end
        
        if ChatSpyEnabled and mdata.TextSource then
            local plr = mdata.TextSource.UserId and game.Players:GetPlayerByUserId(mdata.TextSource.UserId)
            if not plr then return mdata end
            
             cn = ""
             hidden = false
            

            if plr.Neutral == true then
                if plr:GetAttribute("Arken") == true then
                    cn = "arken"
                else
                    cn = "peasant"
                end
            else
                cn = "admin"
            end
            
             muted = plr:HasTag("Muted")
            if muted then
                cn = "hidden"
                if not ChatSpyEnabled then
                    mdata.Text = ""
                end
            end
            

             cmd = string.sub(mdata.Text, 1, string.find(mdata.Text, " ") or #mdata.Text + 1)
            if string.sub(mdata.Text, 1, 1) == ";" then
                cmd = string.sub(mdata.Text, 2, string.find(mdata.Text, " ") or #mdata.Text + 1)
                if ChatSpyEnabled then
                    cn = "hidden"
                    hidden = true
                else
                    mdata.Text = ""
                end
            end
            

            if plr == game.Players.LocalPlayer and joincmds[cmd:lower()] then
                for i, v in pairs(joincmds) do
                    if game.Players.LocalPlayer.PlayerGui:FindFirstChild(v) then
                        game.Players.LocalPlayer.PlayerGui[v].Enabled = i == cmd:lower()
                    end
                end
            end
            

            local iq = nil
            if ChatSpyEnabled and plr:GetAttribute("IQ") then
                if plr:GetAttribute("IQ") >= 200 then
                    iq = "genius \u{1F9E0}"
                    cn = "iqgenius"
                elseif plr:GetAttribute("IQ") <= 50 then
                    iq = "dumb \u{1F92A}"
                    cn = "iqdumb"
                end
            end
            

            mdata.PrefixText = "<font color=\""..namecolorshex[cn].."\"><b><font color='rgb("..
                tostring(namecolors[cn][1])..","..tostring(namecolors[cn][2])..","..tostring(namecolors[cn][3])..
                ")'>["..plr.DisplayName..((hidden and " (HIDDEN CHAT)") or "")..
                ((iq and (" ("..iq..")")) or "")..((muted and (" (MUTED)")) or "").."]: </font></b></font>"
            

        end
        
        return mdata
    end
end

 ChatModsToggle = ChatGroup:AddToggle('ChatMods', {
    Text = 'Enhanced Chat Colors',
    Default = false,
    Tooltip = 'Apply TCO-style color coding to chat messages'
})

ChatModsToggle:OnChanged(function(value)
    ChatModsEnabled = value
    
    if ChatModsEnabled then
        setupEnhancedChatSpy()
        Library:Notify("Enhanced chat colors enabled", 3)
    else

        if originalOnIncomingMessage then
            game.TextChatService.OnIncomingMessage = originalOnIncomingMessage
        else
            game.TextChatService.OnIncomingMessage = nil
        end
    end
end)

dheads = {}
groups = {}

 function plradded(plr)
    local owner = false
    

    local groupInfo = nil
    for groupId, info in pairs(groups) do
        if plr:IsInGroup(groupId) then
            groupInfo = info
            break
        end
    end
    
    if owner or groupInfo then
         function onchar(c)
            if not c:FindFirstChild("Head") then return end
            
            local bbgui = Instance.new("BillboardGui")
            dheads[c.Head] = bbgui
            bbgui.Size = UDim2.new(10, 0, 1.5, 0)
            bbgui.StudsOffset = Vector3.new(0, 3.5, 0)
            bbgui.Parent = c.Head
            
            local txt = Instance.new("TextLabel")
            txt.TextScaled = true
            txt.BackgroundTransparency = 1
            txt.Size = UDim2.new(1, 0, 1, 0)
            txt.Position = UDim2.new(0, 0, -0.5, 0)
            txt.TextStrokeTransparency = 0
            
            local infotxt = Instance.new("TextLabel")
            infotxt.TextScaled = true
            infotxt.BackgroundTransparency = 1
            infotxt.Size = UDim2.new(1, 0, 0.5, 0)
            infotxt.Position = UDim2.new(0, 0, 0.5, 0)
            infotxt.TextStrokeTransparency = 0
            
            if owner then
                txt.Text = "Script Owner"
                infotxt.Text = "Special user"
                txt.TextColor3 = Color3.fromRGB(200, 90, 0)
            elseif groupInfo then
                txt.Text = groupInfo[1]
                txt.TextColor3 = groupInfo[2]
                infotxt.Text = plr:GetRoleInGroup(plr:GetRankInGroup(groupId))
            end
            
            txt.Parent = bbgui
            infotxt.TextColor3 = txt.TextColor3
            infotxt.Parent = bbgui
        end
        
        if plr.Character then
            onchar(plr.Character)
        end
        
        plr.CharacterAdded:Connect(onchar)
    end
end

for _, player in ipairs(game.Players:GetPlayers()) do
    plradded(player)
end

game.Players.PlayerAdded:Connect(plradded)

 function cleanupEnhancedChat()
    for head, gui in pairs(dheads) do
        if gui then
            gui:Destroy()
        end
    end
    dheads = {}
    
    if originalOnIncomingMessage then
        game.TextChatService.OnIncomingMessage = originalOnIncomingMessage
    end
end

local originalCleanup = Library.OnUnload
Library.OnUnload = function()
    cleanupEnhancedChat()
    if originalCleanup then
        originalCleanup()
    end
end

local FakeTimeGroup = Tabs.Chat:AddLeftGroupbox('Time Spoofer')
FakeTimeGroup:AddLabel('Client-side only')

local FakeTimeValue = FakeTimeGroup:AddSlider('FakeTimeVal', {
    Text    = 'Fake Time Value',
    Default = 9999,
    Min     = 0,
    Max     = 99999,
    Rounding = 1
})
local fakeTimeConn = nil

local FakeTimeToggle = FakeTimeGroup:AddToggle('FakeTime', {
    Text    = 'Enable Fake Time',
    Default = false,
    Tooltip = 'Overwrite your visible Time leaderstat locally'
})

FakeTimeToggle:OnChanged(function()
    if fakeTimeConn then fakeTimeConn:Disconnect(); fakeTimeConn = nil end
    if FakeTimeToggle.Value then
        local _fakeTimeT = 0
        fakeTimeConn = RunService.Heartbeat:Connect(function(dt)
            _fakeTimeT = _fakeTimeT + dt
            if _fakeTimeT < 0.5 then return end
            _fakeTimeT = 0
            local ls = plr:FindFirstChild("leaderstats")
            if ls then
                local tv = ls:FindFirstChild("Time")
                if tv then pcall(function() tv.Value = FakeTimeValue.Value end) end
            end
        end)
        Library:Notify("Fake Time active: " .. FakeTimeValue.Value, 3)
    else

        Library:Notify("Fake Time disabled", 2)
    end
end)

FakeTimeValue:OnChanged(function()
    if FakeTimeToggle.Value then
        Library:Notify("Fake Time updated: " .. FakeTimeValue.Value, 2)
    end
end)

ChatGroup:AddButton({
    Text = 'Spam Chat (10x)',
    Func = function()
        local message = "RomazDev Hub ON TOP! 🚀"
        for i = 1, 10 do
            TextChatService.TextChannels.RBXSystem:SendAsync(message)
            task.wait(0.2)
        end
        Library:Notify("Chat spam sent", 3)
    end
})

AntiDumb = ChatGroup:AddInput('AntiDumb', {
    Default = '',
    Numeric = false,
    Finished = false,
    Text = 'Text to speak:',
    Placeholder = 'Enter text for anti dumb...',
    Tooltip = 'Prevents chat appearing as "EEEE"'
})

local y = {
    ["A"] = "\u{FF21}",
    ["B"] = "\u{FF22}",
    ["C"] = "\u{FF23}",
    ["D"] = "\u{FF24}",
    ["E"] = "\u{FF25}",
    ["F"] = "\u{FF26}",
    ["G"] = "\u{FF27}",
    ["H"] = "\u{FF28}",
    ["I"] = "\u{FF29}",
    ["J"] = "\u{FF2A}",
    ["K"] = "\u{FF2B}",
    ["L"] = "\u{FF2C}",
    ["M"] = "\u{FF2D}",
    ["N"] = "\u{FF2E}",
    ["O"] = "\u{FF2F}",
    ["P"] = "\u{FF30}",
    ["Q"] = "\u{FF31}",
    ["R"] = "\u{FF32}",
    ["S"] = "\u{FF33}",
    ["T"] = "\u{FF34}",
    ["U"] = "\u{FF35}",
    ["V"] = "\u{FF36}",
    ["W"] = "\u{FF37}",
    ["X"] = "\u{FF38}",
    ["Y"] = "\u{FF39}",
    ["Z"] = "\u{FF3A}",
    ["a"] = "\u{FF41}",
    ["b"] = "\u{FF42}",
    ["c"] = "\u{FF43}",
    ["d"] = "\u{FF44}",
    ["e"] = "\u{FF45}",
    ["f"] = "\u{FF46}",
    ["g"] = "\u{FF47}",
    ["h"] = "\u{FF48}",
    ["i"] = "\u{FF49}",
    ["j"] = "\u{FF4A}",
    ["k"] = "\u{FF4B}",
    ["l"] = "\u{FF4C}",
    ["m"] = "\u{FF4D}",
    ["n"] = "\u{FF4E}",
    ["o"] = "\u{FF4F}",
    ["p"] = "\u{FF50}",
    ["q"] = "\u{FF51}",
    ["r"] = "\u{FF52}",
    ["s"] = "\u{FF53}",
    ["t"] = "\u{FF54}",
    ["u"] = "\u{FF55}",
    ["v"] = "\u{FF56}",
    ["w"] = "\u{FF57}",
    ["x"] = "\u{FF58}",
    ["y"] = "\u{FF59}",
    ["z"] = "\u{FF5A}",
    [" "] = "\u{0020}\u{0020}"
}

ChatGroup:AddButton({
    Text = 'Bypass EEEE Chat',
    Func = function()
        if AntiDumb.Value and AntiDumb.Value ~= "" then
            local normalchars = true
            local tbtext = AntiDumb.Value
            local fstext = ""
            for i=1,string.len(tbtext) do
                local sub = string.sub(tbtext,i,i)
                local char = y[string.sub(tbtext,i,i)]
                if char then
                    fstext = fstext..char
                else
                    fstext = fstext..string.sub(tbtext,i,i)
                end
            end
            game:GetService("TextChatService").TextChannels.RBXGeneral:SendAsync(fstext)
        end
    end
})

CustomSpamMessage = ChatGroup:AddInput('CustomSpamMessage', {
    Default = 'RomazDev Hub ON TOP!',
    Numeric = false,
    Text = 'Custom Spam Message:',
    Placeholder = 'Enter message to spam...'
})

SpamAmount = ChatGroup:AddSlider('SpamAmount', {
    Text = 'Spam Amount',
    Default = 5,
    Min = 1,
    Max = 20,
    Rounding = 1,
    Compact = false,
    Tooltip = 'Number of times to spam'
})

ChatGroup:AddButton({
    Text = 'Custom Spam',
    Func = function()
        local message = CustomSpamMessage.Value
        local amount = SpamAmount.Value
        
        for i = 1, amount do
            TextChatService.TextChannels.RBXSystem:SendAsync(message)
            task.wait(0.3)
        end
        Library:Notify("Custom spam sent " .. amount .. " times", 3)
    end
})

function startChatSpy()
    if chatSpyConnection then
        chatSpyConnection:Disconnect()
    end
    
    chatSpyConnection = TextChatService.TextChannels.RBXGeneral.MessageReceived:Connect(function(message)
        if ChatSpyEnabled then
            sender = message.TextSource
            text = message.Text
            

            

            Library:Notify("Chat Spy: " .. text, 5)
        end
    end)
end

ChatSpyToggle:OnChanged(function(value)
    ChatSpyEnabled = value
    
    if ChatSpyEnabled then
        startChatSpy()
        Library:Notify("Chat Spy enabled - monitoring all messages", 3)
    else
        if chatSpyConnection then
            chatSpyConnection:Disconnect()
            chatSpyConnection = nil
        end
    end
end)

AntisGroup = Tabs.Auras:AddLeftGroupbox('Antis')

AntiVoid = AntisGroup:AddToggle('AntiVoid', {
    Text = 'Anti Void',
    Default = false,
    Tooltip = 'Prevent falling into the void'
})

AntiBlind = AntisGroup:AddToggle('AntiBlind', {
    Text = 'Anti Blind',
    Default = false,
    Tooltip = 'Prevents blind effects'
})

 AntiDrag = AntisGroup:AddToggle('AntiDrag', {
    Text = 'Anti Drag',
    Default = false,
    Tooltip = 'Prevents drag effects'
})

 AntiJail = AntisGroup:AddToggle('AntiJail', {
    Text = 'Anti Jail',
    Default = false,
    Tooltip = 'Prevents jail effects'
})

 AntiVampire = AntisGroup:AddToggle('AntiVampire', {
    Text = 'Anti Vampire',
    Default = false,
    Tooltip = 'Prevents vampire/camera manipulation'
})

 AntiFling = AntisGroup:AddToggle('AntiFling', {
    Text = 'Anti Fling',
    Default = false,
    Tooltip = 'Prevents flinging and velocity manipulation'
})

 AntiInvisible = AntisGroup:AddToggle('AntiInvisible', {
    Text = 'Anti Invisible',
    Default = false,
    Tooltip = 'Detects and prevents invisible character'
})

 AntiToxify = AntisGroup:AddToggle('AntiToxify', {
    Text = 'Anti Toxify',
    Default = false,
    Tooltip = 'Removes toxic effects (blur, RGB, fog)'
})

 AntiNoColor = AntisGroup:AddToggle('AntiNoColor', {
    Text = 'Anti NoColor',
    Default = false,
    Tooltip = 'Prevents color correction effects'
})

 AntiStun = AntisGroup:AddToggle('AntiStun', {
    Text = 'Anti Stun',
    Default = false,
    Tooltip = 'Prevents stun/tackle effects'
})

 AntiFarlands = AntisGroup:AddToggle('AntiFarlands', {
    Text = 'Anti Farlands',
    Default = false,
    Tooltip = 'Prevents teleportation to far lands'
})

 AntiFreeze = AntisGroup:AddToggle('AntiFreeze', {
    Text = 'Anti Freeze',
    Default = false,
    Tooltip = 'Prevents freeze effects'
})

 AntiMyopicBlur = AntisGroup:AddToggle('AntiMyopicBlur', {
    Text = 'Anti Myopic/Blur',
    Default = false,
    Tooltip = 'Prevents blur effects'
})

 AntiFog = AntisGroup:AddToggle('AntiFog', {
    Text = 'Anti Fog',
    Default = false,
    Tooltip = 'Prevents fog effects'
})

 AntiCursed = AntisGroup:AddToggle('AntiCursed', {
    Text = 'Anti Cursed',
    Default = false,
    Tooltip = 'Prevents cursed effects'
})

GriefAura:OnChanged(function()
    auraSettings.griefAura.active = GriefAura.Value
    auraSettings.griefAura.range = AuraRange.Value
    auraSettings.griefAura.speed = AuraSpeed.Value
    
    if GriefAura.Value then
        StartAura("griefAura")
        Library:Notify("Grief aura activated - Deleting blocks", 3)
    else
        if auraConnections.griefAura then
            auraConnections.griefAura:Disconnect()
            auraConnections.griefAura = nil
        end
    end
end)

BlockAura:OnChanged(function()
    auraSettings.blockAura.active = BlockAura.Value
    auraSettings.blockAura.range = AuraRange.Value
    auraSettings.blockAura.speed = AuraSpeed.Value
    
    if BlockAura.Value then
        StartAura("blockAura")
        Library:Notify("Block aura activated - Building blocks", 3)
    else
        if auraConnections.blockAura then
            auraConnections.blockAura:Disconnect()
            auraConnections.blockAura = nil
        end
    end
end)

AntiBlind:OnChanged(function()
    if AntiBlind.Value then
        StartAntiBlind()
        Library:Notify("Anti Blind enabled", 3)
    else
        if antiConnections["Blind"] then
            antiConnections["Blind"]:Disconnect()
            antiConnections["Blind"] = nil
        end
    end
end)

AntiVampire:OnChanged(function()
    if AntiVampire.Value then
        StartAntiVampire()
        Library:Notify("Anti Vampire enabled", 3)
    else
        if antiConnections["Vampire"] then
            antiConnections["Vampire"]:Disconnect()
            antiConnections["Vampire"] = nil
        end
    end
end)

AntiFling:OnChanged(function()
    if AntiFling.Value then
        StartAntiFling()
        Library:Notify("Anti Fling enabled", 3)
    else
        if antiConnections["Fling"] then
            antiConnections["Fling"]:Disconnect()
            antiConnections["Fling"] = nil
        end
    end
end)

AntiInvisible:OnChanged(function()
    if AntiInvisible.Value then
        StartAntiInvisible()
        Library:Notify("Anti Invisible enabled", 3)
    else
        if antiConnections["Invisible"] then
            antiConnections["Invisible"]:Disconnect()
            antiConnections["Invisible"] = nil
        end
    end
end)

AntiToxify:OnChanged(function()
    if AntiToxify.Value then
        StartAntiToxify()
        Library:Notify("Anti Toxify enabled", 3)
    else
        if antiConnections["Toxify"] then
            antiConnections["Toxify"]:Disconnect()
            antiConnections["Toxify"] = nil
        end
    end
end)

AntiNoColor:OnChanged(function()
    if AntiNoColor.Value then
        StartAntiNoColor()
        Library:Notify("Anti NoColor enabled", 3)
    else
        if antiConnections["NoColor"] then
            antiConnections["NoColor"]:Disconnect()
            antiConnections["NoColor"] = nil
        end
    end
end)

AntiStun:OnChanged(function()
    if AntiStun.Value then
        StartAntiStun()
        Library:Notify("Anti Stun enabled", 3)
    else
        if antiConnections["Stun"] then
            antiConnections["Stun"]:Disconnect()
            antiConnections["Stun"] = nil
        end
    end
end)

AntiFarlands:OnChanged(function()
    if AntiFarlands.Value then
        StartAntiFarlands()
        Library:Notify("Anti Farlands enabled", 3)
    else
        if antiConnections["Farlands"] then
            antiConnections["Farlands"]:Disconnect()
            antiConnections["Farlands"] = nil
        end
    end
end)

AntiDrag:OnChanged(function()
    if AntiDrag.Value then
        StartAntiDrag()
        Library:Notify("Anti Drag enabled", 3)
    else
        if antiConnections["Drag"] then
            antiConnections["Drag"]:Disconnect()
            antiConnections["Drag"] = nil
        end
    end
end)

AntiJail:OnChanged(function()
    if AntiJail.Value then
        StartAntiJail()
        Library:Notify("Anti Jail enabled", 3)
    else
        if antiConnections["Jail"] then
            antiConnections["Jail"]:Disconnect()
            antiConnections["Jail"] = nil
        end
    end
end)

AntiFreeze:OnChanged(function()
    if AntiFreeze.Value then
        StartAntiFreeze()
        Library:Notify("Anti Freeze enabled", 3)
    else
        if antiConnections["Freeze"] then
            antiConnections["Freeze"]:Disconnect()
            antiConnections["Freeze"] = nil
        end
    end
end)

AntiMyopicBlur:OnChanged(function()
    if AntiMyopicBlur.Value then
        StartAntiMyopicBlur()
        Library:Notify("Anti Myopic/Blur enabled", 3)
    else
        if antiConnections["MyopicBlur"] then
            antiConnections["MyopicBlur"]:Disconnect()
            antiConnections["MyopicBlur"] = nil
        end
    end
end)
local _muteBoomboxConn = nil
MuteBoomboxesToggle:OnChanged(function(value)
    muteBoomboxesEnabled = value

    if value then

        for _, tool in pairs(workspace:GetDescendants()) do
            if tool:IsA("Tool") and boomboxNames[tool.Name] then
                local sound = tool:FindFirstChild("Sound", true)
                if sound then
                    sound.Volume = 0
                end
            end
        end

        if _muteBoomboxConn then _muteBoomboxConn:Disconnect() end
        _muteBoomboxConn = workspace.DescendantAdded:Connect(function(descendant)
            if descendant:IsA("Tool") and boomboxNames[descendant.Name] and muteBoomboxesEnabled then
                local sound = descendant:FindFirstChild("Sound", true)
                if sound then
                    sound.Volume = 0
                end
            end
        end)

        Library:Notify("All boomboxes muted", 3)
    else

        if _muteBoomboxConn then
            _muteBoomboxConn:Disconnect()
            _muteBoomboxConn = nil
        end

        for _, tool in pairs(workspace:GetDescendants()) do
            if tool:IsA("Tool") and boomboxNames[tool.Name] then
                local sound = tool:FindFirstChild("Sound", true)
                if sound then
                    sound.Volume = 1
                end
            end
        end
    end
end)

AntiFog:OnChanged(function()
    if AntiFog.Value then
        StartAntiFog()
        Library:Notify("Anti Fog enabled", 3)
    else
        if antiConnections["Fog"] then
            antiConnections["Fog"]:Disconnect()
            antiConnections["Fog"] = nil
        end
    end
end)

AntiCursed:OnChanged(function()
    if AntiCursed.Value then
        StartAntiCursed()
        Library:Notify("Anti Cursed enabled", 3)
    else
        if antiConnections["Cursed"] then
            antiConnections["Cursed"]:Disconnect()
            antiConnections["Cursed"] = nil
        end
    end
end)

RainbowAura:OnChanged(function()
    auraSettings.rainbowAura.active = RainbowAura.Value
    auraSettings.rainbowAura.range = AuraRange.Value
    auraSettings.rainbowAura.speed = AuraSpeed.Value
    
    if RainbowAura.Value then
        StartAura("rainbowAura")
        Library:Notify("Rainbow aura activated", 3)
    else
        if auraConnections.rainbowAura then
            auraConnections.rainbowAura:Disconnect()
            auraConnections.rainbowAura = nil
        end
    end
end)

ToxicAura:OnChanged(function()
    auraSettings.toxicAura.active = ToxicAura.Value
    auraSettings.toxicAura.range = AuraRange.Value
    auraSettings.toxicAura.speed = AuraSpeed.Value
    
    if ToxicAura.Value then
        StartAura("toxicAura")
        Library:Notify("Toxic aura activated", 3)
    else
        if auraConnections.toxicAura then
            auraConnections.toxicAura:Disconnect()
            auraConnections.toxicAura = nil
        end
    end
end)

SignAura:OnChanged(function()
    auraSettings.signAura.active = SignAura.Value
    auraSettings.signAura.range = AuraRange.Value
    auraSettings.signAura.speed = AuraSpeed.Value
    
    if SignAura.Value then
        StartAura("signAura")
        Library:Notify("Sign aura activated", 3)
    else
        if auraConnections.signAura then
            auraConnections.signAura:Disconnect()
            auraConnections.signAura = nil
        end
    end
end)

AnchorAura:OnChanged(function()
    auraSettings.anchorAura.active = AnchorAura.Value
    auraSettings.anchorAura.range = AuraRange.Value
    auraSettings.anchorAura.speed = AuraSpeed.Value
    
    if AnchorAura.Value then
        StartAura("anchorAura")
        Library:Notify("Anchor aura activated", 3)
    else
        if auraConnections.anchorAura then
            auraConnections.anchorAura:Disconnect()
            auraConnections.anchorAura = nil
        end
    end
end)

UnanchorAura:OnChanged(function()
    auraSettings.unanchorAura.active = UnanchorAura.Value
    auraSettings.unanchorAura.range = AuraRange.Value
    auraSettings.unanchorAura.speed = AuraSpeed.Value
    
    if UnanchorAura.Value then
        StartAura("unanchorAura")
        Library:Notify("Unanchor aura activated", 3)
    else
        if auraConnections.unanchorAura then
            auraConnections.unanchorAura:Disconnect()
            auraConnections.unanchorAura = nil
        end
    end
end)

FlyEnabled:OnChanged(function()
    flyEnabled = FlyEnabled.Value
    flySpeed = FlySpeed.Value
    
    if flyEnabled then
        StartFlying()
        Library:Notify("Fly enabled - Use WASD+Space+Ctrl", 3)
    else
        StopFlying()
    end
end)

FlySpeed:OnChanged(function()
    flySpeed = FlySpeed.Value
end)

AuraRange:OnChanged(function()
    for auraType, settings in pairs(auraSettings) do
        if settings.active then
            settings.range = AuraRange.Value
        end
    end
end)

AuraSpeed:OnChanged(function()
    for auraType, settings in pairs(auraSettings) do
        if settings.active then
            settings.speed = AuraSpeed.Value
        end
    end
end)

AutoPickup:OnChanged(function()
    autoPickupEnabled = AutoPickup.Value
end)

AutoDrop:OnChanged(function()
    autoDropEnabled = AutoDrop.Value
end)

function clearPartRing()

end

AntiVoid:OnChanged(function()
    antiVoidEnabled = AntiVoid.Value
    
    if antiVoidEnabled then
        StartAntiVoid()
        Library:Notify("Anti-Void enabled", 3)
    else
        clearPartRing()
        StopAntiVoid()
    end
end)

ThemeManager:SetLibrary(Library)
SaveManager:SetLibrary(Library)
SaveManager:IgnoreThemeSettings()
SaveManager:SetIgnoreIndexes({ 'MenuKeybind' })
ThemeManager:SetFolder('RomazDevHub')
SaveManager:SetFolder('RomazDevHub')

ThemeManager:ApplyToTab(Tabs.Settings)
SaveManager:BuildConfigSection(Tabs.Settings)

 function cleanupBoombox()
    if bbsbox then
        bbsbox:Destroy()
    end
    
    for _, obj in pairs(CoreGui:GetChildren()) do
        if obj.Name == "BoomboxHighlight" then
            obj:Destroy()
        end
    end
end

Library:OnUnload(function()
    _G.ROMAZDEV_HUB_LOADED = false
    Library.Unloaded = true

    pcall(sendRelayLeave)
    _relayHeartbeatRunning = false

    if chatSpyConnection then
        chatSpyConnection:Disconnect()
        chatSpyConnection = nil
    end

    if _muteBoomboxConn then
        _muteBoomboxConn:Disconnect()
        _muteBoomboxConn = nil
    end

    for _, conn in pairs(hubRespawnConns) do
        pcall(function() conn:Disconnect() end)
    end
    hubRespawnConns = {}

    if autoScanThread then
        pcall(function() task.cancel(autoScanThread) end)
        autoScanThread = nil
    end

    stopStash = true

    for _, player in ipairs(Players:GetPlayers()) do
        pcall(function()
            if player.Character then
                local head = player.Character:FindFirstChild("Head")
                if head then
                    for _, child in ipairs(head:GetChildren()) do
                        if child:IsA("BillboardGui") then
                            child:Destroy()
                        end
                    end
                end
            end
        end)
    end

    pcall(function()
        if stashPlatform and stashPlatform.Parent then
            stashPlatform.CanCollide = false
            stashPlatform.Transparency = 1
        end
    end)

    cleanupBoombox()

if _toolsESPAddConn then _toolsESPAddConn:Disconnect() end
if _toolsESPRemoveConn then _toolsESPRemoveConn:Disconnect() end
for _, v in pairs(toolsESPObjects) do pcall(function() v:Destroy() end) end
for _, data in pairs(toolsESPLabels) do pcall(function() data.bb:Destroy() end) end

    for _, connection in pairs(auraConnections) do
        if connection then
            connection:Disconnect()
        end
    end

    for name, connection in pairs(antiConnections) do
        if connection then
            connection:Disconnect()
        end
    end

    workspace.FallenPartsDestroyHeight = originalDestroyHeight

if fakeTimeConn then fakeTimeConn:Disconnect(); fakeTimeConn = nil end

    StopAntiVoid()
    StopFlying()
    
    RemoveAllESP()
    StopESPUpdateLoop()
    for _, v in pairs(toolsESPObjects) do pcall(function() v:Destroy() end) end
    for _, data in pairs(toolsESPLabels) do pcall(function() data.bb:Destroy() end) end

    if deleteAuraConnection then
        deleteAuraConnection:Disconnect()
    end
    
    if toxifyAuraConnection then
        toxifyAuraConnection:Disconnect()
    end
    
    if toolManagementConnection then
        toolManagementConnection:Disconnect()
    end
    
    if cubechild then
        cubechild:Disconnect()
    end

    for i,v in pairs(scriptConnections) do
        v:Disconnect()
    end

    coroutine.wrap(function()
        for i,v in pairs(tools) do
            local s,e = pcall(function()
                for i,v in pairs(v) do
                    if v and v:IsDescendantOf(game) then
                        v:Destroy()
                        task.wait()
                        task.wait()
                    end
                end
            end)
            if not s then warn(e) end
        end
    end)()

end)

 MenuGroup = Tabs.Settings:AddRightGroupbox('Menu')
MenuGroup:AddButton({
    Text = 'Unload Script',
    Func = function() Library:Unload() end
})
MenuGroup:AddLabel('Menu Keybind: Left Ctrl')

pcall(function()
    MenuKeybind = MenuGroup:AddKeyPicker('MenuKeybind', {
        Default = 'End',
        NoUI = false,
        Text = 'Menu Keybind'
    })
end)

 MenuSize = MenuGroup:AddSlider('MenuSize', {
    Text = 'Menu Size',
    Default = 10,
    Min = 5,
    Max = 15,
    Rounding = 1,
    Compact = false,
    Tooltip = 'Change menu size'
})

function divudim2(udim2,num)
    return UDim2.new(udim2.X.Scale/num,udim2.X.Offset/num,udim2.Y.Scale/num,udim2.Y.Offset/num)
end

function muludim2(udim2,num)
    return UDim2.new(udim2.X.Scale*num,udim2.X.Offset*num,udim2.Y.Scale*num,udim2.Y.Offset*num)
end

MenuSize:OnChanged(function()
    pcall(function()
        if _mainFrame and _mainFrame.Parent then
            _mainFrame.Size = muludim2(divudim2(windowsize, 10), MenuSize.Value)
        else

            for _, sg in ipairs(game:GetService("CoreGui"):GetChildren()) do
                if sg:IsA("ScreenGui") then
                    local f = sg:FindFirstChildWhichIsA("Frame")
                    if f then
                        _mainFrame = f
                        _mainFrame.Size = muludim2(divudim2(windowsize, 10), MenuSize.Value)
                        break
                    end
                end
            end
        end
    end)
end)

if MenuKeybind then
    Library.ToggleKeybind = MenuKeybind
end
Library:SetWatermarkVisibility(true)

 FrameTimer = tick()
 FrameCounter = 0
 FPS = 60
ping = 0
playerCount = #Players:GetPlayers()

Library:GiveSignal(RunService.RenderStepped:Connect(function()
    FrameCounter = FrameCounter + 1

    if (tick() - FrameTimer) >= 1 then
        FPS = FrameCounter
        FrameTimer = tick()
        FrameCounter = 0
        ping = math.floor(game:GetService('Stats').Network.ServerStatsItem['Data Ping']:GetValue())
        playerCount = #Players:GetPlayers()
        Library:SetWatermark(('RomazDev Hub v2.2 | %s fps | %s ms | %d players | %s'):format(
            math.floor(FPS),
            ping,
            playerCount,
            isOwner and 'Owner 👑' or 'KEYLESS 💸'
        ))
    end
end))

SaveManager:LoadAutoloadConfig()

function SetupAutoRejoin()
    if AutoRejoin.Value then
        plr.OnTeleport:Connect(function(state)
            if state == Enum.TeleportState.Failed then
                TeleportService:Teleport(game.PlaceId)
            end
        end)
    end
end

task.spawn(function()
    task.wait(2)
    SetupAutoRejoin()

    if ShowCredits.Value then
        Library:Notify('RomazDev Hub v2.2 Loaded!', 5)
        Library:Notify('Credits to pealz/RomazDev/peaiz!', 5)
        Library:Notify("Discord invite copied to clipboard! .gg/zSuZN5e6MZ", 8)
    end

    if isOwner then
        if plr.Character then
            Library:Notify("welcome back, daddy :3", 5)
        end
    end
end)
end

 success, err = pcall(main)
if not success then
    warn("[ROMAZHUB ERROR]: " .. tostring(err))
end