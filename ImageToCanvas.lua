-- ============================================================
-- THINKINGFACE v4.0 — Image to Canvas Build System
-- Converts images into colored block art in-game.
-- Supports PNG, BMP natively. JPEG/WebP/GIF via proxy.
-- Features: Build modes, detail modes, shape modes, 3D depth,
--   HD mode, co-op building, visual effects, progress resume.
-- Requires: Build tool + PaintBucket tool in Character or Backpack.
-- ============================================================

-- Cleanup previous instance
if getgenv().ThinkingFace then
    pcall(function() getgenv().ThinkingFace:Unload() end)
    task.wait(0.3)
end
getgenv().stopimage = false

-- SERVICES
local Players  = game:GetService("Players")
local localplr = Players.LocalPlayer

-- LOAD UI LIBRARY (PealLib)
local Library = loadstring(game:HttpGet(
    "https://raw.githubusercontent.com/pealz1/PealLib/main/Library.lua"
))()

-- LOAD PNG UTILITIES
local pnglib       = loadstring(game:HttpGet(
    "https://raw.githubusercontent.com/pealz1/TCO/refs/heads/main/PNGLib"
))()
local Deflate      = pnglib.Deflate
local BinaryReader = pnglib.BinaryReader
local Unfilter     = pnglib.Unfilter

-- STATE
local isBuilding = false
local parsedFile = nil
local depthMapCache = nil

-- ============================================================
-- AUTO PAINTBUCKET GEAR
-- ============================================================
pcall(function()
    game:GetService("ReplicatedStorage").DefaultChatSystemChatEvents.SayMessageRequest:FireServer(";gear me 18474459", "All")
end)

-- ============================================================
-- PNG PARSER (battle-tested inline parser from v2.0)
-- ============================================================
local function getBytesPerPixel(colorType)
    local map = { [0] = 1, [2] = 3, [3] = 1, [4] = 2, [6] = 4 }
    return map[colorType] or 0
end

local function clampInt(value, mn, mx)
    local num = math.floor((tonumber(value) or 0) + 0.5)
    return math.clamp(num, mn, mx)
end

local function indexBitmap(file, x, y)
    x = clampInt(x, 1, file.Width)
    y = clampInt(y, 1, file.Height)
    local bpp = file.BytesPerPixel
    local i0  = ((x - 1) * bpp) + 1
    return file.Bitmap[y], i0
end

local PNG = {}
PNG.__index = PNG

function PNG:GetPixel(x, y)
    local row, i0 = indexBitmap(self, x, y)
    local ct = self.ColorType
    local color, alpha

    if ct == 0 then
        local g = string.byte(row, i0)
        color = Color3.fromHSV(0, 0, g / 255)
        alpha = 255
    elseif ct == 2 then
        color = Color3.fromRGB(string.byte(row, i0), string.byte(row, i0 + 1), string.byte(row, i0 + 2))
        alpha = 255
    elseif ct == 3 then
        local idx = string.byte(row, i0) + 1
        color = self.Palette   and self.Palette[idx]   or Color3.new()
        alpha = self.AlphaData and self.AlphaData[idx] or 255
    elseif ct == 4 then
        local g = string.byte(row, i0)
        color = Color3.fromHSV(0, 0, g / 255)
        alpha = string.byte(row, i0 + 1)
    elseif ct == 6 then
        color = Color3.fromRGB(string.byte(row, i0), string.byte(row, i0 + 1), string.byte(row, i0 + 2))
        alpha = string.byte(row, i0 + 3)
    end

    return color or Color3.new(), alpha or 255
end

function PNG.new(buffer)
    local reader = BinaryReader.new(buffer)
    local file   = { Chunks = {}, Reading = true, ZlibStream = "" }

    -- Validate PNG header
    local header = ""
    for i = 1, 8 do header = header .. string.char(reader:ReadByte()) end
    if header ~= "\137PNG\r\n\26\n" then
        error("Not a valid PNG file.", 2)
    end

    while file.Reading do
        local length    = reader:ReadUInt32()
        local chunkType = ""
        for i = 1, 4 do chunkType = chunkType .. string.char(reader:ReadByte()) end

        local data = nil
        if length > 0 then
            data = reader:ReadBytes(length)
        end
        -- CRC is always present, even for zero-length chunks
        local crc = reader:ReadUInt32()

        if chunkType == "IHDR" then
            local cr         = BinaryReader.new(data)
            file.Width       = cr:ReadUInt32()
            file.Height      = cr:ReadUInt32()
            file.BitDepth    = cr:ReadByte()
            file.ColorType   = cr:ReadByte()
            file.Compression = cr:ReadByte()
            file.Filter      = cr:ReadByte()
            file.Interlace   = cr:ReadByte()
            if file.Interlace ~= 0 then
                error("Interlaced PNGs are not supported. Re-save without interlacing.", 2)
            end
        elseif chunkType == "PLTE" and data then
            local palette = {}
            for i = 1, #data, 3 do
                table.insert(palette, Color3.fromRGB(
                    string.byte(data, i),
                    string.byte(data, i + 1),
                    string.byte(data, i + 2)
                ))
            end
            file.Palette = palette
        elseif chunkType == "tRNS" and data then
            local alphaData = {}
            for i = 1, #data do alphaData[i] = string.byte(data, i) end
            file.AlphaData = alphaData
        elseif chunkType == "IDAT" and data then
            file.ZlibStream = file.ZlibStream .. data
        elseif chunkType == "IEND" then
            file.Reading = false
        end

        table.insert(file.Chunks, { Length = length, Type = chunkType, Data = data, CRC = crc })
    end

    -- Decompress IDAT stream
    local ok, response = pcall(function()
        local result, index = {}, 0
        Deflate:InflateZlib({
            Input  = BinaryReader.new(file.ZlibStream),
            Output = function(byte)
                index = index + 1
                result[index] = string.char(byte)
            end
        })
        return table.concat(result)
    end)
    if not ok then
        error("Failed to decompress PNG data: " .. tostring(response), 2)
    end

    file.ZlibStream = nil

    -- Calculate bytes per pixel
    local bpp
    if file.ColorType == 3 then
        bpp = 1
    else
        bpp = math.max(1, getBytesPerPixel(file.ColorType) * (file.BitDepth / 8))
    end
    file.BytesPerPixel = bpp

    -- Reconstruct scanlines with filter decompression
    local buf     = BinaryReader.new(response)
    local bitmap  = {}
    file.Bitmap   = bitmap
    local rowLen  = file.Width * bpp
    local prevRow = nil

    for row = 1, file.Height do
        local filterType = buf:ReadByte()
        local rawBytes   = buf:ReadBytes(rowLen)
        local recon

        if filterType == 0 then     recon = Unfilter.None(rawBytes)
        elseif filterType == 1 then recon = Unfilter.Sub(rawBytes, prevRow, bpp)
        elseif filterType == 2 then recon = Unfilter.Up(rawBytes, prevRow)
        elseif filterType == 3 then recon = Unfilter.Average(rawBytes, prevRow, bpp)
        elseif filterType == 4 then recon = Unfilter.Paeth(rawBytes, prevRow, bpp)
        else                        recon = rawBytes
        end

        bitmap[row] = recon
        prevRow     = recon
    end

    return setmetatable(file, PNG)
end

-- ============================================================
-- BUILD TOOL UTILITIES
-- ============================================================
local function getplrpos()
    return localplr.Character.HumanoidRootPart.Position
end

local function equiptool(toolname)
    local char = localplr.Character
    if char:FindFirstChild(toolname) then
        return char:FindFirstChild(toolname)
    end
    local tool = localplr.Backpack:FindFirstChild(toolname)
    if tool then tool.Parent = char end
    return tool
end

-- Single persistent ChildAdded connection
local newestchild = nil
local childConn = workspace.Bricks[localplr.Name].ChildAdded:Connect(function(c)
    newestchild = c
end)
Library:GiveSignal(childConn)

-- Direction mapping for configurable build direction
local directionMap = {
    Right = Enum.NormalId.Right,
    Left  = Enum.NormalId.Left,
    Front = Enum.NormalId.Front,
    Back  = Enum.NormalId.Back,
    Up    = Enum.NormalId.Top,
    Down  = Enum.NormalId.Bottom,
}

local function buildonblock(block, normalid, extraDelay)
    local et = equiptool("Build")
    newestchild = nil
    repeat
        if getgenv().stopimage then return nil end
        if not et then
            task.wait(1)
            et = equiptool("Build")
        else
            et.Script.Event:FireServer(block, normalid, getplrpos(), "normal")
            task.wait()
        end
    until newestchild ~= nil
    if extraDelay and extraDelay > 0 then
        task.wait(extraDelay / 1000)
    end
    return newestchild
end

-- ============================================================
-- HD MODE: RESIZE BLOCK FUNCTION
-- ============================================================
local function resizeBlock(block, scale)
    if not block then return end
    local resizeTool = equiptool("Resize")
    if resizeTool then
        local targetSize = Vector3.new(
            block.Size.X * scale,
            block.Size.Y * scale,
            block.Size.Z * scale
        )
        local ok = pcall(function()
            resizeTool.Script.Event:FireServer(block, targetSize)
        end)
        if not ok then
            -- Fallback: try setting size directly
            pcall(function()
                block.Size = targetSize
            end)
        end
    else
        -- Fallback: try setting size directly
        local targetSize = Vector3.new(
            block.Size.X * scale,
            block.Size.Y * scale,
            block.Size.Z * scale
        )
        pcall(function()
            block.Size = targetSize
        end)
        Library:Notify("Resize tool not found! HD Mode needs the Resize tool in your inventory.", 5)
    end
end

-- ============================================================
-- PAINTBUCKET UTILITIES
-- ============================================================
local function getPaintBucket()
    local char = localplr.Character
    if not char then return nil end
    local pb = char:FindFirstChild("PaintBucket")
            or localplr.Backpack:FindFirstChild("PaintBucket")
    if not pb then return nil end
    if pb.Parent ~= char then
        pb.Parent = char
        task.wait(0.2)
    end
    return pb
end

local function paintBlock(paintevent, part, color)
    pcall(function()
        paintevent:InvokeServer("PaintPart", { Part = part, Color = color })
    end)
end

-- ============================================================
-- SAMPLING FUNCTIONS
-- ============================================================
local function sampleNearest(file, cx, cy, ps)
    local centerX = math.clamp(math.floor((cx - 1) * ps + ps / 2) + 1, 1, file.Width)
    local centerY = math.clamp(math.floor((cy - 1) * ps + ps / 2) + 1, 1, file.Height)
    return file:GetPixel(centerX, centerY)
end

local function sampleAverage(file, cx, cy, ps)
    local x1 = math.clamp((cx - 1) * ps + 1, 1, file.Width)
    local y1 = math.clamp((cy - 1) * ps + 1, 1, file.Height)
    local x2 = math.clamp(cx * ps, 1, file.Width)
    local y2 = math.clamp(cy * ps, 1, file.Height)
    return pnglib.Resample.Average(file, x1, y1, x2, y2)
end

local function sampleBilinear(file, cx, cy, ps)
    local fx = math.clamp((cx - 1) * ps + ps / 2 + 1, 1, file.Width)
    local fy = math.clamp((cy - 1) * ps + ps / 2 + 1, 1, file.Height)
    return pnglib.Resample.Bilinear(file, fx, fy)
end

-- ============================================================
-- SHAPE MASK FUNCTIONS
-- Returns true if the block should be SKIPPED
-- ============================================================
local function maskRectangle(x, y, w, h)
    return false
end

local function maskCircle(x, y, w, h)
    local cx = (w + 1) / 2
    local cy = (h + 1) / 2
    local r = math.min(w, h) / 2
    local dx = x - cx
    local dy = y - cy
    return (dx * dx + dy * dy) > (r * r)
end

local function maskDome(x, y, w, h)
    local cx = (w + 1) / 2
    local cy = (h + 1) / 2
    local r = math.min(w, h) / 2
    local dx = x - cx
    local dy = y - cy
    return (dx * dx + dy * dy) > (r * r)
end

local function maskCylinder(x, y, w, h)
    local cx = (w + 1) / 2
    local cy = (h + 1) / 2
    local r = math.min(w, h) / 2
    local dx = x - cx
    local dy = y - cy
    return (dx * dx + dy * dy) > (r * r)
end

local function maskSphere(x, y, w, h)
    local cx = (w + 1) / 2
    local cy = (h + 1) / 2
    local r = math.min(w, h) / 2
    local dx = x - cx
    local dy = y - cy
    return (dx * dx + dy * dy) > (r * r)
end

local function maskWall(x, y, w, h)
    return false
end

local shapeMaskFuncs = {
    Rectangle = maskRectangle,
    Circle    = maskCircle,
    Dome      = maskDome,
    Cylinder  = maskCylinder,
    Sphere    = maskSphere,
    Wall      = maskWall,
}

-- ============================================================
-- SHAPE HEIGHT FUNCTIONS
-- Returns the height (depth) at a given (x, y) for shapes
-- ============================================================
local function shapeHeightDome(x, y, w, h, maxDepth)
    local cx = (w + 1) / 2
    local cy = (h + 1) / 2
    local r = math.min(w, h) / 2
    local dx = x - cx
    local dy = y - cy
    local distSq = dx * dx + dy * dy
    if distSq > r * r then return 0 end
    return math.max(1, math.floor(math.sqrt(r * r - distSq) * (maxDepth / r)))
end

local function shapeHeightSphere(x, y, w, h, maxDepth)
    -- Sphere builds both top and bottom hemispheres
    -- Returns total column height (both halves)
    local cx = (w + 1) / 2
    local cy = (h + 1) / 2
    local r = math.min(w, h) / 2
    local dx = x - cx
    local dy = y - cy
    local distSq = dx * dx + dy * dy
    if distSq > r * r then return 0 end
    local hemi = math.max(1, math.floor(math.sqrt(r * r - distSq) * (maxDepth / r)))
    return hemi * 2
end

-- ============================================================
-- DEPTH FUNCTIONS
-- ============================================================
local function getBrightness(color)
    return pnglib.GetBrightness(color)
end

-- ============================================================
-- CO-OP: SECTION CHECK FUNCTION
-- ============================================================
local function isMyBlock(cx, cy, canvasW, canvasH, section, customRange)
    if section == "Full Canvas" then
        return true
    elseif section == "Left Half" then
        return cx <= math.ceil(canvasW / 2)
    elseif section == "Right Half" then
        return cx > math.ceil(canvasW / 2)
    elseif section == "Top Half" then
        return cy > math.ceil(canvasH / 2)
    elseif section == "Bottom Half" then
        return cy <= math.ceil(canvasH / 2)
    elseif section == "Odd Columns" then
        return cx % 2 == 1
    elseif section == "Even Columns" then
        return cx % 2 == 0
    elseif section == "Custom Range" then
        return cx >= customRange.startCol and cx <= customRange.endCol
           and cy >= customRange.startRow and cy <= customRange.endRow
    end
    return true
end

-- ============================================================
-- EFFECTS: COLOR PIPELINE FUNCTIONS
-- ============================================================

-- Apply mirror coordinate remapping
local function applyMirror(cx, cy, canvasW, canvasH, mirrorMode)
    if mirrorMode == "None" then
        return cx, cy
    elseif mirrorMode == "Horizontal" then
        return canvasW - cx + 1, cy
    elseif mirrorMode == "Vertical" then
        return cx, canvasH - cy + 1
    elseif mirrorMode == "Both" then
        return canvasW - cx + 1, canvasH - cy + 1
    end
    return cx, cy
end

-- Apply color effect
local function applyColorEffect(color, effectName, posterizeLevels)
    if effectName == "None" then
        return color
    end

    local r, g, b = color.R, color.G, color.B

    if effectName == "Grayscale" then
        local gray = 0.299 * r + 0.587 * g + 0.114 * b
        return Color3.new(gray, gray, gray)

    elseif effectName == "Sepia" then
        local sr = math.min(1, 0.393 * r + 0.769 * g + 0.189 * b)
        local sg = math.min(1, 0.349 * r + 0.686 * g + 0.168 * b)
        local sb = math.min(1, 0.272 * r + 0.534 * g + 0.131 * b)
        return Color3.new(sr, sg, sb)

    elseif effectName == "Invert" then
        return Color3.new(1 - r, 1 - g, 1 - b)

    elseif effectName == "Posterize" then
        local levels = posterizeLevels or 4
        local function posterizeChannel(c)
            return math.floor(c * (levels - 1) + 0.5) / (levels - 1)
        end
        return Color3.new(posterizeChannel(r), posterizeChannel(g), posterizeChannel(b))

    elseif effectName == "High Contrast" then
        local function contrastChannel(c)
            return math.clamp((c - 0.5) * 2 + 0.5, 0, 1)
        end
        return Color3.new(contrastChannel(r), contrastChannel(g), contrastChannel(b))

    elseif effectName == "Neon Glow" then
        -- Boost saturation and brightness
        local h, s, v = Color3.toHSV(color)
        s = math.min(1, s * 1.5)
        v = math.min(1, v * 1.3)
        return Color3.fromHSV(h, s, v)
    end

    return color
end

-- Gradient map definitions
local gradientMaps = {
    Sunset = {
        { 0.0, Color3.fromRGB(25, 10, 40) },
        { 0.25, Color3.fromRGB(120, 30, 80) },
        { 0.5, Color3.fromRGB(220, 80, 40) },
        { 0.75, Color3.fromRGB(255, 180, 50) },
        { 1.0, Color3.fromRGB(255, 240, 150) },
    },
    Ocean = {
        { 0.0, Color3.fromRGB(0, 10, 30) },
        { 0.25, Color3.fromRGB(0, 40, 100) },
        { 0.5, Color3.fromRGB(0, 100, 180) },
        { 0.75, Color3.fromRGB(50, 180, 220) },
        { 1.0, Color3.fromRGB(180, 240, 255) },
    },
    Forest = {
        { 0.0, Color3.fromRGB(10, 20, 5) },
        { 0.25, Color3.fromRGB(20, 60, 15) },
        { 0.5, Color3.fromRGB(40, 120, 30) },
        { 0.75, Color3.fromRGB(100, 180, 60) },
        { 1.0, Color3.fromRGB(200, 240, 150) },
    },
    Infrared = {
        { 0.0, Color3.fromRGB(0, 0, 30) },
        { 0.25, Color3.fromRGB(80, 0, 120) },
        { 0.5, Color3.fromRGB(200, 0, 80) },
        { 0.75, Color3.fromRGB(255, 100, 0) },
        { 1.0, Color3.fromRGB(255, 255, 50) },
    },
    Rainbow = {
        { 0.0, Color3.fromRGB(255, 0, 0) },
        { 0.17, Color3.fromRGB(255, 165, 0) },
        { 0.33, Color3.fromRGB(255, 255, 0) },
        { 0.5, Color3.fromRGB(0, 255, 0) },
        { 0.67, Color3.fromRGB(0, 0, 255) },
        { 0.83, Color3.fromRGB(75, 0, 130) },
        { 1.0, Color3.fromRGB(148, 0, 211) },
    },
    Fire = {
        { 0.0, Color3.fromRGB(10, 0, 0) },
        { 0.25, Color3.fromRGB(150, 20, 0) },
        { 0.5, Color3.fromRGB(255, 80, 0) },
        { 0.75, Color3.fromRGB(255, 200, 50) },
        { 1.0, Color3.fromRGB(255, 255, 200) },
    },
    Ice = {
        { 0.0, Color3.fromRGB(10, 10, 30) },
        { 0.25, Color3.fromRGB(40, 60, 130) },
        { 0.5, Color3.fromRGB(100, 150, 220) },
        { 0.75, Color3.fromRGB(180, 220, 255) },
        { 1.0, Color3.fromRGB(240, 250, 255) },
    },
}

local function applyGradientMap(color, mapName)
    if mapName == "None" then return color end
    local gradient = gradientMaps[mapName]
    if not gradient then return color end

    local brightness = 0.299 * color.R + 0.587 * color.G + 0.114 * color.B

    -- Find the two stops to interpolate between
    local lower, upper = gradient[1], gradient[#gradient]
    for i = 1, #gradient - 1 do
        if brightness >= gradient[i][1] and brightness <= gradient[i + 1][1] then
            lower = gradient[i]
            upper = gradient[i + 1]
            break
        end
    end

    local range = upper[1] - lower[1]
    if range <= 0 then return lower[2] end
    local t = (brightness - lower[1]) / range
    return lower[2]:Lerp(upper[2], t)
end

-- Ordered dithering matrices
local orderedMatrix2x2 = {
    { 0, 2 },
    { 3, 1 },
}

local orderedMatrix4x4 = {
    {  0,  8,  2, 10 },
    { 12,  4, 14,  6 },
    {  3, 11,  1,  9 },
    { 15,  7, 13,  5 },
}

local orderedMatrix8x8 = {
    {  0, 32,  8, 40,  2, 34, 10, 42 },
    { 48, 16, 56, 24, 50, 18, 58, 26 },
    { 12, 44,  4, 36, 14, 46,  6, 38 },
    { 60, 28, 52, 20, 62, 30, 54, 22 },
    {  3, 35, 11, 43,  1, 33,  9, 41 },
    { 51, 19, 59, 27, 49, 17, 57, 25 },
    { 15, 47,  7, 39, 13, 45,  5, 37 },
    { 63, 31, 55, 23, 61, 29, 53, 21 },
}

local function applyOrderedDither(color, cx, cy, matrix, matrixSize, levels)
    levels = levels or 4
    local threshold = matrix[((cy - 1) % matrixSize) + 1][((cx - 1) % matrixSize) + 1]
    local maxVal = matrixSize * matrixSize
    local offset = (threshold / maxVal - 0.5) / levels

    local r = math.clamp(color.R + offset, 0, 1)
    local g = math.clamp(color.G + offset, 0, 1)
    local b = math.clamp(color.B + offset, 0, 1)

    -- Quantize to limited levels
    local function quantize(c)
        return math.floor(c * (levels - 1) + 0.5) / (levels - 1)
    end
    return Color3.new(quantize(r), quantize(g), quantize(b))
end

-- Edge detection using Sobel operator on the parsed image
local function computeEdgeMap(file, canvasW, canvasH, pixelSize, sampleFunc, threshold)
    local edgeMap = {}
    for cy = 1, canvasH do
        edgeMap[cy] = {}
        for cx = 1, canvasW do
            -- Sample 3x3 neighborhood brightness
            local function getBr(ox, oy)
                local sx = math.clamp(cx + ox, 1, canvasW)
                local sy = math.clamp(cy + oy, 1, canvasH)
                local imgSY = canvasH - sy + 1
                local c, _ = sampleFunc(file, sx, imgSY, pixelSize)
                return 0.299 * c.R + 0.587 * c.G + 0.114 * c.B
            end

            local tl = getBr(-1, -1)
            local tc = getBr( 0, -1)
            local tr = getBr( 1, -1)
            local ml = getBr(-1,  0)
            local mr = getBr( 1,  0)
            local bl = getBr(-1,  1)
            local bc = getBr( 0,  1)
            local br = getBr( 1,  1)

            local gx = -tl + tr - 2*ml + 2*mr - bl + br
            local gy = -tl - 2*tc - tr + bl + 2*bc + br
            local mag = math.sqrt(gx * gx + gy * gy)

            edgeMap[cy][cx] = mag >= threshold
        end
    end
    return edgeMap
end

-- ============================================================
-- GUI SETUP
-- ============================================================

-- Forward-declare status labels
local statusLabel, imageInfoLabel, canvasInfoLabel, blocksLabel, progressLabel, speedLabel
local blockCountLabel, startBlockLabel
local elapsedLabel, etaLabel, bpsGraphLabel, resumeInfoLabel, coopInfoLabel

-- Forward-declare core functions
local previewImage, startBuild, stopBuild

-- Window
local Window = Library:CreateWindow({
    Title = "ThinkingFace v4.0",
    Center = true,
    AutoShow = true,
    Size = UDim2.fromOffset(580, 650),
})

-- Home Tab
Library:CreateHomeTab(Window, {
    ScriptName = "ThinkingFace",
    Version = "v4.0",
    Creator = "pealz",
    Description = "Image to Canvas build system. Converts images into colored block art using Build and PaintBucket tools. Supports PNG and BMP natively, with proxy support for JPEG/WebP/GIF. Features multiple build modes, detail sampling, shape masks, 3D depth, HD mode (resized blocks for higher detail), co-op building (split canvas between players), visual effects (dithering, edge detection, color effects, gradient maps), and auto-resume.",
})

-- ==================== IMAGE TAB ====================
local ImageTab = Window:AddTab("Image")

-- Left column: Image Settings
local ImageBox = ImageTab:AddLeftGroupbox("Image Settings")

ImageBox:AddInput("ImageURL", {
    Text = "Image URL",
    Default = "",
    Placeholder = "Paste image URL here",
    Tooltip = "Direct link to a PNG or BMP image (or other format with proxy)",
})

ImageBox:AddDivider()

ImageBox:AddSlider("PixelSize", {
    Text = "Pixel Size",
    Default = 4,
    Min = 1,
    Max = 16,
    Rounding = 0,
    Tooltip = "Image pixels per block. Higher = fewer blocks, coarser image",
})

ImageBox:AddSlider("MaxWidth", {
    Text = "Max Width",
    Default = 200,
    Min = 10,
    Max = 500,
    Rounding = 0,
    Suffix = " blk",
})

ImageBox:AddSlider("MaxHeight", {
    Text = "Max Height",
    Default = 100,
    Min = 10,
    Max = 500,
    Rounding = 0,
    Suffix = " blk",
})

ImageBox:AddSlider("ScalePercent", {
    Text = "Scale",
    Default = 100,
    Min = 10,
    Max = 400,
    Rounding = 0,
    Suffix = "%",
    Tooltip = "Resize image before processing. 100% = original size",
})

ImageBox:AddDivider()

ImageBox:AddButton({
    Text = "Preview Image",
    Func = function() previewImage() end,
    Tooltip = "Download and analyze the image without building",
})

local startBtn = ImageBox:AddButton({
    Text = "Start Build",
    Func = function() startBuild() end,
    Tooltip = "Begin building the canvas",
})
startBtn:AddButton({
    Text = "Stop Build",
    Func = function() stopBuild() end,
    Tooltip = "Stop the current build",
})

-- Right column: Status
local StatusBox = ImageTab:AddRightGroupbox("Status")
statusLabel     = StatusBox:AddLabel("Status: Idle", true)
imageInfoLabel  = StatusBox:AddLabel("Image: --", true)
canvasInfoLabel = StatusBox:AddLabel("Canvas: --", true)
blocksLabel     = StatusBox:AddLabel("Blocks: --", true)
progressLabel   = StatusBox:AddLabel("Progress: --", true)
speedLabel      = StatusBox:AddLabel("Speed: --", true)
elapsedLabel    = StatusBox:AddLabel("Elapsed: --", true)
etaLabel        = StatusBox:AddLabel("ETA: --", true)
bpsGraphLabel   = StatusBox:AddLabel("BPS: --", true)

StatusBox:AddDivider()

resumeInfoLabel = StatusBox:AddLabel("Resume: no saved progress", true)

StatusBox:AddButton({
    Text = "Reset Progress",
    Func = function()
        getgenv().ThinkingFace_Progress = nil
        resumeInfoLabel:SetText("Resume: progress cleared")
        Library:Notify("Build progress reset.", 2)
    end,
    Tooltip = "Clear saved build progress",
})

-- Right column: Format Info
local FormatBox = ImageTab:AddRightGroupbox("Supported Formats")
FormatBox:AddLabel("PNG + BMP: Natively supported.", true)
FormatBox:AddLabel("JPEG/WebP/GIF: Requires proxy.", true)
FormatBox:AddLabel("Set proxy URL in Build Settings", true)
FormatBox:AddLabel("tab to convert formats on the fly.", true)
FormatBox:AddLabel("Or convert to PNG manually first.", true)

-- ==================== BUILD SETTINGS TAB ====================
local SettingsTab = Window:AddTab("Build Settings")

-- Left column: Build Mode
local BuildBox = SettingsTab:AddLeftGroupbox("Build Mode")

BuildBox:AddDropdown("BuildMode", {
    Text = "Build Mode",
    Values = { "Standard", "Build-then-Paint", "Paint Only" },
    Default = "Standard",
    Tooltip = "Standard: build+paint each block. Build-then-Paint: build all, then paint all. Paint Only: paint existing blocks.",
})

BuildBox:AddDropdown("DetailMode", {
    Text = "Detail Mode",
    Values = { "Nearest Neighbor", "Average", "Bilinear" },
    Default = "Average",
    Tooltip = "How to sample color from image. Average gives best quality.",
})

BuildBox:AddDivider()

BuildBox:AddSlider("BuildDelay", {
    Text = "Extra Delay",
    Default = 0,
    Min = 0,
    Max = 500,
    Rounding = 0,
    Suffix = " ms",
    Tooltip = "Additional delay between each block placement. 0 = fastest",
})

BuildBox:AddDropdown("HDir", {
    Text = "Horizontal Direction",
    Values = { "Right", "Left", "Front", "Back" },
    Default = "Right",
    Tooltip = "Which direction columns expand from start block",
})

BuildBox:AddDropdown("VDir", {
    Text = "Vertical Direction",
    Values = { "Up", "Down" },
    Default = "Up",
    Tooltip = "Which direction each column builds",
})

BuildBox:AddDivider()

BuildBox:AddToggle("PaintWhite", {
    Text = "Paint Transparent as White",
    Default = true,
    Tooltip = "When off, transparent pixels keep default block color",
})

-- Right column: Format & Proxy
local ProxyBox = SettingsTab:AddRightGroupbox("Format & Proxy")

ProxyBox:AddInput("ProxyURL", {
    Text = "Proxy URL",
    Default = "",
    Placeholder = "https://proxy.example.com/convert",
    Tooltip = "Proxy server URL for converting JPEG/WebP/GIF to PNG",
})

ProxyBox:AddLabel("Proxy converts unsupported formats", true)
ProxyBox:AddLabel("to PNG. The script appends:", true)
ProxyBox:AddLabel("?url=<encoded_image_url>", true)

-- Right column: Block Info
local InfoBox = SettingsTab:AddRightGroupbox("Block Info")
blockCountLabel = InfoBox:AddLabel("Your blocks: --", true)
startBlockLabel = InfoBox:AddLabel("Start block: --", true)

InfoBox:AddDivider()

InfoBox:AddButton({
    Text = "Refresh Block Info",
    Func = function()
        local bricks = workspace.Bricks[localplr.Name]:GetChildren()
        blockCountLabel:SetText("Your blocks: " .. #bricks)
        if #bricks > 0 then
            local b = bricks[1]
            startBlockLabel:SetText(string.format(
                "Start: %.0f, %.0f, %.0f", b.Position.X, b.Position.Y, b.Position.Z
            ))
        else
            startBlockLabel:SetText("Start block: none")
        end
    end,
})

InfoBox:AddButton({
    Text = "Teleport to Start Block",
    Func = function()
        local bricks = workspace.Bricks[localplr.Name]:GetChildren()
        if #bricks == 0 then
            Library:Notify("No blocks in your plot!", 3)
            return
        end
        local hrp = localplr.Character and localplr.Character:FindFirstChild("HumanoidRootPart")
        if hrp then
            hrp.CFrame = bricks[1].CFrame + Vector3.new(0, 5, 0)
            Library:Notify("Teleported to starting block.", 2)
        end
    end,
})

InfoBox:AddButton({
    Text = "Get PaintBucket",
    Func = function()
        pcall(function()
            game:GetService("ReplicatedStorage").DefaultChatSystemChatEvents.SayMessageRequest:FireServer(";gear me 18474459", "All")
        end)
        Library:Notify("Sent PaintBucket gear command.", 3)
    end,
    Tooltip = "Fire chat command to get PaintBucket gear",
})

-- ==================== SHAPE & 3D TAB ====================
local ShapeTab = Window:AddTab("Shape & 3D")

-- Left column: Shape
local ShapeBox = ShapeTab:AddLeftGroupbox("Shape")

ShapeBox:AddDropdown("ShapeMode", {
    Text = "Shape",
    Values = { "Rectangle", "Circle", "Dome", "Cylinder", "Sphere", "Wall" },
    Default = "Rectangle",
    Tooltip = "Shape mask for the canvas. Rectangle = full grid.",
})

-- Left column: 3D Depth
local DepthBox = ShapeTab:AddLeftGroupbox("3D Depth")

DepthBox:AddToggle("EnableDepth", {
    Text = "Enable Depth",
    Default = false,
    Tooltip = "Add 3D depth to the canvas using brightness or a depth map",
})

DepthBox:AddDropdown("DepthSource", {
    Text = "Depth Source",
    Values = { "Brightness", "Depth Map" },
    Default = "Brightness",
    Tooltip = "Brightness: uses pixel brightness. Depth Map: uses separate grayscale image.",
})

DepthBox:AddSlider("MaxDepth", {
    Text = "Max Depth",
    Default = 5,
    Min = 1,
    Max = 20,
    Rounding = 0,
    Tooltip = "Maximum column depth in blocks",
})

DepthBox:AddToggle("InvertDepth", {
    Text = "Invert Depth",
    Default = false,
    Tooltip = "When on, lighter pixels = taller columns",
})

DepthBox:AddInput("DepthMapURL", {
    Text = "Depth Map URL",
    Default = "",
    Placeholder = "URL to grayscale depth map",
    Tooltip = "Optional URL for a depth map image (grayscale)",
})

DepthBox:AddDropdown("DepthDir", {
    Text = "Depth Direction",
    Values = { "Front", "Back", "Left", "Right" },
    Default = "Back",
    Tooltip = "Direction blocks extend for depth",
})

-- Right column: Shape Info
local ShapeInfoBox = ShapeTab:AddRightGroupbox("Shape Info")
ShapeInfoBox:AddLabel("Rectangle: Full grid, no masking.", true)
ShapeInfoBox:AddLabel("Circle: Circular mask on canvas.", true)
ShapeInfoBox:AddLabel("Dome: Hemisphere, height varies", true)
ShapeInfoBox:AddLabel("  by distance from center.", true)
ShapeInfoBox:AddLabel("Cylinder: Circular base, uniform", true)
ShapeInfoBox:AddLabel("  height from depth setting.", true)
ShapeInfoBox:AddLabel("Sphere: Both hemispheres built", true)
ShapeInfoBox:AddLabel("  above and below.", true)
ShapeInfoBox:AddLabel("Wall: Flat vertical wall.", true)

ShapeInfoBox:AddDivider()

local DepthInfoBox = ShapeTab:AddRightGroupbox("Depth Info")
DepthInfoBox:AddLabel("Depth adds Z-axis blocks behind", true)
DepthInfoBox:AddLabel("the canvas surface.", true)
DepthInfoBox:AddLabel("Brightness: pixel brightness maps", true)
DepthInfoBox:AddLabel("  to column height (1..MaxDepth).", true)
DepthInfoBox:AddLabel("Depth Map: separate grayscale", true)
DepthInfoBox:AddLabel("  image controls column height.", true)

-- ==================== HD MODE TAB ====================
local HDTab = Window:AddTab("HD Mode")

local HDBox = HDTab:AddLeftGroupbox("HD Settings")

HDBox:AddToggle("EnableHD", {
    Text = "Enable HD Mode",
    Default = false,
    Tooltip = "Place blocks then shrink them for higher detail in the same space",
})

HDBox:AddDropdown("BlockScale", {
    Text = "Block Scale",
    Values = { "1/2 Size", "1/3 Size", "1/4 Size" },
    Default = "1/2 Size",
    Tooltip = "How much to shrink each block after placement",
})

HDBox:AddDropdown("ResizeMethod", {
    Text = "Resize Method",
    Values = { "Resize Tool", "Direct" },
    Default = "Resize Tool",
    Tooltip = "Resize Tool: uses the in-game Resize tool. Direct: sets block size directly (may not replicate).",
})

local HDInfoBox = HDTab:AddRightGroupbox("HD Mode Info")
HDInfoBox:AddLabel("HD Mode places blocks then shrinks", true)
HDInfoBox:AddLabel("them using the Resize tool. This", true)
HDInfoBox:AddLabel("allows 2x-4x detail in the same", true)
HDInfoBox:AddLabel("space.", true)
HDInfoBox:AddDivider()
HDInfoBox:AddLabel("Requires Resize tool in inventory.", true)
HDInfoBox:AddDivider()
HDInfoBox:AddLabel("1/2 Size = 2x detail (4x blocks)", true)
HDInfoBox:AddLabel("1/3 Size = 3x detail (9x blocks)", true)
HDInfoBox:AddLabel("1/4 Size = 4x detail (16x blocks)", true)
HDInfoBox:AddDivider()
HDInfoBox:AddLabel("Note: HD mode significantly", true)
HDInfoBox:AddLabel("increases build time. Test on", true)
HDInfoBox:AddLabel("small canvases first.", true)

-- ==================== CO-OP TAB ====================
local CoopTab = Window:AddTab("Co-op")

local CoopBox = CoopTab:AddLeftGroupbox("Co-op Settings")

CoopBox:AddToggle("EnableCoop", {
    Text = "Enable Co-op",
    Default = false,
    Tooltip = "Split the canvas between two players building simultaneously",
})

CoopBox:AddDropdown("CoopSection", {
    Text = "My Section",
    Values = { "Full Canvas", "Left Half", "Right Half", "Top Half", "Bottom Half", "Odd Columns", "Even Columns", "Custom Range" },
    Default = "Full Canvas",
    Tooltip = "Which part of the canvas this player builds",
})

CoopBox:AddSlider("CoopStartCol", {
    Text = "Start Column",
    Default = 1,
    Min = 1,
    Max = 500,
    Rounding = 0,
    Tooltip = "Starting column for custom range (only used with Custom Range)",
})

CoopBox:AddSlider("CoopEndCol", {
    Text = "End Column",
    Default = 100,
    Min = 1,
    Max = 500,
    Rounding = 0,
    Tooltip = "Ending column for custom range",
})

CoopBox:AddSlider("CoopStartRow", {
    Text = "Start Row",
    Default = 1,
    Min = 1,
    Max = 500,
    Rounding = 0,
    Tooltip = "Starting row for custom range",
})

CoopBox:AddSlider("CoopEndRow", {
    Text = "End Row",
    Default = 100,
    Min = 1,
    Max = 500,
    Rounding = 0,
    Tooltip = "Ending row for custom range",
})

coopInfoLabel = CoopBox:AddLabel("Section: Full Canvas", true)

local CoopHelpBox = CoopTab:AddRightGroupbox("Co-op Info")
CoopHelpBox:AddLabel("Both players load the same image", true)
CoopHelpBox:AddLabel("URL and settings. Each picks", true)
CoopHelpBox:AddLabel("their section.", true)
CoopHelpBox:AddDivider()
CoopHelpBox:AddLabel("Player 1: Left Half", true)
CoopHelpBox:AddLabel("Player 2: Right Half", true)
CoopHelpBox:AddLabel("Both click Start Build.", true)
CoopHelpBox:AddDivider()
CoopHelpBox:AddLabel("Each player builds only their", true)
CoopHelpBox:AddLabel("assigned columns/rows. The", true)
CoopHelpBox:AddLabel("image sampling uses the correct", true)
CoopHelpBox:AddLabel("global canvas coordinates so", true)
CoopHelpBox:AddLabel("both halves match up.", true)
CoopHelpBox:AddDivider()
CoopHelpBox:AddLabel("Custom Range: set exact column", true)
CoopHelpBox:AddLabel("and row bounds for your section.", true)

-- Update co-op info label when settings change
local function updateCoopInfo()
    local section = Options.CoopSection.Value
    if section == "Custom Range" then
        coopInfoLabel:SetText(string.format("Section: cols %d-%d, rows %d-%d",
            Options.CoopStartCol.Value, Options.CoopEndCol.Value,
            Options.CoopStartRow.Value, Options.CoopEndRow.Value))
    else
        coopInfoLabel:SetText("Section: " .. section)
    end
end

Options.CoopSection:OnChanged(updateCoopInfo)
Options.CoopStartCol:OnChanged(updateCoopInfo)
Options.CoopEndCol:OnChanged(updateCoopInfo)
Options.CoopStartRow:OnChanged(updateCoopInfo)
Options.CoopEndRow:OnChanged(updateCoopInfo)

-- ==================== EFFECTS TAB ====================
local EffectsTab = Window:AddTab("Effects")

local DitherBox = EffectsTab:AddLeftGroupbox("Dithering")

DitherBox:AddDropdown("Dithering", {
    Text = "Dithering",
    Values = { "None", "Floyd-Steinberg", "Ordered 2x2", "Ordered 4x4", "Ordered 8x8" },
    Default = "None",
    Tooltip = "Apply dithering to the image for stylized pixel art look",
})

local EdgeBox = EffectsTab:AddLeftGroupbox("Edge Detection")

EdgeBox:AddToggle("EnableEdge", {
    Text = "Edge Detection",
    Default = false,
    Tooltip = "Detect edges in the image using Sobel operator",
})

EdgeBox:AddSlider("EdgeThreshold", {
    Text = "Edge Threshold",
    Default = 0.15,
    Min = 0.01,
    Max = 1.0,
    Rounding = 2,
    Tooltip = "Sensitivity of edge detection. Lower = more edges detected",
})

EdgeBox:AddToggle("EdgesOnly", {
    Text = "Edges Only",
    Default = false,
    Tooltip = "When on, only builds edge blocks. When off, darkens edges.",
})

local ColorFXBox = EffectsTab:AddRightGroupbox("Color Effects")

ColorFXBox:AddDropdown("ColorEffect", {
    Text = "Color Effect",
    Values = { "None", "Grayscale", "Sepia", "Invert", "Posterize", "High Contrast", "Neon Glow" },
    Default = "None",
    Tooltip = "Apply a color effect to the entire image",
})

ColorFXBox:AddSlider("PosterizeLevels", {
    Text = "Posterize Levels",
    Default = 4,
    Min = 2,
    Max = 16,
    Rounding = 0,
    Tooltip = "Number of color levels for posterize effect",
})

ColorFXBox:AddDropdown("MirrorMode", {
    Text = "Mirror",
    Values = { "None", "Horizontal", "Vertical", "Both" },
    Default = "None",
    Tooltip = "Mirror the image before building",
})

ColorFXBox:AddDropdown("GradientMap", {
    Text = "Gradient Map",
    Values = { "None", "Sunset", "Ocean", "Forest", "Infrared", "Rainbow", "Fire", "Ice" },
    Default = "None",
    Tooltip = "Map brightness to a color gradient",
})

-- ==================== HELP TAB ====================
local HelpTab = Window:AddTab("Help")

local HowToBox = HelpTab:AddLeftGroupbox("How to Use")
HowToBox:AddLabel("1. Place ONE block in your plot", true)
HowToBox:AddLabel("   (clear extras for best results)", true)
HowToBox:AddLabel("2. Have Build + PaintBucket tools", true)
HowToBox:AddLabel("   in your inventory or backpack", true)
HowToBox:AddLabel("3. Paste an image URL (PNG/BMP)", true)
HowToBox:AddLabel("4. Adjust Pixel Size for detail", true)
HowToBox:AddLabel("5. Click Preview to check dimensions", true)
HowToBox:AddLabel("6. Click Start Build to begin", true)
HowToBox:AddLabel("7. Use Stop Build to halt mid-build", true)

HowToBox:AddDivider()

HowToBox:AddLabel("Pixel Size Guide:", false)
HowToBox:AddLabel("  1-2: Very detailed (many blocks)", true)
HowToBox:AddLabel("  3-4: Good balance (recommended)", true)
HowToBox:AddLabel("  5-8: Coarse (fewer, faster)", true)
HowToBox:AddLabel("  9+:  Very pixelated", true)

HowToBox:AddDivider()

HowToBox:AddLabel("Build Modes:", false)
HowToBox:AddLabel("  Standard: build + paint each block", true)
HowToBox:AddLabel("  Build-then-Paint: build all blocks,", true)
HowToBox:AddLabel("    then paint all instantly", true)
HowToBox:AddLabel("  Paint Only: paint existing blocks", true)
HowToBox:AddLabel("    without building new ones", true)

local TipsBox = HelpTab:AddRightGroupbox("Tips")
TipsBox:AddLabel("PNG and BMP are supported natively.", true)
TipsBox:AddLabel("JPEG/WebP/GIF need a proxy URL set", true)
TipsBox:AddLabel("in Build Settings to convert them.", true)
TipsBox:AddDivider()
TipsBox:AddLabel("The image builds outward from your", true)
TipsBox:AddLabel("starting block. Use Build Settings", true)
TipsBox:AddLabel("to control direction.", true)
TipsBox:AddDivider()
TipsBox:AddLabel("Large images with Pixel Size 1 take", true)
TipsBox:AddLabel("a very long time. Test with size 4.", true)
TipsBox:AddDivider()
TipsBox:AddLabel("Use Shape & 3D tab to create curved", true)
TipsBox:AddLabel("or 3D structures from your images.", true)
TipsBox:AddDivider()
TipsBox:AddLabel("Average detail mode gives the best", true)
TipsBox:AddLabel("color accuracy for most images.", true)
TipsBox:AddDivider()
TipsBox:AddLabel("Build-then-Paint mode is faster", true)
TipsBox:AddLabel("since painting has no cooldown.", true)
TipsBox:AddDivider()
TipsBox:AddLabel("HD Mode: shrinks blocks for 2-4x", true)
TipsBox:AddLabel("detail. Needs Resize tool.", true)
TipsBox:AddDivider()
TipsBox:AddLabel("Co-op: split canvas between players.", true)
TipsBox:AddLabel("Each picks Left/Right half.", true)
TipsBox:AddDivider()
TipsBox:AddLabel("Effects: add dithering, edge detect,", true)
TipsBox:AddLabel("color effects, and gradient maps.", true)

-- Toggle Button
Library:CreateToggleButton("ThinkingFace")

-- ============================================================
-- CORE FUNCTIONS
-- ============================================================

-- Helper: update canvas info labels from current settings + parsed file
local function updatePreviewInfo()
    if not parsedFile then return end
    local ps = Options.PixelSize.Value
    local canvasW = math.min(math.ceil(parsedFile.Width / ps), Options.MaxWidth.Value)
    local canvasH = math.min(math.ceil(parsedFile.Height / ps), Options.MaxHeight.Value)
    canvasInfoLabel:SetText(string.format("Canvas: %dx%d (px size %d)", canvasW, canvasH, ps))
    blocksLabel:SetText(string.format("Total blocks: %d", canvasW * canvasH))
end

-- Live-update canvas info when sliders change
Options.PixelSize:OnChanged(function() updatePreviewInfo() end)
Options.MaxWidth:OnChanged(function() updatePreviewInfo() end)
Options.MaxHeight:OnChanged(function() updatePreviewInfo() end)
Options.ScalePercent:OnChanged(function()
    parsedFile = nil
end)

-- Clear parsed file when URL changes
Options.ImageURL:OnChanged(function()
    parsedFile = nil
end)

-- Update resume info on load
pcall(function()
    local prog = getgenv().ThinkingFace_Progress
    if prog then
        resumeInfoLabel:SetText(string.format("Resume: col %d, row %d saved", prog.cx or 0, prog.cy or 0))
    end
end)

-- ============================================================
-- IMAGE DOWNLOAD & PARSING
-- ============================================================
local function downloadAndParse(url)
    -- Download
    statusLabel:SetText("Status: Downloading...")
    local ok, rawData = pcall(function() return game:HttpGet(url) end)
    if not ok then
        statusLabel:SetText("Status: Download failed")
        Library:Notify("Failed to download: " .. tostring(rawData), 5)
        return nil, nil
    end

    -- Detect file type
    local fileType = pnglib.DetectFileType(rawData)

    -- Handle format-specific parsing
    if fileType == "png" then
        statusLabel:SetText("Status: Parsing PNG...")
        local parseOk, file = pcall(PNG.new, rawData)
        if not parseOk then
            statusLabel:SetText("Status: Parse failed")
            Library:Notify("PNG parse error: " .. tostring(file), 5)
            return nil, nil
        end
        return file, fileType

    elseif fileType == "bmp" then
        statusLabel:SetText("Status: Parsing BMP...")
        local parseOk, file = pcall(function() return pnglib.BMP.new(rawData) end)
        if not parseOk then
            statusLabel:SetText("Status: BMP parse failed")
            Library:Notify("BMP parse error: " .. tostring(file), 5)
            return nil, nil
        end
        return file, fileType

    elseif fileType == "jpeg" or fileType == "webp" or fileType == "gif" then
        -- Try proxy if configured
        local proxyURL = Options.ProxyURL.Value
        if proxyURL and proxyURL ~= "" then
            statusLabel:SetText("Status: Converting via proxy...")
            local encodedURL = game:GetService("HttpService"):UrlEncode(url)
            local proxyFullURL = proxyURL .. "?url=" .. encodedURL
            local proxyOk, proxyData = pcall(function() return game:HttpGet(proxyFullURL) end)
            if not proxyOk then
                statusLabel:SetText("Status: Proxy failed")
                Library:Notify("Proxy conversion failed: " .. tostring(proxyData), 5)
                return nil, nil
            end
            -- The proxy should return PNG data
            local proxyType = pnglib.DetectFileType(proxyData)
            if proxyType == "png" then
                statusLabel:SetText("Status: Parsing converted PNG...")
                local parseOk2, file2 = pcall(PNG.new, proxyData)
                if not parseOk2 then
                    statusLabel:SetText("Status: Parse failed")
                    Library:Notify("Parse error after proxy: " .. tostring(file2), 5)
                    return nil, nil
                end
                return file2, "png"
            elseif proxyType == "bmp" then
                statusLabel:SetText("Status: Parsing converted BMP...")
                local parseOk2, file2 = pcall(function() return pnglib.BMP.new(proxyData) end)
                if not parseOk2 then
                    statusLabel:SetText("Status: Parse failed")
                    Library:Notify("BMP parse error after proxy: " .. tostring(file2), 5)
                    return nil, nil
                end
                return file2, "bmp"
            else
                statusLabel:SetText("Status: Proxy returned unknown format")
                Library:Notify("Proxy did not return a valid PNG/BMP.", 5)
                return nil, nil
            end
        else
            statusLabel:SetText("Status: Unsupported format")
            Library:Notify(string.format(
                "Detected %s format. Set a Proxy URL in Build Settings to convert, or manually convert to PNG.",
                fileType:upper()
            ), 5)
            imageInfoLabel:SetText("Format: " .. fileType:upper() .. " (needs proxy)")
            return nil, nil
        end

    else
        statusLabel:SetText("Status: Unknown format")
        Library:Notify("Could not detect file type. Only PNG and BMP are natively supported.", 5)
        imageInfoLabel:SetText("Format: unknown")
        return nil, nil
    end
end

-- ============================================================
-- PREVIEW IMAGE
-- ============================================================
function previewImage()
    local url = Options.ImageURL.Value
    if url == "" then
        Library:Notify("Enter an image URL first.", 3)
        return
    end

    local file, fileType = downloadAndParse(url)
    if not file then return end

    -- Apply resize if scale != 100
    local scalePct = Options.ScalePercent.Value
    if scalePct ~= 100 then
        statusLabel:SetText("Status: Resizing...")
        local resizeOk, resized = pcall(function() return pnglib.Resize(file, scalePct) end)
        if resizeOk and resized then
            file = resized
        else
            Library:Notify("Resize failed, using original size.", 3)
        end
    end

    parsedFile = file

    -- Color type names (for PNG)
    local formatInfo = ""
    if fileType == "png" and file.ColorType then
        local ctNames = { [0] = "Grayscale", [2] = "RGB", [3] = "Palette", [4] = "Gray+Alpha", [6] = "RGBA" }
        local ctName = ctNames[file.ColorType] or ("Type " .. file.ColorType)
        formatInfo = string.format("Image: %dx%d | PNG %s %dbit", file.Width, file.Height, ctName, file.BitDepth or 8)
    else
        formatInfo = string.format("Image: %dx%d | %s", file.Width, file.Height, (fileType or "unknown"):upper())
    end

    local ps = Options.PixelSize.Value
    local canvasW = math.min(math.ceil(file.Width / ps), Options.MaxWidth.Value)
    local canvasH = math.min(math.ceil(file.Height / ps), Options.MaxHeight.Value)
    local totalBlocks = canvasW * canvasH

    statusLabel:SetText("Status: Preview ready")
    imageInfoLabel:SetText(formatInfo)
    canvasInfoLabel:SetText(string.format("Canvas: %dx%d (px size %d)", canvasW, canvasH, ps))
    blocksLabel:SetText(string.format("Total blocks: %d", totalBlocks))
    progressLabel:SetText("Progress: --")
    speedLabel:SetText("Speed: --")
    elapsedLabel:SetText("Elapsed: --")
    etaLabel:SetText("ETA: --")

    Library:Notify(string.format(
        "Image loaded: %dx%d -> %dx%d canvas (%d blocks)",
        file.Width, file.Height, canvasW, canvasH, totalBlocks
    ), 5)
end

-- ============================================================
-- START BUILD
-- ============================================================
function startBuild()
    if isBuilding then
        Library:Notify("Build already in progress!", 3)
        return
    end

    local url = Options.ImageURL.Value
    if url == "" then
        Library:Notify("Enter an image URL first.", 3)
        return
    end

    local buildMode = Options.BuildMode.Value

    -- Check for starting blocks
    local bricks = workspace.Bricks[localplr.Name]:GetChildren()
    if #bricks == 0 and buildMode ~= "Paint Only" then
        Library:Notify("No blocks in your plot! Place one block first.", 5)
        statusLabel:SetText("Status: No blocks found")
        return
    end

    -- Check for PaintBucket
    local pb = getPaintBucket()
    if not pb then
        Library:Notify("PaintBucket not found! Add it to your inventory.", 5)
        statusLabel:SetText("Status: No PaintBucket")
        return
    end

    -- Parse image if not cached
    if not parsedFile then
        local file, fileType = downloadAndParse(url)
        if not file then return end

        -- Apply resize
        local scalePct = Options.ScalePercent.Value
        if scalePct ~= 100 then
            statusLabel:SetText("Status: Resizing...")
            local resizeOk, resized = pcall(function() return pnglib.Resize(file, scalePct) end)
            if resizeOk and resized then
                file = resized
            end
        end

        parsedFile = file
    end

    -- Read all settings from GUI
    local file        = parsedFile
    local pixelSize   = Options.PixelSize.Value
    local maxW        = Options.MaxWidth.Value
    local maxH        = Options.MaxHeight.Value
    local extraDelay  = Options.BuildDelay.Value
    local hDir        = directionMap[Options.HDir.Value] or Enum.NormalId.Right
    local vDir        = directionMap[Options.VDir.Value]  or Enum.NormalId.Top
    local paintWhite  = Toggles.PaintWhite.Value
    local detailMode  = Options.DetailMode.Value
    local shapeMode   = Options.ShapeMode.Value
    local enableDepth = Toggles.EnableDepth.Value
    local depthSource = Options.DepthSource.Value
    local maxDepth    = Options.MaxDepth.Value
    local invertDepth = Toggles.InvertDepth.Value
    local depthDir    = directionMap[Options.DepthDir.Value] or Enum.NormalId.Back

    -- HD Mode settings
    local enableHD      = Toggles.EnableHD.Value
    local blockScaleStr = Options.BlockScale.Value
    local resizeMethod  = Options.ResizeMethod.Value
    local hdScale       = 0.5
    if blockScaleStr == "1/3 Size" then hdScale = 1/3
    elseif blockScaleStr == "1/4 Size" then hdScale = 0.25
    end

    -- Co-op settings
    local enableCoop  = Toggles.EnableCoop.Value
    local coopSection = Options.CoopSection.Value
    local customRange = {
        startCol = Options.CoopStartCol.Value,
        endCol   = Options.CoopEndCol.Value,
        startRow = Options.CoopStartRow.Value,
        endRow   = Options.CoopEndRow.Value,
    }

    -- Effects settings
    local ditherMode      = Options.Dithering.Value
    local enableEdge      = Toggles.EnableEdge.Value
    local edgeThreshold   = Options.EdgeThreshold.Value
    local edgesOnly       = Toggles.EdgesOnly.Value
    local colorEffect     = Options.ColorEffect.Value
    local posterizeLevels = Options.PosterizeLevels.Value
    local mirrorMode      = Options.MirrorMode.Value
    local gradientMap     = Options.GradientMap.Value

    local canvasW = math.min(math.ceil(file.Width  / pixelSize), maxW)
    local canvasH = math.min(math.ceil(file.Height / pixelSize), maxH)

    -- Calculate total blocks (accounting for shape, depth, and co-op)
    local shapeMask = shapeMaskFuncs[shapeMode] or maskRectangle
    local totalBlocks = 0
    for cx = 1, canvasW do
        for cy = 1, canvasH do
            -- Co-op filter
            if enableCoop and not isMyBlock(cx, cy, canvasW, canvasH, coopSection, customRange) then
                -- Skip blocks not in our section (still count for iteration but not for total)
            elseif not shapeMask(cx, cy, canvasW, canvasH) then
                if enableDepth or shapeMode == "Dome" or shapeMode == "Sphere" then
                    local d = 1
                    if shapeMode == "Dome" then
                        d = shapeHeightDome(cx, cy, canvasW, canvasH, maxDepth)
                    elseif shapeMode == "Sphere" then
                        d = shapeHeightSphere(cx, cy, canvasW, canvasH, maxDepth)
                    elseif enableDepth then
                        d = maxDepth
                    end
                    totalBlocks = totalBlocks + math.max(1, d)
                else
                    totalBlocks = totalBlocks + 1
                end
            end
        end
    end

    -- Select sampling function
    local sampleFunc
    if detailMode == "Nearest Neighbor" then
        sampleFunc = sampleNearest
    elseif detailMode == "Bilinear" then
        sampleFunc = sampleBilinear
    else
        sampleFunc = sampleAverage
    end

    -- Pre-compute edge map if edge detection is enabled
    local edgeMap = nil
    if enableEdge then
        statusLabel:SetText("Status: Computing edge map...")
        edgeMap = computeEdgeMap(file, canvasW, canvasH, pixelSize, sampleFunc, edgeThreshold)
    end

    -- Download depth map if needed
    local depthMapFile = nil
    if enableDepth and depthSource == "Depth Map" then
        local depthURL = Options.DepthMapURL.Value
        if depthURL and depthURL ~= "" then
            statusLabel:SetText("Status: Downloading depth map...")
            local dFile, dType = downloadAndParse(depthURL)
            if dFile then
                -- Resize depth map to match scale if needed
                local scalePct = Options.ScalePercent.Value
                if scalePct ~= 100 then
                    local rok, rfile = pcall(function() return pnglib.Resize(dFile, scalePct) end)
                    if rok and rfile then dFile = rfile end
                end
                depthMapFile = dFile
            else
                Library:Notify("Depth map download failed, using brightness.", 3)
            end
        else
            Library:Notify("No depth map URL set, using brightness.", 3)
        end
    end

    -- Check for resume
    local resumeFromCX, resumeFromCY = 1, 1
    local resuming = false
    local prog = getgenv().ThinkingFace_Progress
    if prog and prog.url == url and prog.cx and prog.cy then
        Library:Notify(string.format("Saved progress found: col %d, row %d. Resuming...", prog.cx, prog.cy), 5)
        resumeFromCX = prog.cx
        resumeFromCY = prog.cy
        resuming = true
    end

    -- Run build in a separate thread so the GUI stays responsive
    task.spawn(function()
        isBuilding = true
        getgenv().stopimage = false

        statusLabel:SetText("Status: Building...")
        canvasInfoLabel:SetText(string.format("Canvas: %dx%d", canvasW, canvasH))
        blocksLabel:SetText(string.format("Total: ~%d blocks", totalBlocks))

        local paintevent = pb.Remotes.ServerControls

        -- Optional handshake
        pcall(function()
            local templateBrick = game.ReplicatedStorage:FindFirstChild("Brick")
            if templateBrick then
                paintevent:InvokeServer("PaintPart", { Part = templateBrick, Color = Color3.new(1, 1, 1) })
            end
        end)

        local startTime = tick()
        local placed = 0

        -- BPS tracking for graph
        local bpsHistory = {}
        local lastBpsUpdate = startTime

        -- Get pixel color for a canvas coordinate using selected detail mode + effects pipeline
        local function getColor(cx, cy)
            -- Step 1: Apply mirror coordinate remapping (before sampling)
            local sampleCX, sampleCY = cx, cy
            if mirrorMode ~= "None" then
                sampleCX, sampleCY = applyMirror(cx, cy, canvasW, canvasH, mirrorMode)
            end

            -- Step 2: Sample color from image
            local imgCY = canvasH - sampleCY + 1
            local color, alpha = sampleFunc(file, sampleCX, imgCY, pixelSize)
            if alpha < 10 then
                return paintWhite and Color3.new(1, 1, 1) or nil
            end

            -- Step 3: Apply color effect
            if colorEffect ~= "None" then
                color = applyColorEffect(color, colorEffect, posterizeLevels)
            end

            -- Step 4: Apply gradient map
            if gradientMap ~= "None" then
                color = applyGradientMap(color, gradientMap)
            end

            -- Step 5: Apply dithering
            if ditherMode == "Ordered 2x2" then
                color = applyOrderedDither(color, cx, cy, orderedMatrix2x2, 2, 8)
            elseif ditherMode == "Ordered 4x4" then
                color = applyOrderedDither(color, cx, cy, orderedMatrix4x4, 4, 8)
            elseif ditherMode == "Ordered 8x8" then
                color = applyOrderedDither(color, cx, cy, orderedMatrix8x8, 8, 8)
            elseif ditherMode == "Floyd-Steinberg" then
                -- Floyd-Steinberg is applied per-pixel; approximate with ordered + noise
                -- True F-S requires sequential processing of all pixels, so we use the PNGLib implementation
                local ok, result = pcall(function()
                    return pnglib.Dither.FloydSteinberg(color, cx, cy)
                end)
                if ok and result then
                    color = result
                end
            end

            -- Step 6: Apply edge detection
            if enableEdge and edgeMap then
                local isEdge = edgeMap[cy] and edgeMap[cy][cx]
                if edgesOnly then
                    if not isEdge then
                        return nil -- Skip non-edge blocks
                    end
                else
                    -- Darken edges
                    if isEdge then
                        color = Color3.new(
                            color.R * 0.3,
                            color.G * 0.3,
                            color.B * 0.3
                        )
                    end
                end
            end

            return color
        end

        -- Get depth for a canvas coordinate
        local function getDepth(cx, cy)
            if shapeMode == "Dome" then
                return shapeHeightDome(cx, cy, canvasW, canvasH, maxDepth)
            elseif shapeMode == "Sphere" then
                return shapeHeightSphere(cx, cy, canvasW, canvasH, maxDepth)
            end

            if not enableDepth then return 1 end

            local brightness = 0
            if depthSource == "Depth Map" and depthMapFile then
                -- Sample from depth map
                local dmX = math.clamp(math.floor((cx - 1) * (depthMapFile.Width / canvasW)) + 1, 1, depthMapFile.Width)
                local dmY = math.clamp(math.floor((cy - 1) * (depthMapFile.Height / canvasH)) + 1, 1, depthMapFile.Height)
                local dmColor, dmAlpha = depthMapFile:GetPixel(dmX, dmY)
                brightness = getBrightness(dmColor)
            else
                -- Use image brightness
                local imgCY = canvasH - cy + 1
                local color, alpha = sampleFunc(file, cx, imgCY, pixelSize)
                brightness = getBrightness(color)
            end

            -- Default: darker pixels = deeper (more blocks)
            -- Invert: lighter pixels = deeper (more blocks)
            if invertDepth then
                return math.max(1, math.floor(brightness * maxDepth + 0.5))
            else
                return math.max(1, math.floor((1 - brightness) * maxDepth + 0.5))
            end
        end

        -- Format elapsed time as Xm Ys
        local function formatElapsed(seconds)
            local m = math.floor(seconds / 60)
            local s = math.floor(seconds % 60)
            if m > 0 then
                return string.format("%dm %ds", m, s)
            else
                return string.format("%ds", s)
            end
        end

        -- Update progress display with stats
        local function updateProgress()
            placed = placed + 1
            local elapsed = tick() - startTime
            local bps = placed / math.max(elapsed, 0.01)
            local remaining = totalBlocks - placed
            local eta = remaining / math.max(bps, 0.01)
            local pct = (placed / totalBlocks) * 100
            progressLabel:SetText(string.format("Progress: %d/%d (%.1f%%)", placed, totalBlocks, pct))
            speedLabel:SetText(string.format("%.1f blk/s", bps))
            elapsedLabel:SetText("Elapsed: " .. formatElapsed(elapsed))
            etaLabel:SetText("ETA: " .. formatElapsed(eta))

            -- Update BPS graph (text-based, last 10 readings)
            local now = tick()
            if now - lastBpsUpdate >= 2 then
                table.insert(bpsHistory, math.floor(bps * 10 + 0.5) / 10)
                if #bpsHistory > 10 then
                    table.remove(bpsHistory, 1)
                end
                lastBpsUpdate = now

                local graphParts = {}
                for _, v in ipairs(bpsHistory) do
                    table.insert(graphParts, string.format("%.1f", v))
                end
                bpsGraphLabel:SetText("BPS: [" .. table.concat(graphParts, " ") .. "]")
            end
        end

        -- Save progress for resume
        local function saveProgress(cx, cy)
            getgenv().ThinkingFace_Progress = {
                url = url,
                cx = cx,
                cy = cy,
            }
            resumeInfoLabel:SetText(string.format("Resume: col %d, row %d", cx, cy))
        end

        -- ============================================================
        -- BUILD MODE: PAINT ONLY
        -- ============================================================
        if buildMode == "Paint Only" then
            statusLabel:SetText("Status: Paint Only mode...")
            local allBricks = workspace.Bricks[localplr.Name]:GetChildren()
            if #allBricks == 0 then
                Library:Notify("No blocks found to paint!", 5)
                statusLabel:SetText("Status: No blocks")
                isBuilding = false
                return
            end

            -- Sort blocks by position to determine canvas mapping
            -- Find bounding box
            local minX, minY, minZ = math.huge, math.huge, math.huge
            local maxX2, maxY2, maxZ2 = -math.huge, -math.huge, -math.huge
            for _, brick in ipairs(allBricks) do
                local pos = brick.Position
                if pos.X < minX then minX = pos.X end
                if pos.Y < minY then minY = pos.Y end
                if pos.Z < minZ then minZ = pos.Z end
                if pos.X > maxX2 then maxX2 = pos.X end
                if pos.Y > maxY2 then maxY2 = pos.Y end
                if pos.Z > maxZ2 then maxZ2 = pos.Z end
            end

            -- Determine block size (assume uniform)
            local blockSize = 4 -- default block size
            if #allBricks > 0 then
                local firstBrick = allBricks[1]
                blockSize = math.max(firstBrick.Size.X, firstBrick.Size.Y, firstBrick.Size.Z)
                if blockSize < 1 then blockSize = 4 end
            end

            -- Determine which axes correspond to horizontal and vertical
            -- Based on the configured H and V directions
            local hDirStr = Options.HDir.Value
            local vDirStr = Options.VDir.Value

            totalBlocks = #allBricks
            blocksLabel:SetText(string.format("Total: %d blocks", totalBlocks))

            for _, brick in ipairs(allBricks) do
                if getgenv().stopimage then break end

                local pos = brick.Position
                local cx, cy

                -- Map position to canvas coordinate based on direction settings
                if hDirStr == "Right" then
                    cx = math.floor((pos.X - minX) / blockSize) + 1
                elseif hDirStr == "Left" then
                    cx = math.floor((maxX2 - pos.X) / blockSize) + 1
                elseif hDirStr == "Front" then
                    cx = math.floor((minZ - pos.Z) / blockSize) + 1 -- Front is -Z
                    if cx < 1 then cx = math.floor((pos.Z - minZ) / blockSize) + 1 end
                elseif hDirStr == "Back" then
                    cx = math.floor((pos.Z - minZ) / blockSize) + 1
                else
                    cx = math.floor((pos.X - minX) / blockSize) + 1
                end

                if vDirStr == "Up" then
                    cy = math.floor((pos.Y - minY) / blockSize) + 1
                elseif vDirStr == "Down" then
                    cy = math.floor((maxY2 - pos.Y) / blockSize) + 1
                else
                    cy = math.floor((pos.Y - minY) / blockSize) + 1
                end

                cx = math.clamp(cx, 1, canvasW)
                cy = math.clamp(cy, 1, canvasH)

                -- Co-op filter for Paint Only mode
                if enableCoop and not isMyBlock(cx, cy, canvasW, canvasH, coopSection, customRange) then
                    -- Skip blocks not in our section
                else
                    local color = getColor(cx, cy)
                    if color then
                        paintBlock(paintevent, brick, color)
                    end
                    updateProgress()
                end
            end

            local elapsed = tick() - startTime
            isBuilding = false
            getgenv().stopimage = false

            if placed >= totalBlocks then
                statusLabel:SetText("Status: Paint complete!")
                Library:Notify(string.format("Painting complete! %d blocks in %s.", placed, formatElapsed(elapsed)), 5)
            else
                statusLabel:SetText(string.format("Status: Stopped (%d/%d)", placed, totalBlocks))
                Library:Notify(string.format("Painting stopped. %d/%d blocks painted.", placed, totalBlocks), 5)
            end
            return
        end

        -- ============================================================
        -- BUILD MODE: STANDARD & BUILD-THEN-PAINT
        -- ============================================================
        local paintQueue = {} -- Used for Build-then-Paint mode

        bricks = workspace.Bricks[localplr.Name]:GetChildren()
        local sideblock = bricks[1]
        local block = sideblock

        -- Helper: paint or queue a block
        local function handlePaint(blk, color)
            if not color then return end
            if buildMode == "Standard" then
                paintBlock(paintevent, blk, color)
            else -- Build-then-Paint
                table.insert(paintQueue, { block = blk, color = color })
            end
        end

        -- Helper: apply HD resize after placing a block
        local function handleHDResize(blk)
            if enableHD and blk then
                if resizeMethod == "Direct" then
                    pcall(function()
                        blk.Size = Vector3.new(
                            blk.Size.X * hdScale,
                            blk.Size.Y * hdScale,
                            blk.Size.Z * hdScale
                        )
                    end)
                else
                    resizeBlock(blk, hdScale)
                end
            end
        end

        -- Helper: process a canvas position (paint surface + build depth)
        local function processPosition(blk, cx, cy)
            if shapeMask(cx, cy, canvasW, canvasH) then return end

            -- Co-op filter: skip blocks not in our section
            if enableCoop and not isMyBlock(cx, cy, canvasW, canvasH, coopSection, customRange) then
                return
            end

            local c = getColor(cx, cy)
            handlePaint(blk, c)
            handleHDResize(blk)
            updateProgress()

            -- Save progress periodically
            if placed % 50 == 0 then
                saveProgress(cx, cy)
            end

            -- Build depth blocks behind the surface
            local depth = getDepth(cx, cy)
            if depth > 1 then
                local depthBlock = blk
                for d = 2, depth do
                    if getgenv().stopimage then break end
                    depthBlock = buildonblock(depthBlock, depthDir, extraDelay)
                    if not depthBlock then break end
                    handlePaint(depthBlock, c)
                    handleHDResize(depthBlock)
                    updateProgress()
                end
            end
        end

        -- Determine starting column and row based on resume
        local startX = 1
        local startY = 1
        if resuming then
            startX = resumeFromCX
            startY = resumeFromCY
        end

        -- Process starting block at canvas position (1, 1) unless resuming past it
        if startX == 1 and startY <= 1 then
            processPosition(sideblock, 1, 1)
        end

        -- Build columns left to right
        -- Column x=1: build upward from y=2 to canvasH (y=1 already done)
        -- Then step sideways, build column x=2 from y=1 to canvasH, etc.

        -- Grow first column vertically (y=2 to canvasH) unless resuming past it
        if startX == 1 then
            local firstY = (startY > 1) and startY or 2
            for y = 2, canvasH do
                if getgenv().stopimage then break end
                block = buildonblock(block, vDir, extraDelay)
                if not block then break end
                if y >= firstY then
                    processPosition(block, 1, y)
                end
            end
        else
            -- If resuming past column 1, we need to skip to the right column
            -- Build first column fully to get sideblock positioned correctly
            for y = 2, canvasH do
                if getgenv().stopimage then break end
                block = buildonblock(block, vDir, extraDelay)
                if not block then break end
            end
        end

        -- Build remaining columns
        for x = 2, canvasW do
            if getgenv().stopimage then break end

            -- Step sideways from the bottom of the previous column
            sideblock = buildonblock(sideblock, hDir, extraDelay)
            if not sideblock then break end
            block = sideblock

            if x < startX then
                -- Skip this column entirely (resume) but still build structure
                -- We need the sideblock to advance, but skip vertical building
                -- Build the column for structural continuity
                for y = 2, canvasH do
                    if getgenv().stopimage then break end
                    block = buildonblock(block, vDir, extraDelay)
                    if not block then break end
                end
            else
                -- Process bottom block of this column (y=1)
                local colStartY = 1
                if x == startX and startY > 1 then
                    colStartY = startY
                end

                if colStartY <= 1 then
                    processPosition(sideblock, x, 1)
                end

                -- Build this column upward (y=2 to canvasH)
                for y = 2, canvasH do
                    if getgenv().stopimage then break end
                    block = buildonblock(block, vDir, extraDelay)
                    if not block then break end
                    if y >= colStartY then
                        processPosition(block, x, y)
                    end
                end
            end
        end

        -- ============================================================
        -- PHASE 2: BUILD-THEN-PAINT — Paint all blocks rapidly
        -- ============================================================
        if buildMode == "Build-then-Paint" and not getgenv().stopimage then
            statusLabel:SetText("Status: Painting phase...")
            Library:Notify(string.format("Build phase done. Painting %d blocks...", #paintQueue), 3)

            for i, entry in ipairs(paintQueue) do
                if getgenv().stopimage then break end
                paintBlock(paintevent, entry.block, entry.color)
                -- Painting has no cooldown, but yield occasionally to prevent timeout
                if i % 50 == 0 then
                    task.wait()
                    local pct = (i / #paintQueue) * 100
                    progressLabel:SetText(string.format("Painting: %d/%d (%.1f%%)", i, #paintQueue, pct))
                end
            end
        end

        local elapsed = tick() - startTime
        getgenv().stopimage = false
        isBuilding = false

        -- Clear progress on completion
        if placed >= totalBlocks then
            getgenv().ThinkingFace_Progress = nil
            resumeInfoLabel:SetText("Resume: build complete")
            statusLabel:SetText("Status: Complete!")
            Library:Notify(string.format("Build complete! %d blocks in %s.", placed, formatElapsed(elapsed)), 5)
        else
            -- Save final progress for resume
            saveProgress(canvasW, canvasH)
            statusLabel:SetText(string.format("Status: Stopped (%d/%d)", placed, totalBlocks))
            Library:Notify(string.format("Build stopped. %d/%d blocks placed. Progress saved for resume.", placed, totalBlocks), 5)
        end
    end)
end

-- ============================================================
-- STOP BUILD
-- ============================================================
function stopBuild()
    if isBuilding then
        getgenv().stopimage = true
        statusLabel:SetText("Status: Stopping...")
        Library:Notify("Stopping build...", 2)
    else
        Library:Notify("No build in progress.", 2)
    end
end

-- ============================================================
-- INITIALIZATION
-- ============================================================

-- Auto-refresh block info on load
pcall(function()
    local bricks = workspace.Bricks[localplr.Name]:GetChildren()
    blockCountLabel:SetText("Your blocks: " .. #bricks)
    if #bricks > 0 then
        local b = bricks[1]
        startBlockLabel:SetText(string.format(
            "Start: %.0f, %.0f, %.0f", b.Position.X, b.Position.Y, b.Position.Z
        ))
    end
end)

-- Store reference for cleanup on re-run
getgenv().ThinkingFace = Library

print("[ThinkingFace v4.0] Loaded! Toggle UI with RightControl or RightShift.")
