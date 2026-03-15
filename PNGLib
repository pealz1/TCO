local PNG = {}

----------------------------------------------------------------------
-- BinaryReader
----------------------------------------------------------------------
do
    local BinaryReader = {}
    BinaryReader.__index = BinaryReader

    function BinaryReader.new(Data)
        local self = setmetatable({}, BinaryReader)
        self.Data = Data
        self.Position = 1
        return self
    end

    function BinaryReader:ReadByte()
        local Byte = string.byte(self.Data, self.Position)
        self.Position = self.Position + 1
        return Byte
    end

    function BinaryReader:ReadBytes(Count)
        local Bytes = string.sub(self.Data, self.Position, self.Position + Count - 1)
        self.Position = self.Position + Count
        return Bytes
    end

    function BinaryReader:ReadUInt32()
        local Byte1, Byte2, Byte3, Byte4 = string.byte(self.Data, self.Position, self.Position + 3)
        self.Position = self.Position + 4
        return (Byte1 * 16777216) + (Byte2 * 65536) + (Byte3 * 256) + Byte4
    end

    function BinaryReader:ReadUInt16()
        local Byte1, Byte2 = string.byte(self.Data, self.Position, self.Position + 1)
        self.Position = self.Position + 2
        return (Byte1 * 256) + Byte2
    end

    function BinaryReader:ReadUInt32LE()
        local Byte1, Byte2, Byte3, Byte4 = string.byte(self.Data, self.Position, self.Position + 3)
        self.Position = self.Position + 4
        return Byte1 + (Byte2 * 256) + (Byte3 * 65536) + (Byte4 * 16777216)
    end

    function BinaryReader:ReadUInt16LE()
        local Byte1, Byte2 = string.byte(self.Data, self.Position, self.Position + 1)
        self.Position = self.Position + 2
        return Byte1 + (Byte2 * 256)
    end

    function BinaryReader:Tell()
        return self.Position
    end

    function BinaryReader:Seek(Pos)
        self.Position = Pos
    end

    function BinaryReader:Size()
        return #self.Data
    end

    PNG.BinaryReader = BinaryReader
end

----------------------------------------------------------------------
-- Unfilter
----------------------------------------------------------------------
do
    local Unfilter = {}

    local function bytesToString(bytes, len)
        local CHUNK = 2048
        local parts = {}
        for i = 1, len, CHUNK do
            local j = math.min(i + CHUNK - 1, len)
            parts[#parts + 1] = string.char(unpack(bytes, i, j))
        end
        return table.concat(parts)
    end

    function Unfilter.None(Data)
        return Data
    end

    function Unfilter.Sub(Data, PrevLine, BPP)
        local len = #Data
        local bytes = {}
        for I = 1, len do
            local raw = string.byte(Data, I)
            local left = I > BPP and bytes[I - BPP] or 0
            bytes[I] = (raw + left) % 256
        end
        return bytesToString(bytes, len)
    end

    function Unfilter.Up(Data, PrevLine)
        local len = #Data
        local bytes = {}
        for I = 1, len do
            local raw = string.byte(Data, I)
            local above = PrevLine and string.byte(PrevLine, I) or 0
            bytes[I] = (raw + above) % 256
        end
        return bytesToString(bytes, len)
    end

    function Unfilter.Average(Data, PrevLine, BPP)
        local len = #Data
        local bytes = {}
        for I = 1, len do
            local raw = string.byte(Data, I)
            local left = I > BPP and bytes[I - BPP] or 0
            local above = PrevLine and string.byte(PrevLine, I) or 0
            bytes[I] = (raw + math.floor((left + above) / 2)) % 256
        end
        return bytesToString(bytes, len)
    end

    function Unfilter.Paeth(Data, PrevLine, BPP)
        local len = #Data
        local bytes = {}
        for I = 1, len do
            local raw = string.byte(Data, I)
            local a = I > BPP and bytes[I - BPP] or 0
            local b = PrevLine and string.byte(PrevLine, I) or 0
            local c = (PrevLine and I > BPP) and string.byte(PrevLine, I - BPP) or 0

            local p = a + b - c
            local pa = math.abs(p - a)
            local pb = math.abs(p - b)
            local pc = math.abs(p - c)

            local pr
            if pa <= pb and pa <= pc then
                pr = a
            elseif pb <= pc then
                pr = b
            else
                pr = c
            end

            bytes[I] = (raw + pr) % 256
        end
        return bytesToString(bytes, len)
    end

    PNG.Unfilter = Unfilter
end

----------------------------------------------------------------------
-- Deflate
----------------------------------------------------------------------
do
    local band = bit32.band
    local lshift = bit32.lshift
    local rshift = bit32.rshift

    local BTYPE_NO_COMPRESSION = 0
    local BTYPE_FIXED_HUFFMAN = 1
    local BTYPE_DYNAMIC_HUFFMAN = 2

    local lens = {[0] = 3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,67,83,99,115,131,163,195,227,258}
    local lext = {[0] = 0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0}
    local dists = {[0] = 1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769,1025,1537,2049,3073,4097,6145,8193,12289,16385,24577}
    local dext = {[0] = 0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13}
    local order = {16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15}
    local fixedLit = {0,8,144,9,256,7,280,8,288}
    local fixedDist = {0,5,32}

    local function createState(bitStream)
        local state = {Output = bitStream, Window = {}, Pos = 1}
        return state
    end

    local function write(state, byte)
        local pos = state.Pos
        state.Output(byte)
        state.Window[pos] = byte
        state.Pos = pos % 32768 + 1
    end

    local function memoize(fn)
        local meta = {}
        local memoizer = setmetatable({}, meta)
        function meta:__index(k)
            local v = fn(k)
            memoizer[k] = v
            return v
        end
        return memoizer
    end

    local pow2 = memoize(function (n) return 2 ^ n end)
    local isBitStream = setmetatable({}, { __mode = 'k' })

    local function createBitStream(reader)
        local buffer = 0
        local bitsLeft = 0
        local stream = {}
        isBitStream[stream] = true

        function stream:GetBitsLeft()
            return bitsLeft
        end

        function stream:Read(count)
            count = count or 1
            while bitsLeft < count do
                local byte = reader:ReadByte()
                if not byte then return end
                buffer = buffer + lshift(byte, bitsLeft)
                bitsLeft = bitsLeft + 8
            end
            local bits
            if count == 0 then
                bits = 0
            elseif count == 32 then
                bits = buffer
                buffer = 0
            else
                bits = band(buffer, rshift(2^32 - 1, 32 - count))
                buffer = rshift(buffer, count)
            end
            bitsLeft = bitsLeft - count
            return bits
        end

        return stream
    end

    local function getBitStream(obj)
        if isBitStream[obj] then return obj end
        return createBitStream(obj)
    end

    local function sortHuffman(a, b)
        return a.NumBits == b.NumBits and a.Value < b.Value or a.NumBits < b.NumBits
    end

    local function msb(bits, numBits)
        local res = 0
        for i = 1, numBits do
            res = lshift(res, 1) + band(bits, 1)
            bits = rshift(bits, 1)
        end
        return res
    end

    local function createHuffmanTable(init, isFull)
        local hTable = {}
        if isFull then
            for val, numBits in pairs(init) do
                if numBits ~= 0 then
                    hTable[#hTable + 1] = {Value = val, NumBits = numBits}
                end
            end
        else
            for i = 1, #init - 2, 2 do
                local firstVal = init[i]
                local numBits = init[i + 1]
                local nextVal = init[i + 2]
                if numBits ~= 0 then
                    for val = firstVal, nextVal - 1 do
                        hTable[#hTable + 1] = {Value = val, NumBits = numBits}
                    end
                end
            end
        end
        table.sort(hTable, sortHuffman)
        local code = 1
        local numBits = 0
        for i, slide in ipairs(hTable) do
            if slide.NumBits ~= numBits then
                code = code * pow2[slide.NumBits - numBits]
                numBits = slide.NumBits
            end
            slide.Code = code
            code = code + 1
        end
        local minBits = math.huge
        local look = {}
        for i, slide in ipairs(hTable) do
            minBits = math.min(minBits, slide.NumBits)
            look[slide.Code] = slide.Value
        end
        local firstCode = memoize(function (bits)
            return pow2[minBits] + msb(bits, minBits)
        end)
        function hTable:Read(bitStream)
            local code = 1
            local numBits = 0
            while true do
                if numBits == 0 then
                    local index = bitStream:Read(minBits)
                    numBits = numBits + minBits
                    code = firstCode[index]
                else
                    local bit = bitStream:Read()
                    numBits = numBits + 1
                    code = code * 2 + bit
                end
                local val = look[code]
                if val then return val end
            end
        end
        return hTable
    end

    local function parseZlibHeader(bitStream)
        local cm = bitStream:Read(4)
        local cinfo = bitStream:Read(4)
        local fcheck = bitStream:Read(5)
        local fdict = bitStream:Read(1)
        local flevel = bitStream:Read(2)
        local cmf = cinfo * 16 + cm
        local flg = fcheck + fdict * 32 + flevel * 64
        if cm ~= 8 then error("unrecognized zlib compression method: " .. cm) end
        if cinfo > 7 then error("invalid zlib window size: cinfo=" .. cinfo) end
        local windowSize = 2 ^ (cinfo + 8)
        if (cmf * 256 + flg) % 31 ~= 0 then error("invalid zlib header (bad fcheck sum)") end
        if fdict == 1 then error("FIX:TODO - FDICT not currently implemented") end
        return windowSize
    end

    local function parseHuffmanTables(bitStream)
        local numLits = bitStream:Read(5)
        local numDists = bitStream:Read(5)
        local numCodes = bitStream:Read(4)
        local codeLens = {}
        for i = 1, numCodes + 4 do
            local index = order[i]
            codeLens[index] = bitStream:Read(3)
        end
        codeLens = createHuffmanTable(codeLens, true)
        local function decode(numCodes)
            local init = {}
            local numBits
            local val = 0
            while val < numCodes do
                local codeLen = codeLens:Read(bitStream)
                local numRepeats
                if codeLen <= 15 then
                    numRepeats = 1
                    numBits = codeLen
                elseif codeLen == 16 then
                    numRepeats = 3 + bitStream:Read(2)
                elseif codeLen == 17 then
                    numRepeats = 3 + bitStream:Read(3)
                    numBits = 0
                elseif codeLen == 18 then
                    numRepeats = 11 + bitStream:Read(7)
                    numBits = 0
                end
                for i = 1, numRepeats do
                    init[val] = numBits
                    val = val + 1
                end
            end
            return createHuffmanTable(init, true)
        end
        local numLitCodes = numLits + 257
        local numDistCodes = numDists + 1
        local litTable = decode(numLitCodes)
        local distTable = decode(numDistCodes)
        return litTable, distTable
    end

    local function parseCompressedItem(bitStream, state, litTable, distTable)
        local val = litTable:Read(bitStream)
        if val < 256 then
            write(state, val)
        elseif val == 256 then
            return true
        else
            local lenBase = lens[val - 257]
            local numExtraBits = lext[val - 257]
            local extraBits = bitStream:Read(numExtraBits)
            local len = lenBase + extraBits
            local distVal = distTable:Read(bitStream)
            local distBase = dists[distVal]
            local distNumExtraBits = dext[distVal]
            local distExtraBits = bitStream:Read(distNumExtraBits)
            local dist = distBase + distExtraBits
            for i = 1, len do
                local pos = (state.Pos - 1 - dist) % 32768 + 1
                local byte = assert(state.Window[pos], "invalid distance")
                write(state, byte)
            end
        end
        return false
    end

    local function parseBlock(bitStream, state)
        local bFinal = bitStream:Read(1)
        local bType = bitStream:Read(2)
        if bType == BTYPE_NO_COMPRESSION then
            local left = bitStream:GetBitsLeft()
            bitStream:Read(left)
            local len = bitStream:Read(16)
            local nlen = bitStream:Read(16)
            for i = 1, len do
                local byte = bitStream:Read(8)
                write(state, byte)
            end
        elseif bType == BTYPE_FIXED_HUFFMAN or bType == BTYPE_DYNAMIC_HUFFMAN then
            local litTable, distTable
            if bType == BTYPE_DYNAMIC_HUFFMAN then
                litTable, distTable = parseHuffmanTables(bitStream)
            else
                litTable = createHuffmanTable(fixedLit)
                distTable = createHuffmanTable(fixedDist)
            end
            repeat until parseCompressedItem(bitStream, state, litTable, distTable)
        else
            error("unrecognized compression type")
        end
        return bFinal ~= 0
    end

    local Deflate = {}

    function Deflate:Inflate(io)
        local state = createState(io.Output)
        local bitStream = getBitStream(io.Input)
        repeat until parseBlock(bitStream, state)
    end

    function Deflate:InflateZlib(io)
        local bitStream = getBitStream(io.Input)
        local windowSize = parseZlibHeader(bitStream)
        self:Inflate { Input = bitStream, Output = io.Output }
        local bitsLeft = bitStream:GetBitsLeft()
        bitStream:Read(bitsLeft)
    end

    PNG.Deflate = Deflate
end

----------------------------------------------------------------------
-- Chunk Loader
----------------------------------------------------------------------
do
    function PNG.LoadChunkHandler(chunkType)
        local chunkUrl = "https://raw.githubusercontent.com/MaximumADHD/Roblox-PNG-Library/refs/heads/master/Chunks/" .. chunkType .. ".lua"
        local handler = game:HttpGet(chunkUrl)
        return loadstring(handler)()
    end
end

----------------------------------------------------------------------
-- BMP Parser Module
----------------------------------------------------------------------
do
    local BMP = {}

    local BMPImage = {}
    BMPImage.__index = BMPImage

    function BMPImage:GetPixel(x, y)
        -- x, y are 1-based pixel coordinates
        local row = self.Bitmap[y]
        if not row then
            return Color3.new(0, 0, 0), 0
        end

        local bpp = self.BytesPerPixel
        local offset = (x - 1) * bpp + 1

        if bpp == 3 then
            local r = string.byte(row, offset)
            local g = string.byte(row, offset + 1)
            local b = string.byte(row, offset + 2)
            return Color3.new(r / 255, g / 255, b / 255), 255
        else -- bpp == 4
            local r = string.byte(row, offset)
            local g = string.byte(row, offset + 1)
            local b = string.byte(row, offset + 2)
            local a = string.byte(row, offset + 3)
            return Color3.new(r / 255, g / 255, b / 255), a
        end
    end

    function BMP.new(buffer)
        local reader = PNG.BinaryReader.new(buffer)

        -- BITMAPFILEHEADER (14 bytes)
        local sig = reader:ReadBytes(2)
        if sig ~= "BM" then
            error("Not a valid BMP file: bad signature")
        end

        local fileSize = reader:ReadUInt32LE()
        local reserved1 = reader:ReadUInt16LE()
        local reserved2 = reader:ReadUInt16LE()
        local pixelOffset = reader:ReadUInt32LE()

        -- BITMAPINFOHEADER (40 bytes)
        local headerSize = reader:ReadUInt32LE()
        local width = reader:ReadUInt32LE()
        local heightRaw = reader:ReadUInt32LE()

        -- Height can be signed (top-down if negative). Handle via two's complement.
        local topDown = false
        local height = heightRaw
        if height >= 2147483648 then
            -- Negative height in unsigned 32-bit means top-down
            height = 4294967296 - height
            topDown = true
        end

        local planes = reader:ReadUInt16LE()
        local bitsPerPixel = reader:ReadUInt16LE()
        local compression = reader:ReadUInt32LE()
        local imageSize = reader:ReadUInt32LE()
        local xPPM = reader:ReadUInt32LE()
        local yPPM = reader:ReadUInt32LE()
        local colorsUsed = reader:ReadUInt32LE()
        local colorsImportant = reader:ReadUInt32LE()

        if compression ~= 0 then
            error("Only uncompressed BMP is supported (compression=" .. compression .. ")")
        end

        if bitsPerPixel ~= 24 and bitsPerPixel ~= 32 then
            error("Only 24-bit and 32-bit BMP supported (got " .. bitsPerPixel .. "bpp)")
        end

        local bytesPerPixel = bitsPerPixel / 8
        local rowStride = math.floor((bitsPerPixel * width + 31) / 32) * 4

        -- Build bitmap rows: convert BGR(A) to RGB(A), flip to top-down order
        local bitmap = {}

        for row = 0, height - 1 do
            -- Determine which file row to read
            local fileRow
            if topDown then
                fileRow = row
            else
                fileRow = height - 1 - row
            end

            reader:Seek(pixelOffset + fileRow * rowStride + 1) -- +1 for 1-based positioning

            local pixels = {}
            for col = 0, width - 1 do
                local b = reader:ReadByte()
                local g = reader:ReadByte()
                local r = reader:ReadByte()

                pixels[#pixels + 1] = string.char(r, g, b)

                if bytesPerPixel == 4 then
                    local a = reader:ReadByte()
                    pixels[#pixels] = string.char(r, g, b, a)
                end
            end

            bitmap[row + 1] = table.concat(pixels)
        end

        local image = setmetatable({}, BMPImage)
        image.Width = width
        image.Height = height
        image.BitDepth = 8
        image.BytesPerPixel = bytesPerPixel
        image.Bitmap = bitmap

        if bytesPerPixel == 3 then
            image.ColorType = 2  -- RGB, matches PNG truecolor
        else
            image.ColorType = 6  -- RGBA, matches PNG truecolor+alpha
        end

        return image
    end

    PNG.BMP = BMP
end

----------------------------------------------------------------------
-- Image Resampling Module
----------------------------------------------------------------------
do
    local Resample = {}

    function Resample.Nearest(file, x, y)
        local cx = math.clamp(x, 1, file.Width)
        local cy = math.clamp(y, 1, file.Height)
        return file:GetPixel(cx, cy)
    end

    function Resample.Average(file, x1, y1, x2, y2)
        -- Clamp all coordinates
        x1 = math.clamp(x1, 1, file.Width)
        x2 = math.clamp(x2, 1, file.Width)
        y1 = math.clamp(y1, 1, file.Height)
        y2 = math.clamp(y2, 1, file.Height)

        -- Ensure proper ordering
        if x1 > x2 then x1, x2 = x2, x1 end
        if y1 > y2 then y1, y2 = y2, y1 end

        local rSum, gSum, bSum, aSum = 0, 0, 0, 0
        local count = 0

        for py = y1, y2 do
            for px = x1, x2 do
                local color, alpha = file:GetPixel(px, py)
                rSum = rSum + color.R
                gSum = gSum + color.G
                bSum = bSum + color.B
                aSum = aSum + alpha
                count = count + 1
            end
        end

        if count == 0 then
            return Color3.new(0, 0, 0), 0
        end

        return Color3.new(rSum / count, gSum / count, bSum / count), math.floor(aSum / count + 0.5)
    end

    function Resample.Bilinear(file, fx, fy)
        -- fx, fy are 1-based floating point coordinates
        local x0 = math.floor(fx)
        local y0 = math.floor(fy)
        local x1 = x0 + 1
        local y1 = y0 + 1

        local xFrac = fx - x0
        local yFrac = fy - y0

        -- Clamp to image bounds
        x0 = math.clamp(x0, 1, file.Width)
        x1 = math.clamp(x1, 1, file.Width)
        y0 = math.clamp(y0, 1, file.Height)
        y1 = math.clamp(y1, 1, file.Height)

        local c00, a00 = file:GetPixel(x0, y0)
        local c10, a10 = file:GetPixel(x1, y0)
        local c01, a01 = file:GetPixel(x0, y1)
        local c11, a11 = file:GetPixel(x1, y1)

        local invX = 1 - xFrac
        local invY = 1 - yFrac

        local r = (c00.R * invX + c10.R * xFrac) * invY + (c01.R * invX + c11.R * xFrac) * yFrac
        local g = (c00.G * invX + c10.G * xFrac) * invY + (c01.G * invX + c11.G * xFrac) * yFrac
        local b = (c00.B * invX + c10.B * xFrac) * invY + (c01.B * invX + c11.B * xFrac) * yFrac
        local a = (a00 * invX + a10 * xFrac) * invY + (a01 * invX + a11 * xFrac) * yFrac

        return Color3.new(
            math.clamp(r, 0, 1),
            math.clamp(g, 0, 1),
            math.clamp(b, 0, 1)
        ), math.floor(math.clamp(a, 0, 255) + 0.5)
    end

    PNG.Resample = Resample
end

----------------------------------------------------------------------
-- Image Resize Function
----------------------------------------------------------------------
do
    local ResizedImage = {}
    ResizedImage.__index = ResizedImage

    function ResizedImage:GetPixel(x, y)
        -- Map from resized coordinates back to original image coordinates
        -- Formula: floor((x - 0.5) * scaleInv) + 1  (center-to-center nearest neighbor)
        local srcX = math.clamp(math.floor((x - 0.5) * self._scaleInv) + 1, 1, self._source.Width)
        local srcY = math.clamp(math.floor((y - 0.5) * self._scaleInv) + 1, 1, self._source.Height)
        return self._source:GetPixel(srcX, srcY)
    end

    function PNG.Resize(file, scalePct)
        local scale = scalePct / 100
        local newWidth = math.max(1, math.floor(file.Width * scale + 0.5))
        local newHeight = math.max(1, math.floor(file.Height * scale + 0.5))

        local resized = setmetatable({}, ResizedImage)
        resized.Width = newWidth
        resized.Height = newHeight
        resized.ColorType = file.ColorType
        resized.BitDepth = file.BitDepth
        resized.BytesPerPixel = file.BytesPerPixel
        resized._source = file
        resized._scaleInv = 1 / scale
        return resized
    end
end

----------------------------------------------------------------------
-- File Type Detection
----------------------------------------------------------------------
do
    function PNG.DetectFileType(data)
        if #data < 4 then
            return "unknown"
        end

        local b1, b2, b3, b4 = string.byte(data, 1, 4)

        -- PNG: 137 80 78 71 (0x89 P N G)
        if b1 == 137 and b2 == 80 and b3 == 78 and b4 == 71 then
            return "png"
        end

        -- BMP: "BM"
        if b1 == 66 and b2 == 77 then
            return "bmp"
        end

        -- JPEG: FF D8 FF
        if b1 == 255 and b2 == 216 and b3 == 255 then
            return "jpeg"
        end

        -- GIF: "GIF8"
        if b1 == 71 and b2 == 73 and b3 == 70 and b4 == 56 then
            return "gif"
        end

        -- WebP: "RIFF" + offset 8 "WEBP"
        if b1 == 82 and b2 == 73 and b3 == 70 and b4 == 70 and #data >= 12 then
            local w1, w2, w3, w4 = string.byte(data, 9, 12)
            if w1 == 87 and w2 == 69 and w3 == 66 and w4 == 80 then
                return "webp"
            end
        end

        return "unknown"
    end
end

----------------------------------------------------------------------
-- Brightness Utility
----------------------------------------------------------------------
do
    function PNG.GetBrightness(color)
        return 0.299 * color.R + 0.587 * color.G + 0.114 * color.B
    end
end

----------------------------------------------------------------------
-- Global assignments for backward compatibility
----------------------------------------------------------------------
Deflate = PNG.Deflate
Unfilter = PNG.Unfilter
BinaryReader = PNG.BinaryReader

return PNG
