# ThinkingFace v4.0 — Design Spec

**Date:** 2026-03-15
**Scope:** Major feature upgrade to ThinkingFace image-to-canvas build system for TCO (Roblox).

---

## Overview

ThinkingFace v4.0 adds micro-block high-detail mode, co-op multi-worker canvas splitting, Floyd-Steinberg dithering, mirror/symmetry mode, edge/outline mode, and color filter pre-processing. PNGLib receives performance improvements: alpha-weighted averaging and a scanline byte cache.

---

## 1. PNGLib Improvements

### 1.1 Scanline Byte Cache

**Problem:** `GetPixel` calls `string.byte` on every access. `Resample.Average` hits every pixel in a region per block — for a 4×4 pixel-size region that is 16 `string.byte` calls per channel per block.

**Fix:** Add a `_rowCache` table on the file object. On first access of row `y`, decode the entire scanline string into a `{number}` array and store it in `_rowCache[y]`. Subsequent accesses index the table directly. Applied inside `GetPixel` transparently.

**Impact:** Average mode (the default and highest quality mode) becomes significantly faster. No API changes required.

### 1.2 Alpha-Weighted Color Averaging

**Problem:** `Resample.Average` sums all pixel colors equally regardless of alpha. A region with 15 opaque colored pixels and 1 fully transparent pixel biases the average toward black (alpha=0 means Color3.new(0,0,0)).

**Fix:** Weight each pixel's RGB contribution by its alpha value. Only include pixels with alpha > 0 in the divisor. If all pixels are transparent, return white (or the paintWhite color) with alpha=0.

```
rSum += color.R * alpha
gSum += color.G * alpha
bSum += color.B * alpha
alphaSum += alpha
-- final: Color3.new(rSum/alphaSum, gSum/alphaSum, bSum/alphaSum), avgAlpha
```

### 1.3 Dithering Pre-Processor

**New function:** `PNG.Dither(file, canvasW, canvasH, pixelSize)` — returns a 2D array `ditherMap[cy][cx] = Color3`.

**Algorithm:** Floyd-Steinberg error diffusion. Process pixels left-to-right, top-to-bottom across canvas coordinates. At each position, sample the true color, then distribute the quantization error to neighbors:
- Right: +7/16 of error
- Bottom-left: +3/16
- Bottom: +5/16
- Bottom-right: +1/16

In TCO the PaintBucket accepts any Color3, so "quantization" here means clamping to `[0,1]` per channel — the error is the difference between the ideal float color and the clamped result. This propagates sub-pixel color information into neighboring blocks, making the overall canvas appear more accurate.

Pre-computed once before build starts. Build loop does `color = ditherMap[cy][cx]` — pure table lookup, zero sampling overhead during build.

---

## 2. Micro-Block Mode

### 2.1 How It Works

The Build tool's `FireServer` call takes a 4th string parameter for block type:
- `"normal"` — standard 4-stud block (current behavior)
- `"small"` — 1-stud micro block

Micro-block mode toggles this parameter. At 1-stud resolution, each block represents exactly 1 image pixel — no pixel-size grouping needed.

### 2.2 UI

- **Toggle:** "Micro-Block Mode (High Detail)" in Build Settings tab
- When enabled: Pixel Size slider is hidden/disabled (forced to 1), canvas dimensions = image dimensions
- Warning label shown: "Small blocks are slow. Recommended max: 100×100 blocks (~10,000 blocks)"
- Works with all build modes, shapes, depth, co-op, and dithering

### 2.3 Constraints

- Co-op is strongly recommended for large micro-block builds
- Depth mode with micro-blocks produces extremely high block counts — warn user if `canvasW * canvasH * maxDepth > 5000`

---

## 3. Co-op Mode

### 3.1 Concept

Each worker runs ThinkingFace on their own account. They all use the same image URL and settings. The canvas columns are divided evenly. Each worker independently builds their slice.

### 3.2 Column Split

```
sliceW = floor(canvasW / totalWorkers)
startCol = (workerNum - 1) * sliceW + 1
endCol   = workerNum == totalWorkers and canvasW or workerNum * sliceW
```

Last worker gets any remainder columns so no pixels are missed.

### 3.3 Starting Block Placement

Each worker needs a starting block at the correct horizontal offset. The UI shows:

> "Place your start block **N columns** to the right of Worker 1's block"
> (N = `(workerNum - 1) * sliceW`)

Worker 1's starting block position is the anchor. All other workers count from there. Coordination is manual — players use chat/Discord to agree on the anchor block position before starting.

### 3.4 UI

New section in Build Settings (visible when "Enable Co-op" is toggled on):
- **Total Workers** — dropdown: 2, 3, 4
- **My Worker #** — dropdown: 1, 2, 3, 4 (auto-filtered to ≤ Total Workers)
- **Offset helper label** — "Place your start block X columns right of Worker 1"

All workers must use identical Image, Pixel Size, Max Width/Height, Scale, and direction settings or the slices won't align.

---

## 4. New Features

### 4.1 Dithering Mode

Added as a new option in the **Detail Mode** dropdown: `"Dithering"`.

When selected, `PNG.Dither()` is called during build setup (after image download/parse, before the build loop starts). Progress shown: "Status: Pre-processing dither...". Build loop then uses `ditherMap[cy][cx]` instead of calling sampleFunc.

Dithering is incompatible with Paint Only mode (no canvas position→pixel mapping). Show a warning and fall back to Average if Paint Only + Dithering is selected.

### 4.2 Mirror/Symmetry Mode

**Location:** Shape & 3D tab, new dropdown "Mirror Mode" with values: None, Horizontal, Vertical, Quad.

**How it works:**

The build loop always builds the full canvas physically (blocks are placed everywhere). Color sampling is modified:

- **Horizontal:** For columns `cx > canvasW/2`, sample from `canvasW - cx + 1` instead. Left half mirrors to right.
- **Vertical:** For rows `cy > canvasH/2`, sample from `canvasH - cy + 1`. Bottom mirrors top.
- **Quad:** Both axes applied simultaneously. Only the top-left quadrant is sampled; all four quadrants are identical.

No change to build order or block placement logic — only `getColor(cx, cy)` is modified.

### 4.3 Edge/Outline Mode

**Location:** Shape & 3D tab, toggle "Edge/Outline Mode" + slider "Edge Threshold" (0–100%, default 30%).

**Algorithm:** Sobel edge detection applied at sample time per pixel. For each canvas position, sample a 3×3 neighborhood of pixels and compute horizontal (Gx) and vertical (Gy) gradient magnitudes. Edge strength = `sqrt(Gx² + Gy²)`.

If edge strength < threshold: skip the block (treat as transparent — apply paintWhite logic).
If edge strength ≥ threshold: place and paint the block normally.

Result: only edges/outlines are built. Produces a sketch/line-art aesthetic. Combine with Depth for raised outlines.

**Performance note:** Sobel requires sampling up to 9 pixels per canvas position. Pre-bake the edge map before building (same pattern as dithering).

### 4.4 Color Filters

**Location:** New "Filters" tab.

**Filters (all applied per sampled pixel before painting):**

| Filter | Range | Default | Effect |
|--------|-------|---------|--------|
| Brightness | -100 to +100 | 0 | Add/subtract to V in HSV |
| Contrast | -100 to +100 | 0 | Scale deviation from 0.5 in V |
| Saturation | -100 to +100 | 0 | Add/subtract to S in HSV |
| Hue Shift | 0 to 360 | 0 | Add to H in HSV (wraps) |

**Implementation:** Convert Color3 → HSV, apply adjustments, clamp, convert back to RGB. Applied inside `getColor()` after sampling but before returning. If all four sliders are at their default (0 / 0 / 0 / 0), skip the HSV conversion entirely and return the sampled color unchanged — no performance cost.

---

## 5. Tab Layout

| Tab | Contents |
|-----|----------|
| **Image** | URL input, scale %, pixel size, max width/height, preview/start/stop buttons, status labels |
| **Build Settings** | Build mode, detail mode (+ Dithering option), extra delay, H/V directions, paint-white toggle, micro-block toggle + warning, co-op section (enable toggle, total workers, worker #, offset label), proxy URL |
| **Shape & 3D** | Shape dropdown, depth settings, mirror mode dropdown, edge/outline toggle + threshold slider |
| **Filters** | Brightness, contrast, saturation, hue shift sliders |
| **Help** | Updated guide covering all v4.0 features |

---

## 6. Build Algorithm (Updated)

```
Setup:
  1. Download + parse image
  2. Apply scale resize if needed
  3. If detail mode == "Dithering": pre-compute ditherMap
  4. If edge mode enabled: pre-compute edgeMap (stores raw gradient magnitude 0–1 per canvas cell; threshold comparison happens at build time)
  5. Compute canvas dimensions (accounting for co-op slice)
  6. Compute totalBlocks for progress tracking

Build loop (Standard / Build-then-Paint):
  for cx = startCol to endCol:
    for cy = 1 to canvasH:
      if shapeMask(cx, cy): skip
      if edgeMode and edgeMap[cy][cx] < threshold: skip
      mirroredCX, mirroredCY = applyMirror(cx, cy)
      color = getColor(mirroredCX, mirroredCY)  -- uses ditherMap or sampleFunc
      color = applyFilters(color)
      place block (normal or small)
      paint block (standard) or queue (build-then-paint)
      build depth column if enabled
```

---

## 7. Error Handling

- Micro-block + large canvas: warn if total blocks > 10,000 before starting
- Dithering + Paint Only: warn and fall back to Average
- Co-op worker # > total workers: clamp and show error
- Edge mode pre-bake: show "Pre-processing edges..." status during Sobel pass
- All existing v3.0 error handling preserved

---

## 8. Files Changed

| File | Change |
|------|--------|
| `PNGLib` | Scanline cache in GetPixel, alpha-weighted Average, new Dither() function |
| `ThinkingFace` | (main script) All new features, new Filters tab, updated build algorithm |

No new files created. PNGLib and ThinkingFace are published to GitHub (pealz1/TCO).
