# Pealie Notes — Design Spec

**Date:** 2026-03-17
**Type:** Windows Desktop Application (Electron)
**Status:** Approved

---

## Overview

Pealie Notes is a professional-grade notes application for Windows, heavily inspired by the iOS Notes app. Built with Electron, it features rich text editing, folder organization, grammar correction, auto-save, themes, smooth animations, and sound effects. The goal is a polished, intuitive experience comparable to iOS Notes or Discord in terms of UI quality.

## Architecture

### Approach: Monolithic Single-Window

One Electron window with a three-panel layout. All state managed in the renderer process with a simple store. Local file storage for notes.

### Tech Stack

- **Framework:** Electron (latest stable)
- **Rich Text Editor:** TipTap (ProseMirror-based)
- **UI:** Vanilla HTML/CSS/JS with CSS custom properties for theming
- **Storage:** Local files — JSON metadata + HTML content per note, organized in folders mirroring the app's folder structure
- **Config:** JSON file in app data directory
- **Sound:** Web Audio API (programmatically generated)
- **Spellcheck:** Electron built-in (Windows system) + custom grammar engine
- **Packaging:** electron-builder for Windows installer + portable

### Data Storage

Notes stored as local files in a user-configurable directory (default: `Documents/PealieNotes/`).

```
PealieNotes/
├── config.json              # App settings
├── notes/
│   ├── <folder-id>/
│   │   ├── folder.json      # Folder metadata (name, color, order)
│   │   └── <note-id>/
│   │       ├── note.json    # Note metadata (title, created, modified, tags, pinned, favorited)
│   │       ├── content.html # Note content (TipTap HTML output)
│   │       └── versions/    # Version history
│   │           ├── v1.html
│   │           └── v2.html
│   └── uncategorized/       # Default folder
├── templates/               # Note templates
│   ├── meeting-notes.json
│   ├── todo-list.json
│   └── journal.json
└── trash/                   # Soft-deleted notes (same structure as notes/)
```

## Layout & Navigation

### Three-Panel Layout

- **Left panel (220px, resizable):** Folder sidebar
  - "All Notes" smart folder at top
  - "Favorites" smart folder
  - User-created folders with nesting support
  - Drag & drop notes between folders
  - Right-click context menu: rename, delete, new subfolder, change color
  - "Trash" at bottom
  - Collapsible via hamburger button for focused writing mode

- **Middle panel (280px, resizable):** Notes list
  - Search bar at top with instant filtering
  - Each note shows: title (first line), preview snippet (second line), last-edited date
  - Sorted by most recent (configurable: date modified, date created, title A-Z, manual)
  - Pinned notes stick to top
  - Favorited notes show a star indicator
  - Tag pills displayed on each note card
  - Right-click context menu: pin, favorite, duplicate, move to folder, trash

- **Right panel (remaining width):** Rich text editor
  - Formatting toolbar at top
  - Clean, distraction-free editing area
  - Status bar at bottom

### Top Bar

- Custom frameless titlebar (Electron menu hidden)
- App logo + "Pealie Notes" on the left
- Window controls (minimize, maximize, close) on the right in platform style
- Theme toggle and settings gear icon

## Rich Text Editor

### Engine: TipTap

Full-featured rich text editing with ProseMirror under the hood.

### Toolbar

| Control | Details |
|---------|---------|
| Text style dropdown | Heading 1, Heading 2, Heading 3, Body, Caption |
| Font family dropdown | Segoe UI, Arial, Georgia, Times New Roman, Courier New, Consolas, Comic Sans MS |
| Font size selector | 8–72pt with presets (10, 12, 14, 16, 18, 24, 30, 36, 48, 72) |
| Text color | Color picker palette with recent colors |
| Highlight color | Color picker palette for text background |
| Bold | Ctrl+B |
| Italic | Ctrl+I |
| Underline | Ctrl+U |
| Strikethrough | Ctrl+Shift+X |
| Alignment | Left, Center, Right, Justify |
| Bullet list | Unordered list |
| Numbered list | Ordered list |
| Checklist | Checkboxes, clickable |
| Indent / Outdent | Tab / Shift+Tab |
| Hyperlink | Ctrl+K — insert/edit URL |
| Insert menu | Horizontal rule, code block, block quote, table, image (from file) |

### Editor Features

- **Markdown shortcuts:** `# ` → H1, `## ` → H2, `- ` → bullet, `1. ` → numbered, `> ` → quote, triple backtick → code block
- **Copy/paste:** Preserves formatting from other apps. Ctrl+Shift+V for plain text paste.
- **Undo/redo:** Full history per note session. Ctrl+Z / Ctrl+Y.
- **Find & replace:** Ctrl+F inline search, Ctrl+H for replace.
- **Drag & drop images:** Drop images from file explorer into notes.

### Status Bar (bottom of editor)

- Word count | Character count | Line count
- "Saved" / "Saving..." indicator with subtle animation
- Cursor position (Ln:Col)
- Zoom level slider (80%–200%)

## Smart Features

### Auto-Save
- Triggers 500ms after user stops typing (debounced)
- Status bar shows "Saving..." → "Saved" with a checkmark fade
- Saves both note content and metadata

### Note Versioning
- Keeps last 10 versions of each note
- New version created on each save (deduplicated — only if content changed)
- History panel accessible from editor toolbar — shows timestamps, can preview and restore

### Pin & Favorite
- Pin: note stays at top of the notes list in its folder
- Favorite: note appears in the "Favorites" smart folder
- Both toggled via right-click or keyboard shortcut

### Tags
- Color-coded tags assigned per note
- Create/manage tags in settings or inline
- Filter notes by tag via sidebar or search

### Templates
- Built-in: Meeting Notes, To-Do List, Journal Entry
- Custom templates: save any note as a template
- Template picker shown when creating a new note (skippable)

### Export & Import
- Export: PDF, HTML, plain text, Markdown
- Import: .txt, .md, .html files as new notes
- Bulk export all notes as a zip

### Search
- Full-text search across all notes, instant results
- Search highlighting in editor when navigating from results
- Filter by: folder, tag, date range

### Quick Note
- Global hotkey: Ctrl+Shift+N
- Creates a new note and brings the app to focus, even when minimized

## Grammar & Spelling

### Windows System Spellcheck
- Electron's built-in spellcheck (uses Windows dictionary)
- Red underlines on misspelled words
- Right-click for suggestions

### Built-in Grammar Engine
- Detects: double words, missing capitalization after periods, basic punctuation errors, common typos
- Subtle blue underlines for grammar issues (distinct from red spell underlines)
- Right-click for suggestions
- "Check Grammar" button in toolbar scans entire note and shows all issues in a side panel

## Themes

### Built-in Themes (10)

| Theme | Description |
|-------|-------------|
| Light | White/gray, iOS-inspired. Default. |
| Dark | Dark grays (#1e1e1e), soft white text. Discord-like. |
| Midnight | True black (#000000), blue accents. OLED-style. |
| Burple | Discord blurple (#5865F2) accents on dark base. |
| Ocean Gradient | Blue-to-teal gradient on sidebar/toolbar. |
| Sunset Gradient | Orange-to-pink gradient accents. |
| Rose | Soft pink tones, light background. |
| Forest | Deep greens and earthy tones. |
| Lavender | Light purple, gentle and calming. |
| Nord | Muted blues and grays. Popular dev aesthetic. |

### Theme System
- All themes as CSS custom properties — swappable at runtime
- Smooth 300ms crossfade transition when switching
- Accent color independently customizable
- Theme preview thumbnails in settings

### Visual Design Language
- Default font: Segoe UI
- Editor font separately configurable
- Line spacing: 1.6 default (adjustable)
- Border radius: 8px on panels, buttons, inputs
- Subtle shadows for depth (layered panels)
- Hover states on all interactive elements (smooth transitions)
- Active/selected states with accent color
- Consistent 4px spacing grid

## Animations

### Transitions
| Element | Animation | Duration |
|---------|-----------|----------|
| Sidebar collapse/expand | Horizontal slide | 250ms ease |
| Page transitions (settings, welcome, main) | Horizontal slide + fade | 300ms ease |
| Note selection | Background fill highlight | 150ms ease |
| Folder expand/collapse | Chevron rotation + children slide in/out | 200ms ease |
| Button hover | Scale to 1.02x + background color fade | 150ms ease |
| Button click | Scale to 0.97x then back | 100ms ease |
| Modal/dialog open | Fade in + scale from 0.95 to 1.0, backdrop blur | 200ms ease |
| Context menu | Fade in + slide down from click point | 150ms ease |
| Toolbar tooltips | Fade in | 400ms delay, 150ms ease |
| Theme switch | Full-app color crossfade | 300ms ease |
| Trash note | Slide out left + fade | 250ms ease |
| New note | Slide in from top of list | 200ms ease |
| Search filter | Fade transition on results | 150ms ease |
| Welcome steps | Crossfade + slide | 400ms ease |

## Sound Effects

All generated programmatically via Web Audio API (no external files).

| Action | Sound | Description |
|--------|-------|-------------|
| Button press / menu select | Soft click | Short sine wave tap |
| Create new note | Gentle pop | Quick rising tone |
| Trash note | Light swoosh | Short noise burst sweep |
| Complete checklist item | Soft ding | Brief high-pitched sine |
| Toggle settings switch | Muted tap | Very short click |

### Sound Settings
- Master toggle (on/off)
- Volume slider (0–100%)
- Individual sound toggles per action

## Settings Menu

Settings panel slides in from the right, replacing the editor.

### Sections

**General**
- Display name
- Default folder for new notes
- Startup behavior: start minimized to tray, open last note on launch
- Language (for spellcheck)

**Appearance**
- Theme picker with visual previews
- Accent color picker
- Font settings (UI font, editor font)
- Editor zoom default
- UI scale

**Editor**
- Default font and size for new notes
- Line spacing
- Spellcheck toggle
- Grammar check toggle
- Auto-save interval (100ms–5000ms)
- Markdown shortcuts toggle
- Show word count toggle
- Show line numbers toggle

**Sounds**
- Master toggle
- Volume slider
- Individual toggles per sound type

**Keyboard Shortcuts**
- Full list of all shortcuts
- Rebindable — click a shortcut, press new keys

**Storage**
- Notes folder location (changeable with folder picker)
- Storage usage display (number of notes, total size)
- Export all notes (zip)
- Import notes

**About**
- App version
- Credits
- Check for updates button

## System Tray

- App minimizes to system tray on close (configurable — can also just close)
- Tray icon with right-click menu: New Quick Note, Show App, Quit
- Global hotkey (Ctrl+Shift+N) works when app is minimized to tray

## Welcome / Onboarding

Shown on first launch (detected by absence of config.json).

1. **Splash (2s):** "Pealie Notes" logo fades in with a subtle glow, app icon centered
2. **Step 1 — Welcome:** "Welcome to Pealie Notes" heading, text input for display name, smooth slide-in transition
3. **Step 2 — Theme:** "Pick your style" — grid of theme preview cards, clicking one applies it live as preview. Smooth transitions between selections.
4. **Step 3 — Highlights:** "Here's what you can do" — 3-4 feature cards (rich formatting, folders & tags, auto-save, keyboard shortcuts) that slide in one by one with icons
5. **Done:** "You're all set, [name]!" — button animates and transitions smoothly into the main app with the first empty note ready

Progress indicator (dots) at the bottom of each step.

## Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| Ctrl+N | New note |
| Ctrl+Shift+N | Quick note (global) |
| Ctrl+Delete | Trash note |
| Ctrl+F | Find in note |
| Ctrl+H | Find and replace |
| Ctrl+Shift+F | Search all notes |
| Ctrl+B | Bold |
| Ctrl+I | Italic |
| Ctrl+U | Underline |
| Ctrl+Shift+X | Strikethrough |
| Ctrl+K | Insert hyperlink |
| Ctrl+Z | Undo |
| Ctrl+Y | Redo |
| Ctrl+S | Force save (though auto-save handles this) |
| Ctrl+, | Open settings |
| Ctrl+Shift+T | Toggle theme (cycle) |
| Ctrl+P | Export as PDF |
| Tab | Indent |
| Shift+Tab | Outdent |
| Ctrl+] | Indent |
| Ctrl+[ | Outdent |
| Ctrl+1/2/3 | Heading 1/2/3 |
| Ctrl+0 | Normal text |

## App Icon & Branding

- Custom app icon: a stylized notepad/pen in the app's accent color (generated as SVG, exported to ICO for Windows)
- App name: "Pealie Notes"
- Window title: "Pealie Notes"
- Installer name: PealieNotes-Setup.exe

## Packaging

- electron-builder for Windows
- NSIS installer (.exe) + portable version (.zip)
- Auto-updater support (for future use)
- App data stored in `%APPDATA%/PealieNotes/`
- Notes stored in user-chosen directory (default: `%USERPROFILE%/Documents/PealieNotes/`)
