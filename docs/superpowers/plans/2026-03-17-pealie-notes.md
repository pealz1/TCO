# Pealie Notes Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a professional-grade Windows notes app (Electron) inspired by iOS Notes, with rich text editing, folder organization, themes, animations, sound effects, grammar checking, and auto-save.

**Architecture:** Single Electron window with three-panel layout (folder sidebar | notes list | editor). Vanilla HTML/CSS/JS renderer with TipTap for rich text editing. Local file storage (JSON + HTML per note). CSS custom properties for theming. Web Audio API for sounds.

**Tech Stack:** Electron 28+, TipTap 2.x (with extensions: StarterKit, Underline, TextAlign, TextStyle, Color, Highlight, FontFamily, Link, Image, TaskList, Table, CodeBlock, Placeholder), electron-builder, Vitest (for testable modules)

**Module strategy:** Use a simple bundler (esbuild) to bundle renderer JS into a single file. TipTap is ESM-only and has many small packages — bundling avoids module resolution issues in Electron's renderer. The main process and preload remain unbundled (Node.js CJS). Add an `"build:renderer": "esbuild src/renderer/app.js --bundle --outfile=src/renderer/bundle.js --format=iife --platform=browser"` script and load `bundle.js` from index.html.

**Custom extensions note:** TipTap does not ship a FontSize extension. Task 14 requires writing a small custom extension (~20 lines) that adds a `fontSize` attribute to the TextStyle mark.

---

## File Structure

```
PealieNotes/                          # Project root (C:\Users\pealz\.local\bin\PealieNotes)
├── package.json                      # Dependencies, scripts, electron-builder config
├── electron-builder.yml              # Build configuration
├── vitest.config.js                  # Test configuration
├── src/
│   ├── main/
│   │   ├── main.js                   # Electron main process entry point
│   │   ├── tray.js                   # System tray icon and menu
│   │   └── globalShortcuts.js        # OS-level global hotkeys
│   ├── preload/
│   │   └── preload.js                # Electron preload script (IPC bridge)
│   ├── renderer/
│   │   ├── index.html                # Single HTML entry point
│   │   ├── app.js                    # App init, view routing, event bus
│   │   ├── store.js                  # Centralized reactive state
│   │   ├── storage.js                # File system operations via IPC
│   │   ├── sounds.js                 # Web Audio API sound generator
│   │   ├── grammar.js                # Built-in grammar checking engine
│   │   ├── themes.js                 # Theme loading and switching
│   │   ├── shortcuts.js              # In-app keyboard shortcut manager
│   │   ├── styles/
│   │   │   ├── main.css              # Reset, layout grid, base styles, scrollbar
│   │   │   ├── themes.css            # All 10 theme definitions as CSS custom props
│   │   │   ├── animations.css        # All animation keyframes and transition classes
│   │   │   ├── titlebar.css          # Custom titlebar styles
│   │   │   ├── sidebar.css           # Folder sidebar styles
│   │   │   ├── notesList.css         # Notes list panel styles
│   │   │   ├── editor.css            # TipTap editor and toolbar styles
│   │   │   ├── statusBar.css         # Status bar styles
│   │   │   ├── settings.css          # Settings panel styles
│   │   │   ├── welcome.css           # Onboarding flow styles
│   │   │   ├── contextMenu.css       # Context menu styles
│   │   │   └── modal.css             # Modal dialog styles
│   │   ├── components/
│   │   │   ├── titlebar.js           # Custom frameless titlebar with window controls
│   │   │   ├── sidebar.js            # Folder tree, smart folders, drag targets
│   │   │   ├── notesList.js          # Sorted/filtered note cards, search
│   │   │   ├── editor.js             # TipTap editor init, content loading/saving
│   │   │   ├── toolbar.js            # Formatting toolbar (dropdowns, buttons, pickers)
│   │   │   ├── statusBar.js          # Word count, save indicator, zoom slider
│   │   │   ├── settings.js           # Settings panel with all sections
│   │   │   ├── welcome.js            # Onboarding flow (splash → steps → done)
│   │   │   ├── contextMenu.js        # Right-click context menu system
│   │   │   ├── modal.js              # Reusable modal dialogs
│   │   │   ├── search.js             # Full-text search across all notes
│   │   │   ├── templates.js          # Template picker and management
│   │   │   ├── findReplace.js        # Find & replace bar in editor
│   │   │   ├── versionHistory.js     # Note version history panel
│   │   │   └── grammarPanel.js       # Grammar check results panel
│   │   └── utils/
│   │       ├── ids.js                # UUID generation
│   │       ├── debounce.js           # Debounce/throttle utilities
│   │       ├── dom.js                # DOM helper (createElement, etc.)
│   │       └── format.js             # Date/number formatting
│   └── assets/
│       ├── icon.svg                  # App icon source (SVG)
│       └── icon.ico                  # Windows icon (generated from SVG)
├── tests/
│   ├── storage.test.js               # Storage operations tests
│   ├── grammar.test.js               # Grammar engine tests
│   ├── store.test.js                 # State management tests
│   ├── sounds.test.js                # Sound generation tests
│   └── utils.test.js                 # Utility function tests
```

---

## Task 1: Project Scaffolding & Electron Shell

**Files:**
- Create: `PealieNotes/package.json`
- Create: `PealieNotes/src/main/main.js`
- Create: `PealieNotes/src/preload/preload.js`
- Create: `PealieNotes/src/renderer/index.html`
- Create: `PealieNotes/src/renderer/app.js`

**Dependencies:** electron, @electron/remote, tiptap core + extensions, electron-builder, vitest

- [ ] **Step 1: Create project directory and package.json**

```json
{
  "name": "pealie-notes",
  "version": "1.0.0",
  "description": "A professional-grade notes app for Windows",
  "main": "src/main/main.js",
  "scripts": {
    "start": "electron .",
    "test": "vitest run",
    "test:watch": "vitest",
    "build": "electron-builder --win",
    "pack": "electron-builder --dir"
  },
  "devDependencies": {
    "electron": "^28.0.0",
    "electron-builder": "^24.9.0",
    "vitest": "^1.0.0"
  },
  "dependencies": {
    "@tiptap/core": "^2.1.0",
    "@tiptap/starter-kit": "^2.1.0",
    "@tiptap/extension-underline": "^2.1.0",
    "@tiptap/extension-text-align": "^2.1.0",
    "@tiptap/extension-text-style": "^2.1.0",
    "@tiptap/extension-color": "^2.1.0",
    "@tiptap/extension-highlight": "^2.1.0",
    "@tiptap/extension-font-family": "^2.1.0",
    "@tiptap/extension-link": "^2.1.0",
    "@tiptap/extension-image": "^2.1.0",
    "@tiptap/extension-task-list": "^2.1.0",
    "@tiptap/extension-task-item": "^2.1.0",
    "@tiptap/extension-table": "^2.1.0",
    "@tiptap/extension-table-row": "^2.1.0",
    "@tiptap/extension-table-cell": "^2.1.0",
    "@tiptap/extension-table-header": "^2.1.0",
    "@tiptap/extension-code-block-lowlight": "^2.1.0",
    "@tiptap/extension-placeholder": "^2.1.0",
    "@tiptap/extension-character-count": "^2.1.0",
    "@tiptap/pm": "^2.1.0"
  },
  "build": {
    "appId": "com.pealie.notes",
    "productName": "Pealie Notes",
    "win": {
      "target": ["nsis", "portable"],
      "icon": "src/assets/icon.ico"
    },
    "nsis": {
      "oneClick": false,
      "allowToChangeInstallationDirectory": true
    },
    "files": ["src/**/*", "node_modules/**/*"],
    "directories": {
      "output": "dist"
    }
  }
}
```

- [ ] **Step 2: Create Electron main process**

Create `src/main/main.js` — frameless window, 1200x800 default, min 800x600, preload script, single instance lock, IPC handlers for file operations.

- [ ] **Step 3: Create preload script**

Create `src/preload/preload.js` — exposes safe IPC bridge (`window.api`) for: readFile, writeFile, readDir, mkdir, deleteFile, moveFile, showOpenDialog, showSaveDialog, getAppDataPath, getDocumentsPath, minimize, maximize, close, isMaximized, onMaximizeChange.

- [ ] **Step 4: Create index.html**

Create `src/renderer/index.html` — single HTML file with all CSS links, script imports, and the DOM skeleton: `#app` container with `#titlebar`, `#main-content` (containing `#sidebar`, `#notes-list`, `#editor-panel`), `#welcome-screen`, `#settings-panel`.

- [ ] **Step 5: Create app.js entry point**

Create `src/renderer/app.js` — initializes the app: checks for config.json (first launch → show welcome, else → show main), imports and initializes all components, sets up the event bus (simple EventTarget subclass for inter-component communication).

- [ ] **Step 6: Install dependencies and verify the app launches**

Run: `cd PealieNotes && npm install && npm start`
Expected: A blank frameless Electron window appears with the DOM skeleton visible.

- [ ] **Step 7: Commit**

```bash
git add PealieNotes/
git commit -m "feat: scaffold Pealie Notes Electron project with main process, preload, and renderer skeleton"
```

---

## Task 2: Utility Modules

**Files:**
- Create: `PealieNotes/src/renderer/utils/ids.js`
- Create: `PealieNotes/src/renderer/utils/debounce.js`
- Create: `PealieNotes/src/renderer/utils/dom.js`
- Create: `PealieNotes/src/renderer/utils/format.js`
- Create: `PealieNotes/vitest.config.js`
- Create: `PealieNotes/tests/utils.test.js`

- [ ] **Step 1: Write tests for utility functions**

Test `generateId()` returns a valid UUID-like string, `debounce()` delays execution, `formatDate()` formats dates, `formatFileSize()` formats bytes, `createElement()` creates DOM elements with classes/attributes.

- [ ] **Step 2: Run tests to verify they fail**

Run: `cd PealieNotes && npx vitest run tests/utils.test.js`
Expected: FAIL — modules don't exist yet.

- [ ] **Step 3: Implement utility modules**

- `ids.js`: `generateId()` using `crypto.randomUUID()`
- `debounce.js`: `debounce(fn, ms)` and `throttle(fn, ms)`
- `dom.js`: `createElement(tag, { className, attrs, children, text, onClick })` helper
- `format.js`: `formatDate(date)` (relative: "Just now", "5 min ago", "Yesterday", then date), `formatFileSize(bytes)`, `truncate(str, len)`

- [ ] **Step 4: Run tests to verify they pass**

Run: `cd PealieNotes && npx vitest run tests/utils.test.js`
Expected: All PASS.

- [ ] **Step 5: Commit**

```bash
git add PealieNotes/src/renderer/utils/ PealieNotes/tests/utils.test.js PealieNotes/vitest.config.js
git commit -m "feat: add utility modules (ids, debounce, dom helpers, formatters) with tests"
```

---

## Task 3: State Management (Store)

**Files:**
- Create: `PealieNotes/src/renderer/store.js`
- Create: `PealieNotes/tests/store.test.js`

- [ ] **Step 1: Write store tests**

Test: initial state shape, `store.set('key', value)` updates state and fires listeners, `store.get('key')` retrieves values, `store.on('key', callback)` subscribes to changes, `store.off('key', callback)` unsubscribes.

- [ ] **Step 2: Run tests to verify they fail**

Run: `cd PealieNotes && npx vitest run tests/store.test.js`

- [ ] **Step 3: Implement store**

Simple reactive store using a Map + listener pattern. Initial state includes:
- `folders`: array of folder objects
- `notes`: array of note metadata objects
- `activeFolder`: current folder ID or 'all'/'favorites'/'trash'
- `activeNote`: current note ID
- `config`: app configuration object
- `view`: 'main' | 'settings' | 'welcome'
- `searchQuery`: current search string
- `sidebarCollapsed`: boolean
- `theme`: current theme name

- [ ] **Step 4: Run tests to verify they pass**

- [ ] **Step 5: Commit**

```bash
git add PealieNotes/src/renderer/store.js PealieNotes/tests/store.test.js
git commit -m "feat: add reactive state store with subscription system"
```

---

## Task 4: Storage Layer (File Operations)

**Files:**
- Create: `PealieNotes/src/renderer/storage.js`
- Create: `PealieNotes/tests/storage.test.js`

The storage module talks to the main process via the preload IPC bridge to perform all file operations.

- [ ] **Step 1: Write storage tests**

Test (with mocked `window.api`):
- `initStorage(basePath)` creates directory structure if missing
- `loadFolders()` reads all folder.json files and returns sorted array
- `loadNotes(folderId)` reads all note.json files in a folder
- `loadAllNotes()` loads notes from all folders
- `createFolder(name, parentId)` creates folder directory + folder.json
- `renameFolder(id, name)` updates folder.json
- `deleteFolder(id)` moves folder to trash
- `createNote(folderId)` creates note directory with note.json + empty content.html
- `saveNoteContent(noteId, html)` writes content.html + creates version if changed
- `saveNoteMetadata(noteId, metadata)` writes note.json
- `trashNote(noteId)` moves note directory to trash/
- `restoreNote(noteId)` moves note back from trash/
- `permanentlyDelete(noteId)` removes note directory
- `loadNoteContent(noteId)` reads content.html
- `loadVersions(noteId)` lists version files
- `restoreVersion(noteId, versionFile)` copies version to content.html
- `pruneVersions(noteId)` keeps only last 10 versions
- `duplicateNote(noteId, targetFolderId)` copies note to another folder
- `moveNote(noteId, targetFolderId)` moves note directory
- `loadConfig()` reads config.json, returns defaults if missing
- `saveConfig(config)` writes config.json
- `searchNotes(query)` searches content.html files for text matches
- `getStorageStats()` returns note count and total size
- `exportNote(noteId, format)` generates PDF/HTML/TXT/MD
- `importFile(filePath)` creates a note from an imported file

- [ ] **Step 2: Run tests to verify they fail**

- [ ] **Step 3: Implement storage.js**

All file operations go through `window.api` (IPC to main process). Uses the folder structure from the spec. Note IDs and folder IDs are UUIDs. The "uncategorized" folder is created on init with a fixed ID.

Key implementation details:
- `saveNoteContent` compares new HTML with current content.html; if different, copies current to `versions/v{timestamp}.html` before writing new content, then calls `pruneVersions`
- `pruneVersions` lists version files sorted by timestamp, deletes all but the 10 most recent
- `searchNotes` reads each content.html and does case-insensitive text search (strip HTML tags first)
- `exportNote` for PDF uses Electron's `webContents.printToPDF()` via IPC

- [ ] **Step 4: Run tests to verify they pass**

- [ ] **Step 5: Commit**

```bash
git add PealieNotes/src/renderer/storage.js PealieNotes/tests/storage.test.js
git commit -m "feat: add storage layer for notes, folders, config, versioning, and search"
```

---

## Task 5: Theme System

**Files:**
- Create: `PealieNotes/src/renderer/styles/themes.css`
- Create: `PealieNotes/src/renderer/themes.js`

- [ ] **Step 1: Create themes.css with all 10 theme definitions**

Each theme defined as a `[data-theme="name"]` attribute selector on `:root`, setting CSS custom properties:
- `--bg-primary`, `--bg-secondary`, `--bg-tertiary` (panel backgrounds)
- `--bg-hover`, `--bg-active` (interactive states)
- `--text-primary`, `--text-secondary`, `--text-muted` (text colors)
- `--accent`, `--accent-hover`, `--accent-text` (accent/brand color)
- `--border`, `--border-light` (borders)
- `--shadow` (box shadows)
- `--sidebar-bg`, `--noteslist-bg`, `--editor-bg` (panel-specific)
- `--toolbar-bg`, `--statusbar-bg` (component-specific)
- `--scrollbar-thumb`, `--scrollbar-track`
- Gradient themes use `--sidebar-gradient`, `--toolbar-gradient` for background-image

All 10 themes: Light, Dark, Midnight, Burple, Ocean Gradient, Sunset Gradient, Rose, Forest, Lavender, Nord — with carefully chosen color palettes for each.

Add a `* { transition: background-color 300ms ease, color 300ms ease, border-color 300ms ease; }` rule for smooth theme transitions.

- [ ] **Step 2: Create themes.js**

```javascript
export function setTheme(name) { document.documentElement.setAttribute('data-theme', name); }
export function setAccentColor(color) { document.documentElement.style.setProperty('--accent', color); }
export function getThemeList() { /* returns array of { id, name, description, preview colors } */ }
```

- [ ] **Step 3: Verify theme switching works**

Run app, open devtools, execute `setTheme('dark')`, `setTheme('burple')` etc. Verify smooth color transitions.

- [ ] **Step 4: Commit**

```bash
git add PealieNotes/src/renderer/styles/themes.css PealieNotes/src/renderer/themes.js
git commit -m "feat: add 10-theme system with CSS custom properties and smooth transitions"
```

---

## Task 6: Base Styles & Layout

**Files:**
- Create: `PealieNotes/src/renderer/styles/main.css`
- Create: `PealieNotes/src/renderer/styles/animations.css`

- [ ] **Step 1: Create main.css**

- CSS reset (margin, padding, box-sizing)
- Custom scrollbar styles (thin, themed)
- `#app` full viewport, flex column
- `#main-content` flex row, flex:1
- `#sidebar` 220px width, resizable, flex-shrink:0
- `#notes-list` 280px width, resizable, flex-shrink:0
- `#editor-panel` flex:1, flex column (toolbar + editor + statusbar)
- Resize handles between panels (4px draggable dividers)
- Selection colors using accent
- Focus ring styles using accent
- Typography: Segoe UI, line-height 1.6, 4px grid
- Border-radius: 8px on containers, 6px on buttons/inputs
- Subtle shadows on panels

- [ ] **Step 2: Create animations.css**

All animation keyframes and utility classes:
- `@keyframes fadeIn`, `fadeOut`, `slideInLeft`, `slideOutLeft`, `slideInRight`, `slideOutRight`, `slideInTop`, `slideInBottom`, `scaleIn`, `scaleOut`, `pulse`
- `.animate-fade-in`, `.animate-slide-in-left`, etc.
- Button hover: `transform: scale(1.02)` with 150ms transition
- Button active: `transform: scale(0.97)` with 100ms transition
- `.sidebar-collapse` transition for sidebar width
- `.note-enter` (slide from top), `.note-exit` (slide out left)
- `.modal-backdrop` fade in + backdrop-filter blur
- `.modal-content` scale from 0.95 + fade
- `.context-menu` fade + translateY
- `.tooltip` fade in with delay
- Theme transition already in themes.css

- [ ] **Step 3: Update index.html to include all stylesheets**

- [ ] **Step 4: Verify layout renders correctly**

Run: `npm start` — three panels should be visible with correct proportions, scrollbars themed.

- [ ] **Step 5: Commit**

```bash
git add PealieNotes/src/renderer/styles/main.css PealieNotes/src/renderer/styles/animations.css PealieNotes/src/renderer/index.html
git commit -m "feat: add base layout styles, animations, and CSS architecture"
```

---

## Task 7: Sound System

**Files:**
- Create: `PealieNotes/src/renderer/sounds.js`
- Create: `PealieNotes/tests/sounds.test.js`

- [ ] **Step 1: Write sound system tests**

Test: `SoundEngine` creates AudioContext lazily, `play('click')` generates correct waveform, respects `enabled` and `volume` settings, individual sound toggles work.

- [ ] **Step 2: Implement sounds.js**

```javascript
class SoundEngine {
  constructor() { this.ctx = null; this.enabled = true; this.volume = 0.3; this.toggles = {}; }
  init() { this.ctx = new AudioContext(); }
  play(name) { /* generate sound based on name */ }
}
```

Sound definitions (all programmatic, no files):
- `click`: 1000Hz sine, 30ms duration, quick envelope attack/decay
- `pop`: 600→1200Hz sine sweep, 80ms, moderate envelope
- `swoosh`: white noise filtered through bandpass, 150ms, sweep from 2000→500Hz
- `ding`: 1400Hz sine, 200ms, slow decay
- `tap`: 800Hz sine, 15ms, very quick

Each sound is a function that creates oscillator/noise nodes, connects through a gain node (volume), and schedules start/stop.

- [ ] **Step 3: Run tests**

- [ ] **Step 4: Verify sounds play in the app**

Add a temporary button in index.html that calls each sound. Listen to verify they're subtle and pleasant.

- [ ] **Step 5: Commit**

```bash
git add PealieNotes/src/renderer/sounds.js PealieNotes/tests/sounds.test.js
git commit -m "feat: add Web Audio API sound engine with 5 programmatic sounds"
```

---

## Task 8: Custom Titlebar

**Files:**
- Create: `PealieNotes/src/renderer/components/titlebar.js`
- Create: `PealieNotes/src/renderer/styles/titlebar.css`

- [ ] **Step 1: Create titlebar.css**

- Fixed at top, 36px height, flex row, `-webkit-app-region: drag`
- Left: logo icon + "Pealie Notes" text
- Right: theme cycle button, settings gear, window controls (min/max/close)
- Window controls: `-webkit-app-region: no-drag`, hover states (close button turns red)
- Buttons are 46x36px hit targets, centered icons
- Uses theme variables for all colors

- [ ] **Step 2: Create titlebar.js**

Component that renders the titlebar DOM, hooks up:
- Minimize/maximize/close buttons via `window.api`
- Double-click to maximize/restore
- Settings gear click → `store.set('view', 'settings')`
- Theme cycle button → cycles through themes
- Maximize state tracking (updates button icon)

- [ ] **Step 3: Verify titlebar works**

Run app — titlebar renders, window controls work, drag to move.

- [ ] **Step 4: Commit**

```bash
git add PealieNotes/src/renderer/components/titlebar.js PealieNotes/src/renderer/styles/titlebar.css
git commit -m "feat: add custom frameless titlebar with window controls and theme toggle"
```

---

## Task 9: Context Menu System

**Files:**
- Create: `PealieNotes/src/renderer/components/contextMenu.js`
- Create: `PealieNotes/src/renderer/styles/contextMenu.css`

- [ ] **Step 1: Create contextMenu.css**

- Positioned absolutely, z-index 1000
- Themed background with subtle shadow and 8px border-radius
- Menu items: 32px height, padding 8px 16px, hover background
- Separator: 1px line with margin
- Fade-in + slide-down animation from click point (150ms)
- Sub-menu support (arrow indicator, slides out to the right)

- [ ] **Step 2: Create contextMenu.js**

```javascript
export function showContextMenu(x, y, items) { ... }
// items: [{ label, icon?, action, separator?, disabled?, submenu? }]
```

- Creates menu DOM at click position, adjusts if near edge of window
- Click outside or Escape dismisses
- Plays `click` sound on item selection
- Returns a promise that resolves with the selected action

- [ ] **Step 3: Verify context menus work**

Right-click in the app — context menu appears with animation at click position, dismisses correctly.

- [ ] **Step 4: Commit**

```bash
git add PealieNotes/src/renderer/components/contextMenu.js PealieNotes/src/renderer/styles/contextMenu.css
git commit -m "feat: add animated context menu system"
```

---

## Task 10: Modal Dialog System

**Files:**
- Create: `PealieNotes/src/renderer/components/modal.js`
- Create: `PealieNotes/src/renderer/styles/modal.css`

- [ ] **Step 1: Create modal.css**

- Full-screen backdrop with `backdrop-filter: blur(4px)`, semi-transparent dark overlay
- Modal content: centered, max-width 480px, themed background, 12px border-radius, shadow
- Fade-in + scale-in animation (200ms)
- Header with title + close button
- Body with padding
- Footer with action buttons (primary accent, secondary outline)
- Close animation: fade-out + scale-out

- [ ] **Step 2: Create modal.js**

```javascript
export function showModal({ title, body, buttons }) { ... }
// Returns promise resolving to clicked button id
export function showConfirm(message) { ... }
// Shorthand for OK/Cancel modal
export function showPrompt(message, defaultValue) { ... }
// Modal with text input
```

- Escape key and backdrop click dismiss (unless modal is marked `persistent`)
- Plays `click` sound on button press
- Traps focus within modal while open

- [ ] **Step 3: Verify modals work**

Test all three modal types from devtools console.

- [ ] **Step 4: Commit**

```bash
git add PealieNotes/src/renderer/components/modal.js PealieNotes/src/renderer/styles/modal.css
git commit -m "feat: add modal dialog system with confirm and prompt variants"
```

---

## Task 11: Folder Sidebar

**Files:**
- Create: `PealieNotes/src/renderer/components/sidebar.js`
- Create: `PealieNotes/src/renderer/styles/sidebar.css`

- [ ] **Step 1: Create sidebar.css**

- Full height, flex column, themed sidebar background
- Header: hamburger toggle button + "Folders" label
- Smart folders section: "All Notes" (icon: notes stack), "Favorites" (icon: star) — always visible at top
- Folder tree: indented nested structure
  - Each folder row: 36px height, expand chevron (rotates on open), folder icon, name, note count badge
  - Hover: background highlight
  - Active: accent background
  - Drag target highlighting (dashed border when note dragged over)
- "New Folder" button at bottom of tree (+ icon)
- "Trash" item at very bottom with trash icon
- Collapse animation: width transitions from 220px to 0 with opacity fade (250ms)
- Folder children slide in/out on expand/collapse (200ms)

- [ ] **Step 2: Create sidebar.js**

Component that:
- Renders folder tree from `store.get('folders')`
- Smart folders ("All Notes", "Favorites") at top — click to filter notes list
- User folders with expand/collapse (saves state)
- Click folder → `store.set('activeFolder', folderId)` → notes list updates
- Right-click folder → context menu: New Subfolder, Rename, Change Color, Delete
- Rename: inline edit (double-click or context menu)
- New folder: prompt modal for name, creates via storage
- Delete folder: confirm modal, moves to trash
- "Trash" shows trashed notes in the notes list
- Folder color: small colored dot before folder name
- Drag & drop: folders accept note drops (highlight on dragover)
- Hamburger button toggles `store.set('sidebarCollapsed', true/false)` with animation
- Subscribes to store changes for reactivity

- [ ] **Step 3: Verify sidebar renders and interacts correctly**

Run app — folder tree visible with smart folders, can create/rename/delete folders, collapse works.

- [ ] **Step 4: Commit**

```bash
git add PealieNotes/src/renderer/components/sidebar.js PealieNotes/src/renderer/styles/sidebar.css
git commit -m "feat: add folder sidebar with tree navigation, smart folders, and context menus"
```

---

## Task 12: Notes List Panel

**Files:**
- Create: `PealieNotes/src/renderer/components/notesList.js`
- Create: `PealieNotes/src/renderer/styles/notesList.css`

- [ ] **Step 1: Create notesList.css**

- Full height, flex column
- Search bar at top: 40px height, rounded input with search icon, themed
- Sort dropdown: small dropdown in header area
- Notes list: scrollable, flex column
- Note card: padding 12px 16px, flex column
  - Title: bold, single line, truncated
  - Preview: secondary text, 1-2 lines, truncated
  - Date: muted small text
  - Pin icon (top-right if pinned)
  - Star icon (if favorited)
  - Tag pills: small colored rounded badges
- Note card hover: background highlight (150ms)
- Note card active/selected: accent left border + subtle accent background
- New note enter animation: slideInTop (200ms)
- Trash note exit animation: slideOutLeft + fade (250ms)
- Empty state: centered message "No notes yet" with icon
- Drag: note card becomes draggable, shows ghost

- [ ] **Step 2: Create notesList.js**

Component that:
- Renders note cards from `store.get('notes')` filtered by `activeFolder`
- Search bar with instant filtering (debounced 150ms): searches title + content
- Sort selector: Date Modified, Date Created, Title A-Z, Manual
- Pinned notes always at top within sort
- Click note → `store.set('activeNote', noteId)` → editor loads content
- Double-click note title → inline rename
- Right-click note → context menu: Pin/Unpin, Favorite/Unfavorite, Duplicate, Move to..., Add Tag, Trash
- "New Note" button in header (+ icon) — creates note in active folder
- Drag notes to sidebar folders
- Shows "Favorites" or "All Notes" filtered views for smart folders
- Trash view: shows restore and permanent delete options
- Subscribes to store changes for reactivity

- [ ] **Step 3: Verify notes list works**

Run app — can create notes, see them in the list, select them, search/sort, context menu works.

- [ ] **Step 4: Commit**

```bash
git add PealieNotes/src/renderer/components/notesList.js PealieNotes/src/renderer/styles/notesList.css
git commit -m "feat: add notes list panel with search, sort, pin, favorites, and drag-drop"
```

---

## Task 13: TipTap Editor Core

**Files:**
- Create: `PealieNotes/src/renderer/components/editor.js`
- Create: `PealieNotes/src/renderer/styles/editor.css`

- [ ] **Step 1: Create editor.css**

- Full flex:1 area, flex column
- `.editor-container`: flex:1, overflow auto, padding 24px 40px (generous margins like iOS Notes)
- TipTap ProseMirror overrides:
  - `.ProseMirror`: outline none, min-height 100%, font per config
  - Headings: H1 28px bold, H2 22px bold, H3 18px semibold
  - Paragraphs: 16px, line-height 1.6
  - Links: accent colored, underline on hover
  - Code blocks: themed background, monospace, rounded, padding
  - Block quotes: left border accent, italic, padding-left
  - Tables: bordered, alternating row backgrounds
  - Task lists: custom checkbox styling with accent color, strikethrough on checked
  - Images: max-width 100%, rounded corners, click to resize
  - Horizontal rules: themed border
  - Placeholder text: muted color "Start writing..."

- [ ] **Step 2: Create editor.js**

Component that:
- Initializes TipTap editor with all extensions:
  - StarterKit (bold, italic, strike, code, heading, bulletList, orderedList, blockquote, codeBlock, horizontalRule, history)
  - Underline
  - TextAlign (left, center, right, justify)
  - TextStyle + Color + FontFamily (for text formatting)
  - Highlight (with multicolor support)
  - Link (autolink, openOnClick)
  - Image (inline, allowBase64)
  - TaskList + TaskItem (with nested support)
  - Table + TableRow + TableCell + TableHeader
  - Placeholder ("Start writing...")
  - CharacterCount
- `loadNote(noteId)` — reads content.html via storage, sets editor content
- `saveNote()` — gets editor HTML, saves via storage, updates metadata (title from first line)
- Auto-save: debounced 500ms after any editor update
- Editor update events → update status bar (word count, etc.)
- Focus management: focuses editor when note selected
- Handles image drops: saves to note's `assets/` folder, inserts as relative path
- Markdown input rules enabled (# for heading, - for list, etc.)

- [ ] **Step 3: Verify editor loads and saves content**

Run app — create a note, type text, formatting renders, switch notes, content persists.

- [ ] **Step 4: Commit**

```bash
git add PealieNotes/src/renderer/components/editor.js PealieNotes/src/renderer/styles/editor.css
git commit -m "feat: add TipTap rich text editor with all formatting extensions and auto-save"
```

---

## Task 14: Editor Toolbar

**Files:**
- Create: `PealieNotes/src/renderer/components/toolbar.js`
- Create: (styles already in editor.css, but add toolbar-specific section)

- [ ] **Step 1: Add toolbar styles to editor.css**

- Toolbar: 44px height, flex row, gap 2px, padding 4px 8px, themed background, border-bottom
- Button groups separated by thin vertical dividers
- Toolbar buttons: 32x32px, rounded 6px, icon centered, themed
  - Hover: scale 1.02, background highlight
  - Active (format applied): accent background, accent text
  - Click: scale 0.97 spring
- Dropdowns (text style, font, size): styled select-like buttons that open custom dropdown panels
  - Dropdown panel: themed background, shadow, rounded, max-height 300px scroll
  - Dropdown items: hover highlight, checkmark for active
- Color picker: grid of color swatches (16 preset colors + "More..." for custom), recent colors row at top
- Tooltip on each button (400ms delay)

- [ ] **Step 2: Create toolbar.js**

Component that renders the toolbar with these groups (left to right):
1. **Text style dropdown**: Heading 1/2/3, Body, Caption — applies heading level or paragraph
2. **Font family dropdown**: 7 fonts — applies fontFamily mark
3. **Font size**: number input with dropdown presets — applies fontSize via TextStyle
4. **Divider**
5. **Bold (B)**, **Italic (I)**, **Underline (U)**, **Strikethrough (S)** — toggle buttons
6. **Text color** (A with colored underline) — opens color picker
7. **Highlight color** (marker icon) — opens color picker
8. **Divider**
9. **Alignment**: Left, Center, Right, Justify — radio group
10. **Divider**
11. **Bullet list**, **Numbered list**, **Checklist** — toggle buttons
12. **Indent**, **Outdent** — action buttons
13. **Divider**
14. **Link** (chain icon) — opens prompt for URL (Ctrl+K)
15. **Insert menu** (+ icon) — dropdown: Horizontal rule, Code block, Block quote, Table (3x3), Image

Each button:
- Listens to editor `selectionUpdate` and `transaction` events to update active states
- Calls corresponding TipTap commands on click
- Plays `click` sound on press
- Shows tooltip on hover (400ms delay)

Font size implementation: custom TipTap extension `FontSize` that adds a `fontSize` attribute to TextStyle mark.

- [ ] **Step 3: Verify all toolbar buttons work**

Run app — test every formatting option, verify active states update when cursor moves through formatted text.

- [ ] **Step 4: Commit**

```bash
git add PealieNotes/src/renderer/components/toolbar.js PealieNotes/src/renderer/styles/editor.css
git commit -m "feat: add editor toolbar with all formatting controls, dropdowns, and color pickers"
```

---

## Task 15: Status Bar

**Files:**
- Create: `PealieNotes/src/renderer/components/statusBar.js`
- Create: `PealieNotes/src/renderer/styles/statusBar.css`

- [ ] **Step 1: Create statusBar.css**

- 28px height, flex row, padding 0 12px, themed background, border-top
- Left section: word count | char count | line count (muted text, small font)
- Center: save indicator ("Saved ✓" fades in, "Saving..." with animated dots)
- Right section: cursor position "Ln X, Col Y" | zoom slider (range input 80–200%)
- Save indicator animation: text fades between states

- [ ] **Step 2: Create statusBar.js**

Component that:
- Subscribes to editor updates via event bus for word/char/line counts (from CharacterCount extension)
- Tracks cursor position from editor `selectionUpdate`
- Shows save state: "Saving..." during debounce, "Saved" after successful save (with animated checkmark that fades after 2s)
- Zoom slider: adjusts editor container CSS transform scale, saved to config

- [ ] **Step 3: Verify status bar updates in real time**

Run app — type text, watch counts update, save indicator transitions, zoom slider works.

- [ ] **Step 4: Commit**

```bash
git add PealieNotes/src/renderer/components/statusBar.js PealieNotes/src/renderer/styles/statusBar.css
git commit -m "feat: add editor status bar with word count, save indicator, and zoom"
```

---

## Task 16: Find & Replace

**Files:**
- Create: `PealieNotes/src/renderer/components/findReplace.js`

- [ ] **Step 1: Create findReplace.js**

- Ctrl+F opens a floating bar at top of editor (slides down, 44px)
- Search input + result count ("3 of 12") + up/down arrows + close button
- Ctrl+H expands to show replace input + Replace / Replace All buttons
- Uses TipTap's search-and-replace or ProseMirror decorations to highlight matches
- Navigate between matches with Enter / Shift+Enter or arrow buttons
- Escape closes the bar (slides up)
- Case-sensitive toggle button
- Styled in editor.css (add section)

- [ ] **Step 2: Verify find and replace works**

Run app — Ctrl+F opens find bar, matches highlighted, replace works.

- [ ] **Step 3: Commit**

```bash
git add PealieNotes/src/renderer/components/findReplace.js
git commit -m "feat: add find and replace with highlighting and keyboard navigation"
```

---

## Task 17: Grammar Engine

**Files:**
- Create: `PealieNotes/src/renderer/grammar.js`
- Create: `PealieNotes/src/renderer/components/grammarPanel.js`
- Create: `PealieNotes/tests/grammar.test.js`

- [ ] **Step 1: Write grammar engine tests**

Test cases:
- Detects double words: "the the" → suggests "the"
- Detects missing capitalization after period: "hello. world" → "hello. World"
- Detects missing capitalization at start of text
- Detects double spaces
- Detects common typos: "teh" → "the", "recieve" → "receive", etc. (dictionary of ~50 common typos)
- Detects missing period at end of sentence (before newline)
- Returns array of `{ offset, length, message, suggestions, type }` for each issue

- [ ] **Step 2: Run tests to verify they fail**

- [ ] **Step 3: Implement grammar.js**

```javascript
export function checkGrammar(text) {
  const issues = [];
  // Run each rule:
  checkDoubleWords(text, issues);
  checkCapitalization(text, issues);
  checkDoubleSpaces(text, issues);
  checkCommonTypos(text, issues);
  return issues;
}
```

Each checker function scans text with regex, pushes issues with offset/length/message/suggestions.

- [ ] **Step 4: Run tests to verify they pass**

- [ ] **Step 5: Create grammarPanel.js**

- "Check Grammar" button in toolbar opens a side panel (slides in from right, 300px)
- Runs `checkGrammar()` on plain text extracted from editor
- Lists all issues with line numbers
- Click issue → scrolls to and highlights it in editor
- "Fix" button per issue applies the first suggestion
- "Fix All" button applies all suggestions
- "Ignore" dismisses individual issues
- Panel shows issue count in header

- [ ] **Step 6: Integrate with editor**

- Grammar decorations: TipTap plugin that runs `checkGrammar()` on content changes (debounced 1s)
- Renders blue wavy underlines under grammar issues (via ProseMirror decorations)
- Right-click on underlined text shows suggestions in context menu

- [ ] **Step 7: Verify grammar checking works end-to-end**

Run app — type "the the" → blue underline appears, right-click shows "the" suggestion, grammar panel shows the issue.

- [ ] **Step 8: Commit**

```bash
git add PealieNotes/src/renderer/grammar.js PealieNotes/src/renderer/components/grammarPanel.js PealieNotes/tests/grammar.test.js
git commit -m "feat: add grammar engine with inline decorations and grammar panel"
```

---

## Task 18: Version History Panel

**Files:**
- Create: `PealieNotes/src/renderer/components/versionHistory.js`

- [ ] **Step 1: Create versionHistory.js**

- Toolbar button (clock icon) opens version history panel (slides in from right, 300px, replaces grammar panel if open)
- Loads versions from storage for active note
- Shows list of versions: timestamp (relative date), version number
- Click version → shows diff/preview in a read-only editor view (muted, overlay)
- "Restore" button per version → confirms via modal, restores to editor, creates new save
- "Close" button slides panel out
- Empty state if no versions: "No previous versions"

- [ ] **Step 2: Verify version history works**

Run app — edit a note several times, open history, browse versions, restore one.

- [ ] **Step 3: Commit**

```bash
git add PealieNotes/src/renderer/components/versionHistory.js
git commit -m "feat: add version history panel with preview and restore"
```

---

## Task 19: Note Templates

**Files:**
- Create: `PealieNotes/src/renderer/components/templates.js`

- [ ] **Step 1: Create templates.js**

Built-in templates (HTML content):
- **Meeting Notes**: H2 "Meeting: [Title]", date, attendees list, agenda (numbered), discussion notes, action items (checklist)
- **To-Do List**: H2 "To-Do", today section (checklist), upcoming section (checklist), notes section
- **Journal Entry**: H2 with date, "How was your day?" prompt, highlights (bullet list), gratitude (bullet list)

Template picker:
- Shown when "New Note" is clicked (modal or dropdown)
- Grid of template cards: icon, name, brief description
- "Blank Note" option always first
- "My Templates" section for custom ones
- "Save as Template" option in note context menu → saves current note's content as a custom template
- Templates stored in `templates/` directory as JSON `{ name, icon, content }`

- [ ] **Step 2: Verify templates work**

Run app — click New Note, template picker appears, selecting a template creates a note with that content.

- [ ] **Step 3: Commit**

```bash
git add PealieNotes/src/renderer/components/templates.js
git commit -m "feat: add note templates with built-in and custom template support"
```

---

## Task 20: Tags System

**Files:**
- Modify: `PealieNotes/src/renderer/components/notesList.js`
- Modify: `PealieNotes/src/renderer/components/sidebar.js`
- Modify: `PealieNotes/src/renderer/storage.js`

- [ ] **Step 1: Add tag management to storage**

- Tags stored in config.json: `{ tags: [{ id, name, color }] }`
- Note metadata includes `tags: [tagId, ...]`
- `createTag(name, color)`, `deleteTag(id)`, `renameTag(id, name)`
- `addTagToNote(noteId, tagId)`, `removeTagFromNote(noteId, tagId)`
- `getNotesWithTag(tagId)` filters all notes

- [ ] **Step 2: Add tag UI to notes list**

- Tag pills on note cards (small colored rounded badges with tag name)
- "Add Tag" in note context menu → shows tag picker (existing tags + "New Tag")
- Tag picker: list of tags with colored dots, checkboxes for multi-select

- [ ] **Step 3: Add tag filter to sidebar**

- "Tags" collapsible section in sidebar below folders
- Lists all tags with colored dots and note counts
- Click tag → filters notes list to show only notes with that tag

- [ ] **Step 4: Verify tags work end to end**

Run app — create tags, assign to notes, filter by tag, remove tags.

- [ ] **Step 5: Commit**

```bash
git add PealieNotes/src/renderer/components/notesList.js PealieNotes/src/renderer/components/sidebar.js PealieNotes/src/renderer/storage.js
git commit -m "feat: add color-coded tags with filtering and management"
```

---

## Task 21: Search System

**Files:**
- Create: `PealieNotes/src/renderer/components/search.js`
- Modify: `PealieNotes/src/renderer/components/notesList.js`

- [ ] **Step 1: Create search.js**

Full-text search module:
- `searchAllNotes(query)` — calls storage.searchNotes, returns results with match context (snippet around match)
- Ctrl+Shift+F opens global search mode: search bar at top of notes list expands with "Search all notes" placeholder
- Results show: note title, folder name, match snippet with highlighted query terms
- Click result → navigates to folder/note, highlights matches in editor
- Filter options: folder dropdown, tag dropdown, date range (from/to)
- Debounced 200ms

- [ ] **Step 2: Integrate with notes list search bar**

The existing search bar in notes list:
- When typing, if `activeFolder` is set → searches within that folder only
- Ctrl+Shift+F → switches to global search (searches all notes regardless of folder)

- [ ] **Step 3: Verify search works**

Run app — create several notes with different content, search finds correct matches with highlighted snippets.

- [ ] **Step 4: Commit**

```bash
git add PealieNotes/src/renderer/components/search.js PealieNotes/src/renderer/components/notesList.js
git commit -m "feat: add full-text search with highlighting, filters, and global search mode"
```

---

## Task 22: Export & Import

**Files:**
- Modify: `PealieNotes/src/renderer/storage.js`
- Modify: `PealieNotes/src/renderer/components/notesList.js` (context menu additions)

- [ ] **Step 1: Implement export in storage.js**

- `exportAsPDF(noteId)` — uses Electron's `printToPDF` via IPC, opens save dialog
- `exportAsHTML(noteId)` — wraps content.html in a full HTML document with inline styles, save dialog
- `exportAsText(noteId)` — strips HTML tags, save dialog
- `exportAsMarkdown(noteId)` — converts HTML to Markdown (basic: headings, bold, italic, links, lists, code)
- `exportAllNotes()` — creates a zip of all note folders, save dialog

- [ ] **Step 2: Implement import in storage.js**

- `importFile(filePath)` — reads file, detects format by extension:
  - `.txt` → plain text wrapped in paragraph tags
  - `.md` → basic Markdown to HTML conversion (headings, bold, italic, links, lists, code)
  - `.html` → used as-is (sanitized)
- Creates a new note in active folder with imported content
- `importMultiple(filePaths)` — batch import from file dialog (multi-select)

- [ ] **Step 3: Add export/import to note context menu**

- Right-click note → "Export as..." → submenu: PDF, HTML, Text, Markdown
- Ctrl+P shortcut for PDF export
- "Import Notes..." option in notes list header menu

- [ ] **Step 4: Verify export and import work**

Run app — export a formatted note as PDF/HTML/TXT/MD, import a .md file, verify content.

- [ ] **Step 5: Commit**

```bash
git add PealieNotes/src/renderer/storage.js PealieNotes/src/renderer/components/notesList.js
git commit -m "feat: add export (PDF, HTML, TXT, MD) and import (TXT, MD, HTML) support"
```

---

## Task 23: Keyboard Shortcuts Manager

**Files:**
- Create: `PealieNotes/src/renderer/shortcuts.js`

- [ ] **Step 1: Create shortcuts.js**

Centralized shortcut manager:
- `registerShortcut(id, defaultKeys, action)` — registers a shortcut
- `rebindShortcut(id, newKeys)` — changes the key binding, saves to config
- `getShortcuts()` — returns all registered shortcuts with current bindings
- Uses `keydown` event listener on document
- Parses key combos: "Ctrl+Shift+N" → checks event.ctrlKey, event.shiftKey, event.key === 'n'
- Prevents default browser behavior for registered shortcuts
- Handles conflicts (warns if same combo registered twice)

Register all shortcuts from spec:
- Ctrl+N → new note
- Ctrl+Delete → trash note
- Ctrl+F → find in note
- Ctrl+H → find and replace
- Ctrl+Shift+F → search all notes
- Ctrl+S → force save
- Ctrl+, → open settings
- Ctrl+Shift+T → cycle theme
- Ctrl+P → export as PDF
- Ctrl+1/2/3 → heading levels
- Ctrl+0 → normal text
- (Editor formatting shortcuts handled by TipTap directly)

- [ ] **Step 2: Verify shortcuts work**

Run app — test each shortcut, verify correct action fires.

- [ ] **Step 3: Commit**

```bash
git add PealieNotes/src/renderer/shortcuts.js
git commit -m "feat: add keyboard shortcuts manager with rebinding support"
```

---

## Task 24: Settings Panel

**Files:**
- Create: `PealieNotes/src/renderer/components/settings.js`
- Create: `PealieNotes/src/renderer/styles/settings.css`

- [ ] **Step 1: Create settings.css**

- Full-size panel replacing the editor (slides in from right, 300ms)
- Left navigation: vertical tab list (General, Appearance, Editor, Sounds, Shortcuts, Storage, About)
- Right content: scrollable settings area
- Settings controls:
  - Text inputs: themed, rounded, focus ring
  - Toggles: iOS-style switches (44x24px, animated thumb slide, accent color when on)
  - Sliders: styled range inputs with accent track
  - Dropdowns: custom styled selects
  - Color pickers: grid of swatches + custom color input
  - Theme cards: 120x80px preview thumbnails with theme colors, selected state with accent border
  - Shortcut rebinder: click to capture → listening state with pulsing border → press keys → saves
- Section headers: bold, with subtle separator lines
- "Back" arrow button in top-left to return to editor
- All changes auto-save to config (debounced)

- [ ] **Step 2: Create settings.js**

Component that renders all settings sections:

**General:**
- Display name (text input, saved to config)
- Default folder for new notes (dropdown of folders)
- Start minimized to tray (toggle)
- Open last note on launch (toggle)

**Appearance:**
- Theme picker: grid of theme preview cards, click to apply with smooth transition
- Accent color: color picker with presets + custom
- UI font family (dropdown)
- Editor font family (dropdown)
- Editor zoom default (slider 80–200%)

**Editor:**
- Default font size (number input with stepper)
- Line spacing (dropdown: 1.0, 1.2, 1.4, 1.6, 1.8, 2.0)
- Spellcheck toggle (controls Electron webFrame.setSpellCheckProvider)
- Grammar check toggle (controls grammar decoration plugin)
- Auto-save interval (slider 100ms–5000ms, shown in ms)
- Markdown shortcuts toggle
- Show word count toggle (controls status bar)

**Sounds:**
- Master toggle (big switch at top)
- Volume slider (0–100%)
- Individual toggles: Click, Pop, Swoosh, Ding, Tap — each with a "Preview" button

**Keyboard Shortcuts:**
- Full list from shortcuts.js, grouped by category
- Each row: action name | current binding | "Edit" button
- Click "Edit" → row enters capture mode, press new keys, Enter to save, Escape to cancel
- "Reset to Defaults" button at bottom

**Storage:**
- Notes folder location: path display + "Change..." button (opens folder dialog)
- Storage stats: "X notes, Y folders, Z MB total"
- "Export All Notes" button → zip download
- "Import Notes..." button → file dialog

**About:**
- "Pealie Notes" with logo
- Version: 1.0.0
- "Made with love" or similar

Each section reads from and writes to `store.get('config')` which auto-saves via storage.

- [ ] **Step 3: Verify all settings sections work**

Run app — open settings, change theme (live preview), toggle sounds (hear preview), change font, rebind a shortcut, verify all saved to config.json.

- [ ] **Step 4: Commit**

```bash
git add PealieNotes/src/renderer/components/settings.js PealieNotes/src/renderer/styles/settings.css
git commit -m "feat: add settings panel with all sections (general, appearance, editor, sounds, shortcuts, storage, about)"
```

---

## Task 25: Welcome / Onboarding Flow

**Files:**
- Create: `PealieNotes/src/renderer/components/welcome.js`
- Create: `PealieNotes/src/renderer/styles/welcome.css`

- [ ] **Step 1: Create welcome.css**

- Full-screen overlay on top of (blurred) main app
- Centered content area: max-width 600px
- Splash screen: centered logo + "Pealie Notes" text with glow animation
- Step container: centered card (500px wide) with generous padding
- Step transitions: horizontal slide + fade (400ms)
- Progress dots: row of small circles at bottom, active dot filled with accent
- Inputs: large, rounded, centered
- Theme grid: 2x5 grid of theme preview cards (each ~100x70px)
- Feature cards: icon + title + description, slide in one by one (staggered 100ms delay)
- "Get Started" button: large, rounded, accent color, hover scale, click spring
- Overall feel: spacious, modern, welcoming

- [ ] **Step 2: Create welcome.js**

Onboarding flow component:

1. **Splash** (auto-advances after 2s):
   - App icon (SVG) fades in (0→1 over 500ms)
   - "Pealie Notes" text fades in (delayed 300ms)
   - Subtle glow pulse on icon

2. **Step 1 — Welcome**:
   - "Welcome to Pealie Notes" heading (slides in from right)
   - "What should we call you?" subtext
   - Large centered text input for display name
   - "Continue" button (disabled until name entered)

3. **Step 2 — Theme**:
   - "Pick your style" heading
   - Grid of 10 theme cards showing preview colors
   - Clicking a card applies the theme live (smooth transition behind the overlay)
   - Currently selected card has accent border + checkmark

4. **Step 3 — Highlights**:
   - "Here's what you can do" heading
   - 4 feature cards that slide in with stagger:
     - Rich Formatting (text icon): "Bold, colors, fonts, and more"
     - Folders & Tags (folder icon): "Organize your way"
     - Auto-Save (save icon): "Never lose your work"
     - Keyboard Shortcuts (keyboard icon): "Work faster"

5. **Done**:
   - "You're all set, [name]!" heading
   - "Start Writing" button
   - Button click → saves config (firstLaunch: false, displayName, theme), plays `pop` sound, overlay fades out to reveal main app with first empty note

Progress dots at bottom of steps 1-4. Back button on steps 2-4.

- [ ] **Step 3: Verify onboarding flow end to end**

Delete config.json, run app — full flow appears, each step transitions smoothly, theme selection works live, completes into main app.

- [ ] **Step 4: Commit**

```bash
git add PealieNotes/src/renderer/components/welcome.js PealieNotes/src/renderer/styles/welcome.css
git commit -m "feat: add welcome onboarding flow with splash, theme picker, and feature highlights"
```

---

## Task 26: System Tray & Global Shortcuts

**Files:**
- Modify: `PealieNotes/src/main/main.js`
- Create: `PealieNotes/src/main/tray.js`
- Create: `PealieNotes/src/main/globalShortcuts.js`

- [ ] **Step 1: Create tray.js**

System tray module:
- Creates tray icon using the app icon
- Right-click menu: "New Quick Note", "Show Pealie Notes", separator, "Quit"
- "New Quick Note" → sends IPC to renderer to create note + shows window
- "Show Pealie Notes" → shows and focuses window
- "Quit" → actually quits the app (not just hide)
- Left-click on tray icon → show/hide window toggle

- [ ] **Step 2: Create globalShortcuts.js**

Global shortcut registration:
- Registers Ctrl+Shift+N globally via Electron's `globalShortcut`
- On trigger: creates a new note via IPC, shows and focuses window
- Reads shortcut binding from config (supports rebinding)
- Unregisters on app quit

- [ ] **Step 3: Modify main.js for minimize-to-tray behavior**

- On window close event: if config says minimize-to-tray, hide window instead of quitting
- On all windows closed: don't quit if tray is active
- Initialize tray and global shortcuts after window creation

- [ ] **Step 4: Verify tray and global shortcuts work**

Run app — close window (goes to tray), click tray to restore, Ctrl+Shift+N creates note from background.

- [ ] **Step 5: Commit**

```bash
git add PealieNotes/src/main/tray.js PealieNotes/src/main/globalShortcuts.js PealieNotes/src/main/main.js
git commit -m "feat: add system tray with menu and global quick-note shortcut"
```

---

## Task 27: App Icon

**Files:**
- Create: `PealieNotes/src/assets/icon.svg`
- Create: `PealieNotes/src/assets/icon.ico`

- [ ] **Step 1: Create SVG app icon**

Design: A stylized notepad with a pen/pencil, using the default accent color (#5865F2 burple as base). Clean, flat design with subtle gradients. Should look good at 16x16 through 256x256.

SVG elements:
- Rounded rectangle (notepad body) with slight shadow
- Horizontal lines suggesting text
- Angled pen/pencil overlapping bottom-right corner
- Accent color as primary, with lighter/darker shades for depth

- [ ] **Step 2: Generate ICO from SVG**

Use Electron's nativeImage or a build-time script to convert SVG to ICO with multiple sizes (16, 32, 48, 64, 128, 256).

Alternatively, create a simple PNG-based ICO using the SVG rendered at multiple sizes. For now, use the SVG directly in the app and a pre-generated ICO for the window/taskbar.

- [ ] **Step 3: Wire icon into main.js and index.html**

- Set window icon in main.js BrowserWindow options
- Add favicon to index.html
- Use in titlebar component
- Use in welcome screen splash

- [ ] **Step 4: Commit**

```bash
git add PealieNotes/src/assets/
git commit -m "feat: add Pealie Notes app icon (SVG + ICO)"
```

---

## Task 28: Panel Resize Handles

**Files:**
- Modify: `PealieNotes/src/renderer/app.js`
- Modify: `PealieNotes/src/renderer/styles/main.css`

- [ ] **Step 1: Add resize handle styles to main.css**

- `.resize-handle`: 4px width, cursor: col-resize, background transparent
- Hover: background shows a subtle line (accent color, 50% opacity)
- Active/dragging: background shows a thicker line
- Two handles: between sidebar and notes list, between notes list and editor

- [ ] **Step 2: Implement resize logic in app.js**

- Mousedown on handle starts drag
- Mousemove updates panel width (clamped: sidebar 150-400px, notes list 200-500px)
- Mouseup ends drag, saves widths to config
- Loads saved widths from config on startup

- [ ] **Step 3: Verify resizing works smoothly**

Run app — drag handles, panels resize fluidly, widths persist across restart.

- [ ] **Step 4: Commit**

```bash
git add PealieNotes/src/renderer/app.js PealieNotes/src/renderer/styles/main.css
git commit -m "feat: add draggable resize handles between panels"
```

---

## Task 29: Drag & Drop

**Files:**
- Modify: `PealieNotes/src/renderer/components/notesList.js`
- Modify: `PealieNotes/src/renderer/components/sidebar.js`
- Modify: `PealieNotes/src/renderer/components/editor.js`

- [ ] **Step 1: Add note drag from notes list**

- Note cards get `draggable="true"`
- `dragstart` sets note ID in dataTransfer
- Drag ghost shows note title
- Dragging note gets opacity: 0.5

- [ ] **Step 2: Add folder drop targets in sidebar**

- Folder items listen for `dragover` (prevent default, show highlight), `dragleave` (remove highlight), `drop` (move note to folder)
- Drop highlight: dashed accent border on folder item
- On drop: calls `storage.moveNote(noteId, folderId)`, refreshes both panels, plays `pop` sound

- [ ] **Step 3: Add image drop into editor**

- Editor container listens for drag events
- Detects file drops (dataTransfer.files)
- For image files: copies to note's `assets/` folder, inserts `<img>` into editor at drop position
- Drop zone highlight when dragging files over editor

- [ ] **Step 4: Verify all drag & drop interactions**

Run app — drag note to folder (moves), drag image into editor (inserts).

- [ ] **Step 5: Commit**

```bash
git add PealieNotes/src/renderer/components/notesList.js PealieNotes/src/renderer/components/sidebar.js PealieNotes/src/renderer/components/editor.js
git commit -m "feat: add drag-and-drop for notes between folders and images into editor"
```

---

## Task 30: Integration & Polish

**Files:**
- Modify: `PealieNotes/src/renderer/app.js`
- Modify: various component files

- [ ] **Step 1: Wire all components together in app.js**

- Initialize all components in correct order
- Event bus connections:
  - Sidebar folder click → notes list filter
  - Notes list note click → editor load
  - Editor save → notes list metadata refresh
  - Settings changes → all components react
  - Theme changes → immediate visual update
  - Sound settings → sound engine config
- Handle first-launch vs returning user flow
- Load last active note on startup (if configured)

- [ ] **Step 2: Add all sound effect triggers**

Wire sounds to:
- Button clicks (all toolbar buttons, sidebar buttons, settings controls)
- New note creation (`pop`)
- Trash note (`swoosh`)
- Checklist item toggle (`ding`)
- Settings toggle switches (`tap`)
- Context menu selections (`click`)

- [ ] **Step 3: Add all tooltip text**

Every toolbar button, sidebar button, and icon button gets a tooltip:
- Appear on hover after 400ms delay
- Fade-in animation
- Show shortcut key in parentheses where applicable (e.g., "Bold (Ctrl+B)")

- [ ] **Step 4: Add empty states**

- No notes in folder: centered icon + "No notes in this folder" + "Create one" button
- No folders: "Create your first folder" prompt
- Trash empty: "Trash is empty" message
- Search no results: "No results for '[query]'"

- [ ] **Step 5: Add loading states**

- App startup: show brief loading indicator before main UI
- Note loading: skeleton placeholder while content loads
- Search: subtle loading indicator during search

- [ ] **Step 6: Final visual polish pass**

- Verify all animations are smooth (no jank)
- Check all theme colors in every theme (no invisible text, no bad contrast)
- Verify all hover/active states
- Check scrollbar styling in all panels
- Verify focus management (Tab navigation, focus rings)
- Test window resize behavior (responsive panels)
- Check text overflow/truncation in notes list and sidebar

- [ ] **Step 7: Commit**

```bash
git add -A PealieNotes/
git commit -m "feat: wire all components together, add sound triggers, tooltips, empty states, and polish"
```

---

## Task 31: Testing & Bug Fixes

**Files:**
- All test files
- Various component files (bug fixes)

- [ ] **Step 1: Run all unit tests**

Run: `cd PealieNotes && npm test`
Expected: All tests pass.

- [ ] **Step 2: Manual testing checklist**

Test every feature systematically:
- [ ] Create folder, rename, delete, nest folders
- [ ] Create note, edit, auto-save, switch between notes
- [ ] All formatting options (bold, italic, underline, strikethrough, headings, fonts, sizes, colors, highlights)
- [ ] Lists (bullet, numbered, checklist with checkboxes)
- [ ] Links, images, tables, code blocks, block quotes, horizontal rules
- [ ] Markdown shortcuts (# for heading, - for list, etc.)
- [ ] Copy/paste formatting preservation
- [ ] Find & replace
- [ ] Pin, favorite, duplicate notes
- [ ] Tags: create, assign, filter, remove
- [ ] Search: local and global
- [ ] Sort: all 4 modes
- [ ] Templates: built-in and custom
- [ ] Version history: browse, preview, restore
- [ ] Grammar check: inline underlines and panel
- [ ] Export: PDF, HTML, TXT, MD
- [ ] Import: TXT, MD, HTML
- [ ] All 10 themes: switch, verify no visual issues
- [ ] Sound effects: all 5 sounds play correctly, volume control, individual toggles
- [ ] Settings: all sections, changes persist
- [ ] Welcome flow: complete on fresh start
- [ ] Keyboard shortcuts: all shortcuts fire correct actions
- [ ] System tray: minimize to tray, restore, tray menu
- [ ] Global hotkey: Ctrl+Shift+N from background
- [ ] Drag & drop: notes between folders, images into editor
- [ ] Panel resize: drag handles, persist widths
- [ ] Context menus: right-click everywhere appropriate
- [ ] Window: minimize, maximize, restore, close, drag titlebar

- [ ] **Step 3: Fix any bugs found**

Address each issue found during testing.

- [ ] **Step 4: Commit fixes**

```bash
git add -A PealieNotes/
git commit -m "fix: address bugs found during comprehensive testing"
```

---

## Task 32: Packaging

**Files:**
- Create: `PealieNotes/electron-builder.yml`
- Modify: `PealieNotes/package.json` (if needed)

- [ ] **Step 1: Create electron-builder.yml**

```yaml
appId: com.pealie.notes
productName: Pealie Notes
directories:
  output: dist
win:
  target:
    - nsis
    - portable
  icon: src/assets/icon.ico
nsis:
  oneClick: false
  allowToChangeInstallationDirectory: true
  installerIcon: src/assets/icon.ico
  uninstallerIcon: src/assets/icon.ico
  installerHeaderIcon: src/assets/icon.ico
files:
  - src/**/*
  - node_modules/**/*
  - "!**/*.test.js"
  - "!tests/**/*"
```

- [ ] **Step 2: Build the app**

Run: `cd PealieNotes && npm run build`
Expected: Creates `dist/PealieNotes-Setup.exe` and `dist/PealieNotes-Portable.exe`

- [ ] **Step 3: Test the built installer**

Run the installer, verify the installed app works identically to dev mode.

- [ ] **Step 4: Commit**

```bash
git add PealieNotes/electron-builder.yml
git commit -m "feat: add electron-builder config for Windows installer and portable builds"
```

---

## Summary

| Task | Description | Dependencies |
|------|-------------|-------------|
| 1 | Project scaffolding & Electron shell | None |
| 2 | Utility modules | Task 1 |
| 3 | State management (store) | Task 1 |
| 4 | Storage layer | Tasks 1, 2, 3 |
| 5 | Theme system | Task 1 |
| 6 | Base styles & layout | Tasks 1, 5 |
| 7 | Sound system | Task 1 |
| 8 | Custom titlebar | Tasks 5, 6 |
| 9 | Context menu system | Task 6 |
| 10 | Modal dialog system | Task 6 |
| 11 | Folder sidebar | Tasks 4, 6, 9 |
| 12 | Notes list panel | Tasks 4, 6, 9 |
| 13 | TipTap editor core | Tasks 4, 6 |
| 14 | Editor toolbar | Task 13 |
| 15 | Status bar | Task 13 |
| 16 | Find & replace | Task 13 |
| 17 | Grammar engine | Task 13 |
| 18 | Version history panel | Tasks 4, 13 |
| 19 | Note templates | Tasks 4, 10, 12 |
| 20 | Tags system | Tasks 4, 11, 12 |
| 21 | Search system | Tasks 4, 12 |
| 22 | Export & import | Tasks 4, 9 |
| 23 | Keyboard shortcuts | Task 3 |
| 24 | Settings panel | Tasks 3, 5, 7, 23 |
| 25 | Welcome / onboarding | Tasks 5, 7, 10 |
| 26 | System tray & global shortcuts | Task 1 |
| 27 | App icon | Task 1 |
| 28 | Panel resize handles | Task 6 |
| 29 | Drag & drop | Tasks 11, 12, 13 |
| 30 | Integration & polish | Tasks 1–29 |
| 31 | Testing & bug fixes | Task 30 |
| 32 | Packaging | Task 31 |
