import { initTitlebar } from './components/titlebar.js';
import { initSidebar } from './components/sidebar.js';
import { initNotesList } from './components/notesList.js';
import { initEditor, getEditor } from './components/editor.js';
import { initToolbar } from './components/toolbar.js';
import { initStatusBar } from './components/statusBar.js';
import { initFindReplace } from './components/findReplace.js';
import { initSearch } from './components/search.js';
import { initShortcuts, registerShortcut } from './shortcuts.js';
import { initSettings } from './components/settings.js';
import { initWelcome } from './components/welcome.js';
import { store } from './store.js';
import * as storage from './storage.js';
import { setTheme, setAccentColor } from './themes.js';
import { soundEngine } from './sounds.js';

class EventBus extends EventTarget {
  emit(event, detail) {
    this.dispatchEvent(new CustomEvent(event, { detail }));
  }

  on(event, handler) {
    this.addEventListener(event, handler);
  }

  off(event, handler) {
    this.removeEventListener(event, handler);
  }

  once(event, handler) {
    this.addEventListener(event, handler, { once: true });
  }
}

const app = {
  eventBus: new EventBus(),

  async init() {
    // Make app globally accessible before initializing components
    window.app = app;

    // --- 1. Initialize storage ---
    let firstLaunch = false;
    try {
      const appDataPath = await window.api.getAppDataPath();
      const basePath = await window.api.joinPath(appDataPath, 'pealie-notes');
      await storage.initStorage(basePath);

      const config = await storage.loadConfig();
      firstLaunch = config.firstLaunch !== false;
      store.set('config', config);
    } catch (err) {
      console.error('Storage init error:', err);
      firstLaunch = true;
    }

    // --- 2. Apply theme and accent from config ---
    const config = store.get('config');
    if (config.theme) {
      setTheme(config.theme);
      store.set('theme', config.theme);
    }
    if (config.accentColor) {
      setAccentColor(config.accentColor);
    }

    // --- 3. Configure sound engine from config ---
    soundEngine.setEnabled(config.soundEnabled !== false);
    soundEngine.setVolume((config.soundVolume || 30) / 100);

    // --- 4. Initialize all UI components ---
    initTitlebar();
    initSidebar();
    initNotesList();
    initEditor();
    initToolbar();
    initStatusBar();
    initFindReplace();
    initSearch();
    initShortcuts();
    initSettings();
    initWelcome();

    // --- 5. Initialize resize handles ---
    initResizeHandles();

    // --- 6. Initialize image drag & drop on editor ---
    initEditorDragDrop();

    // --- 7. Load data from storage into store ---
    try {
      const folders = await storage.loadFolders();
      store.set('folders', folders);

      const notes = await storage.loadAllNotes();
      store.set('notes', notes);
    } catch (err) {
      console.error('Failed to load data:', err);
    }

    // --- 8. Restore last active folder/note from config ---
    if (config.lastActiveFolder) {
      store.set('activeFolder', config.lastActiveFolder);
    }
    if (config.openLastNote !== false && config.lastActiveNote) {
      store.set('activeNote', config.lastActiveNote);
    }

    // --- 9. Register keyboard shortcuts ---
    registerShortcut('settings', () => {
      const current = store.get('view');
      store.set('view', current === 'settings' ? 'main' : 'settings');
    });

    registerShortcut('new-note', () => {
      const btn = document.querySelector('.new-note-btn');
      if (btn) btn.click();
    });

    registerShortcut('trash-note', async () => {
      const noteId = store.get('activeNote');
      if (!noteId) return;
      const note = store.get('notes').find(n => n.id === noteId);
      if (!note) return;
      const { showConfirm } = await import('./components/modal.js');
      if (await showConfirm(`Move "${note.title || 'Untitled'}" to trash?`)) {
        try {
          await storage.trashNote(note.id, note.folderId);
          const notes = await storage.loadAllNotes();
          store.set('notes', notes);
          store.set('activeNote', null);
          soundEngine.play('swoosh');
        } catch (err) {
          console.error('Trash note error:', err);
        }
      }
    });

    registerShortcut('save', () => {
      // Trigger auto-save immediately by emitting
      app.eventBus.emit('force-save');
    });

    registerShortcut('cycle-theme', () => {
      const { getThemeList, getCurrentTheme, setTheme: st } = require('./themes.js');
      // Titlebar already handles this
      document.querySelector('.theme-btn')?.click();
    });

    // --- 10. Wire view switching ---
    store.on('view', (view) => {
      const mainContent = document.getElementById('main-content');
      const settingsPanel = document.getElementById('settings-panel');
      if (view === 'settings') {
        mainContent.style.display = 'none';
        settingsPanel.style.display = 'flex';
      } else {
        mainContent.style.display = 'flex';
        settingsPanel.style.display = 'none';
      }
    });

    // --- 11. Persist active folder/note to config ---
    store.on('activeFolder', async (folderId) => {
      try {
        const cfg = await storage.loadConfig();
        cfg.lastActiveFolder = folderId;
        await storage.saveConfig(cfg);
      } catch {}
    });

    store.on('activeNote', async (noteId) => {
      try {
        const cfg = await storage.loadConfig();
        cfg.lastActiveNote = noteId;
        await storage.saveConfig(cfg);
      } catch {}
      // Show/hide empty state in editor
      updateEditorEmptyState(noteId);
    });

    // --- 12. Apply editor font/size from config ---
    applyEditorStyles(config);

    // --- 13. Wire config changes for editor styling ---
    store.on('config', (newConfig) => {
      applyEditorStyles(newConfig);
    });

    // --- 14. Show empty state if no active note ---
    updateEditorEmptyState(store.get('activeNote'));

    // --- 15. Listen for quick-note from tray ---
    if (window.api && window.api.onQuickNote) {
      window.api.onQuickNote(() => {
        app.eventBus.emit('quick-note');
      });
    }

    console.log('Pealie Notes initialized');
    app.eventBus.emit('app:ready', { firstLaunch });
  },
};

// --- Resize Handles ---

function initResizeHandles() {
  setupResize('resize-handle-1', 'sidebar', 150, 400);
  setupResize('resize-handle-2', 'notes-list', 200, 500);
}

function setupResize(handleId, panelId, minWidth, maxWidth) {
  const handle = document.getElementById(handleId);
  const panel = document.getElementById(panelId);
  if (!handle || !panel) return;

  let startX, startWidth;

  handle.addEventListener('mousedown', (e) => {
    startX = e.clientX;
    startWidth = panel.offsetWidth;
    handle.classList.add('dragging');
    document.body.style.cursor = 'col-resize';
    document.body.style.userSelect = 'none';

    const onMove = (e) => {
      const diff = e.clientX - startX;
      const newWidth = Math.max(minWidth, Math.min(maxWidth, startWidth + diff));
      panel.style.width = newWidth + 'px';
    };

    const onUp = () => {
      handle.classList.remove('dragging');
      document.body.style.cursor = '';
      document.body.style.userSelect = '';
      document.removeEventListener('mousemove', onMove);
      document.removeEventListener('mouseup', onUp);
      // Save widths to config
      try {
        const config = store.get('config');
        config[panelId === 'sidebar' ? 'sidebarWidth' : 'notesListWidth'] = panel.offsetWidth;
        store.set('config', config);
        storage.saveConfig(config);
      } catch {}
    };

    document.addEventListener('mousemove', onMove);
    document.addEventListener('mouseup', onUp);
  });

  // Load saved width
  const config = store.get('config');
  const savedWidth = config[panelId === 'sidebar' ? 'sidebarWidth' : 'notesListWidth'];
  if (savedWidth) panel.style.width = savedWidth + 'px';
}

// --- Image Drag & Drop on Editor ---

function initEditorDragDrop() {
  const editorContainer = document.getElementById('editor-container');
  if (!editorContainer) return;

  editorContainer.addEventListener('dragover', (e) => {
    // Check if files are being dragged
    if (e.dataTransfer && e.dataTransfer.types.includes('Files')) {
      e.preventDefault();
      e.dataTransfer.dropEffect = 'copy';
      editorContainer.classList.add('drop-highlight');
    }
  });

  editorContainer.addEventListener('dragleave', (e) => {
    // Only remove highlight if leaving the container entirely
    if (!editorContainer.contains(e.relatedTarget)) {
      editorContainer.classList.remove('drop-highlight');
    }
  });

  editorContainer.addEventListener('drop', async (e) => {
    editorContainer.classList.remove('drop-highlight');
    if (!e.dataTransfer || !e.dataTransfer.files || e.dataTransfer.files.length === 0) return;

    const noteId = store.get('activeNote');
    if (!noteId) return;
    const note = store.get('notes').find(n => n.id === noteId);
    if (!note) return;

    const imageExts = ['jpg', 'jpeg', 'png', 'gif', 'webp', 'svg', 'bmp'];

    for (const file of e.dataTransfer.files) {
      const ext = file.name.split('.').pop().toLowerCase();
      if (!imageExts.includes(ext)) continue;

      e.preventDefault();

      try {
        // Read image as base64 data URL
        const reader = new FileReader();
        const dataUrl = await new Promise((resolve, reject) => {
          reader.onload = () => resolve(reader.result);
          reader.onerror = reject;
          reader.readAsDataURL(file);
        });

        // Insert into editor
        const ed = getEditor();
        if (ed) {
          ed.chain().focus().setImage({ src: dataUrl, alt: file.name }).run();
          soundEngine.play('pop');
        }
      } catch (err) {
        console.error('Image drop error:', err);
      }
    }
  });
}

// --- Editor empty state ---

function updateEditorEmptyState(noteId) {
  const editorContainer = document.getElementById('editor-container');
  if (!editorContainer) return;

  let emptyEl = editorContainer.querySelector('.editor-empty-state');

  if (!noteId) {
    // Show empty state
    if (!emptyEl) {
      emptyEl = document.createElement('div');
      emptyEl.className = 'editor-empty-state';
      emptyEl.innerHTML = '<div style="text-align:center;padding:80px 20px;color:var(--text-secondary);"><div style="font-size:48px;margin-bottom:16px;">&#x1F4DD;</div><p style="font-size:16px;">Select a note or create a new one</p></div>';
      editorContainer.appendChild(emptyEl);
    }
    emptyEl.style.display = 'block';
    // Hide TipTap
    const proseMirror = editorContainer.querySelector('.ProseMirror');
    if (proseMirror) proseMirror.style.display = 'none';
  } else {
    // Hide empty state, show editor
    if (emptyEl) emptyEl.style.display = 'none';
    const proseMirror = editorContainer.querySelector('.ProseMirror');
    if (proseMirror) proseMirror.style.display = '';
  }
}

// --- Apply editor styling from config ---

function applyEditorStyles(config) {
  const editorContainer = document.getElementById('editor-container');
  if (!editorContainer) return;
  if (config.editorFont) {
    editorContainer.style.fontFamily = config.editorFont;
  }
  if (config.editorFontSize) {
    editorContainer.style.fontSize = config.editorFontSize + 'px';
  }
  if (config.lineSpacing) {
    editorContainer.style.lineHeight = config.lineSpacing;
  }
}

window.addEventListener('DOMContentLoaded', () => {
  app.init();
});

export { app, EventBus };
