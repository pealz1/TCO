import { initTitlebar } from './components/titlebar.js';
import { initSidebar } from './components/sidebar.js';
import { initNotesList } from './components/notesList.js';
import { initEditor } from './components/editor.js';
import { initToolbar } from './components/toolbar.js';
import { initStatusBar } from './components/statusBar.js';
import { initFindReplace } from './components/findReplace.js';
import { initSearch } from './components/search.js';
import { initShortcuts, registerShortcut } from './shortcuts.js';
import { initSettings } from './components/settings.js';
import { initWelcome } from './components/welcome.js';
import { store } from './store.js';

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
    let firstLaunch = false;

    try {
      const appDataPath = await window.api.getAppDataPath();
      const configPath = await window.api.joinPath(appDataPath, 'pealie-notes', 'config.json');
      const configExists = await window.api.exists(configPath);
      firstLaunch = !configExists;
    } catch {
      firstLaunch = true;
    }

    if (firstLaunch) {
      console.log('Pealie Notes: first launch detected');
    }

    // Make app globally accessible before initializing components
    window.app = app;

    // Initialize components
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

    // Register built-in keyboard shortcuts
    registerShortcut('settings', () => {
      const current = store.get('view');
      store.set('view', current === 'settings' ? 'main' : 'settings');
    });

    // Listen for quick-note from tray
    if (window.api && window.api.onQuickNote) {
      window.api.onQuickNote(() => {
        app.eventBus.emit('quick-note');
      });
    }

    console.log('Pealie Notes initialized');
    app.eventBus.emit('app:ready', { firstLaunch });
  },
};

window.addEventListener('DOMContentLoaded', () => {
  app.init();
});

export { app, EventBus };
