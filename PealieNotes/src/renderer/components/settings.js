import { store } from '../store.js';
import * as storage from '../storage.js';
import { setTheme, setAccentColor, getThemeList, getCurrentTheme } from '../themes.js';
import { getShortcuts, rebindShortcut, getDefaultShortcuts } from '../shortcuts.js';
import { soundEngine } from '../sounds.js';

const sections = [
  { id: 'general', label: 'General' },
  { id: 'appearance', label: 'Appearance' },
  { id: 'editor', label: 'Editor' },
  { id: 'sounds', label: 'Sounds' },
  { id: 'shortcuts', label: 'Keyboard Shortcuts' },
  { id: 'storage', label: 'Storage' },
  { id: 'about', label: 'About' },
];

const accentPresets = [
  '#007AFF', '#5865F2', '#e91e63', '#4CAF50', '#FF9800', '#9C27B0',
];

const fontOptions = [
  'Segoe UI', 'Arial', 'Helvetica', 'Georgia', 'Times New Roman', 'Consolas', 'Courier New',
];

let activeSection = 'general';
let capturingShortcut = null;

function getConfig() {
  return store.get('config') || {};
}

async function updateConfig(key, value) {
  const config = await storage.loadConfig();
  config[key] = value;
  await storage.saveConfig(config);
  store.set('config', config);
}

export function initSettings() {
  const panel = document.getElementById('settings-panel');
  if (!panel) return;

  render();

  store.on('view', (view) => {
    if (view === 'settings') {
      panel.classList.add('visible');
      panel.style.display = 'flex';
      render();
    } else {
      panel.classList.remove('visible');
      panel.style.display = 'none';
    }
  });
}

function render() {
  const panel = document.getElementById('settings-panel');
  if (!panel) return;

  panel.innerHTML = `
    <nav class="settings-nav">
      <div class="settings-nav-title">Settings</div>
      ${sections.map(s => `
        <div class="settings-nav-item${s.id === activeSection ? ' active' : ''}" data-section="${s.id}">${s.label}</div>
      `).join('')}
    </nav>
    <div class="settings-content">
      <button class="settings-back" id="settings-back-btn">&larr; Back</button>
      <div id="settings-section-content"></div>
    </div>
  `;

  panel.querySelectorAll('.settings-nav-item').forEach(item => {
    item.addEventListener('click', () => {
      activeSection = item.dataset.section;
      render();
    });
  });

  panel.querySelector('#settings-back-btn').addEventListener('click', () => {
    store.set('view', 'main');
  });

  renderSection();
}

function renderSection() {
  const container = document.getElementById('settings-section-content');
  if (!container) return;

  switch (activeSection) {
    case 'general': renderGeneral(container); break;
    case 'appearance': renderAppearance(container); break;
    case 'editor': renderEditor(container); break;
    case 'sounds': renderSounds(container); break;
    case 'shortcuts': renderShortcuts(container); break;
    case 'storage': renderStorage(container); break;
    case 'about': renderAbout(container); break;
  }
}

// ---- General ----

function renderGeneral(container) {
  const config = getConfig();
  container.innerHTML = `
    <div class="settings-section">
      <h3>General</h3>
      <div class="setting-row">
        <div class="setting-info">
          <div class="setting-label">Display Name</div>
          <div class="setting-description">Your name shown in the app</div>
        </div>
        <input type="text" class="settings-input" id="setting-display-name" value="${escHtml(config.displayName || '')}" placeholder="Your name" />
      </div>
      <div class="setting-row">
        <div class="setting-info">
          <div class="setting-label">Minimize to Tray</div>
          <div class="setting-description">Keep running in system tray when closed</div>
        </div>
        <button class="toggle${config.minimizeToTray !== false ? ' active' : ''}" id="toggle-minimize-tray"></button>
      </div>
      <div class="setting-row">
        <div class="setting-info">
          <div class="setting-label">Open Last Note on Start</div>
          <div class="setting-description">Reopen the last note you were editing</div>
        </div>
        <button class="toggle${config.openLastNote !== false ? ' active' : ''}" id="toggle-open-last"></button>
      </div>
    </div>
  `;

  const nameInput = container.querySelector('#setting-display-name');
  nameInput.addEventListener('change', () => updateConfig('displayName', nameInput.value));

  setupToggle(container, '#toggle-minimize-tray', 'minimizeToTray');
  setupToggle(container, '#toggle-open-last', 'openLastNote');
}

// ---- Appearance ----

function renderAppearance(container) {
  const config = getConfig();
  const themes = getThemeList();
  const currentTheme = getCurrentTheme();
  const currentAccent = config.accentColor || '#007AFF';

  container.innerHTML = `
    <div class="settings-section">
      <h3>Theme</h3>
      <div class="theme-grid">
        ${themes.map(t => `
          <div class="theme-card${t.id === currentTheme ? ' selected' : ''}" data-theme="${t.id}" title="${t.name}">
            ${t.previewColors.map(c => `<div class="theme-card-bar" style="background:${c}"></div>`).join('')}
          </div>
        `).join('')}
      </div>
    </div>
    <div class="settings-section">
      <h3>Accent Color</h3>
      <div class="accent-grid">
        ${accentPresets.map(c => `
          <div class="accent-swatch${c === currentAccent ? ' selected' : ''}" data-color="${c}" style="background:${c}"></div>
        `).join('')}
        <input type="color" id="accent-custom" value="${currentAccent}" style="width:32px;height:32px;border:none;padding:0;cursor:pointer;border-radius:50%;" title="Custom color" />
      </div>
    </div>
    <div class="settings-section">
      <h3>Fonts</h3>
      <div class="setting-row">
        <div class="setting-info">
          <div class="setting-label">Editor Font</div>
        </div>
        <select class="settings-select" id="setting-editor-font">
          ${fontOptions.map(f => `<option value="${f}"${config.editorFont === f ? ' selected' : ''}>${f}</option>`).join('')}
        </select>
      </div>
    </div>
  `;

  container.querySelectorAll('.theme-card').forEach(card => {
    card.addEventListener('click', () => {
      const id = card.dataset.theme;
      setTheme(id);
      updateConfig('theme', id);
      renderAppearance(container);
    });
  });

  container.querySelectorAll('.accent-swatch').forEach(swatch => {
    swatch.addEventListener('click', () => {
      const color = swatch.dataset.color;
      setAccentColor(color);
      updateConfig('accentColor', color);
      renderAppearance(container);
    });
  });

  const customColor = container.querySelector('#accent-custom');
  customColor.addEventListener('input', () => {
    setAccentColor(customColor.value);
    updateConfig('accentColor', customColor.value);
  });

  const fontSelect = container.querySelector('#setting-editor-font');
  fontSelect.addEventListener('change', () => {
    updateConfig('editorFont', fontSelect.value);
    document.querySelector('#editor-container')?.style.setProperty('font-family', fontSelect.value);
  });
}

// ---- Editor ----

function renderEditor(container) {
  const config = getConfig();
  container.innerHTML = `
    <div class="settings-section">
      <h3>Editor</h3>
      <div class="setting-row">
        <div class="setting-info">
          <div class="setting-label">Default Font Size</div>
        </div>
        <input type="number" class="settings-number" id="setting-font-size" value="${config.editorFontSize || 16}" min="10" max="32" />
      </div>
      <div class="setting-row">
        <div class="setting-info">
          <div class="setting-label">Line Spacing</div>
        </div>
        <select class="settings-select" id="setting-line-spacing">
          <option value="1.4"${config.lineSpacing == 1.4 ? ' selected' : ''}>Compact (1.4)</option>
          <option value="1.6"${config.lineSpacing == 1.6 || !config.lineSpacing ? ' selected' : ''}>Normal (1.6)</option>
          <option value="1.8"${config.lineSpacing == 1.8 ? ' selected' : ''}>Relaxed (1.8)</option>
          <option value="2.0"${config.lineSpacing == 2.0 ? ' selected' : ''}>Double (2.0)</option>
        </select>
      </div>
      <div class="setting-row">
        <div class="setting-info">
          <div class="setting-label">Spellcheck</div>
        </div>
        <button class="toggle${config.spellcheck !== false ? ' active' : ''}" id="toggle-spellcheck"></button>
      </div>
      <div class="setting-row">
        <div class="setting-info">
          <div class="setting-label">Grammar Check</div>
        </div>
        <button class="toggle${config.grammarCheck !== false ? ' active' : ''}" id="toggle-grammar"></button>
      </div>
      <div class="setting-row">
        <div class="setting-info">
          <div class="setting-label">Auto-Save Interval</div>
          <div class="setting-description">${config.autoSaveInterval || 500}ms</div>
        </div>
        <input type="range" class="settings-slider" id="setting-autosave" min="200" max="5000" step="100" value="${config.autoSaveInterval || 500}" />
      </div>
      <div class="setting-row">
        <div class="setting-info">
          <div class="setting-label">Markdown Shortcuts</div>
          <div class="setting-description">Convert markdown syntax as you type</div>
        </div>
        <button class="toggle${config.markdownShortcuts !== false ? ' active' : ''}" id="toggle-markdown"></button>
      </div>
      <div class="setting-row">
        <div class="setting-info">
          <div class="setting-label">Show Word Count</div>
        </div>
        <button class="toggle${config.showWordCount !== false ? ' active' : ''}" id="toggle-wordcount"></button>
      </div>
    </div>
  `;

  const fontSize = container.querySelector('#setting-font-size');
  fontSize.addEventListener('change', () => {
    const val = parseInt(fontSize.value, 10);
    updateConfig('editorFontSize', val);
    document.querySelector('#editor-container')?.style.setProperty('font-size', val + 'px');
  });

  const lineSpacing = container.querySelector('#setting-line-spacing');
  lineSpacing.addEventListener('change', () => {
    const val = parseFloat(lineSpacing.value);
    updateConfig('lineSpacing', val);
    document.querySelector('#editor-container')?.style.setProperty('line-height', val);
  });

  const autosave = container.querySelector('#setting-autosave');
  const autosaveDesc = container.querySelector('#setting-autosave')?.closest('.setting-row')?.querySelector('.setting-description');
  autosave.addEventListener('input', () => {
    if (autosaveDesc) autosaveDesc.textContent = autosave.value + 'ms';
  });
  autosave.addEventListener('change', () => {
    updateConfig('autoSaveInterval', parseInt(autosave.value, 10));
  });

  setupToggle(container, '#toggle-spellcheck', 'spellcheck');
  setupToggle(container, '#toggle-grammar', 'grammarCheck');
  setupToggle(container, '#toggle-markdown', 'markdownShortcuts');
  setupToggle(container, '#toggle-wordcount', 'showWordCount');
}

// ---- Sounds ----

function renderSounds(container) {
  const config = getConfig();
  const sounds = ['click', 'pop', 'swoosh', 'ding', 'tap'];

  container.innerHTML = `
    <div class="settings-section">
      <h3>Sounds</h3>
      <div class="setting-row">
        <div class="setting-info">
          <div class="setting-label">Enable Sounds</div>
        </div>
        <button class="toggle${config.soundEnabled !== false ? ' active' : ''}" id="toggle-sound-master"></button>
      </div>
      <div class="setting-row">
        <div class="setting-info">
          <div class="setting-label">Volume</div>
          <div class="setting-description">${config.soundVolume || 30}%</div>
        </div>
        <input type="range" class="settings-slider" id="setting-volume" min="0" max="100" step="5" value="${config.soundVolume || 30}" />
      </div>
      ${sounds.map(s => `
        <div class="sound-row">
          <div class="setting-info">
            <div class="setting-label">${s.charAt(0).toUpperCase() + s.slice(1)}</div>
          </div>
          <button class="sound-preview-btn" data-sound="${s}">Preview</button>
          <button class="toggle${soundEngine.toggles[s] !== false ? ' active' : ''}" data-sound-toggle="${s}"></button>
        </div>
      `).join('')}
    </div>
  `;

  const masterToggle = container.querySelector('#toggle-sound-master');
  masterToggle.addEventListener('click', () => {
    const on = !masterToggle.classList.contains('active');
    masterToggle.classList.toggle('active', on);
    soundEngine.setEnabled(on);
    updateConfig('soundEnabled', on);
  });

  const volumeSlider = container.querySelector('#setting-volume');
  const volDesc = volumeSlider?.closest('.setting-row')?.querySelector('.setting-description');
  volumeSlider.addEventListener('input', () => {
    if (volDesc) volDesc.textContent = volumeSlider.value + '%';
    soundEngine.setVolume(parseInt(volumeSlider.value, 10) / 100);
  });
  volumeSlider.addEventListener('change', () => {
    updateConfig('soundVolume', parseInt(volumeSlider.value, 10));
  });

  container.querySelectorAll('.sound-preview-btn').forEach(btn => {
    btn.addEventListener('click', () => soundEngine.play(btn.dataset.sound));
  });

  container.querySelectorAll('[data-sound-toggle]').forEach(btn => {
    btn.addEventListener('click', () => {
      const name = btn.dataset.soundToggle;
      const on = !btn.classList.contains('active');
      btn.classList.toggle('active', on);
      soundEngine.setToggle(name, on);
    });
  });
}

// ---- Keyboard Shortcuts ----

function renderShortcuts(container) {
  const shortcuts = getShortcuts();
  const defaults = getDefaultShortcuts();
  const names = {
    'new-note': 'New Note',
    'trash-note': 'Trash Note',
    'find': 'Find',
    'find-replace': 'Find & Replace',
    'search-all': 'Search All Notes',
    'save': 'Save',
    'settings': 'Settings',
    'cycle-theme': 'Cycle Theme',
    'export-pdf': 'Export PDF',
    'heading1': 'Heading 1',
    'heading2': 'Heading 2',
    'heading3': 'Heading 3',
    'normal-text': 'Normal Text',
  };

  container.innerHTML = `
    <div class="settings-section">
      <h3>Keyboard Shortcuts</h3>
      ${Object.entries(shortcuts).map(([id, s]) => `
        <div class="shortcut-row" data-shortcut-id="${id}">
          <span class="shortcut-name">${names[id] || id}</span>
          <span class="shortcut-keys${capturingShortcut === id ? ' capturing' : ''}" id="shortcut-keys-${id}">${capturingShortcut === id ? 'Press keys...' : escHtml(s.keys)}</span>
          <button class="shortcut-edit-btn" data-shortcut="${id}">${capturingShortcut === id ? 'Cancel' : 'Edit'}</button>
        </div>
      `).join('')}
    </div>
  `;

  container.querySelectorAll('.shortcut-edit-btn').forEach(btn => {
    btn.addEventListener('click', () => {
      const id = btn.dataset.shortcut;
      if (capturingShortcut === id) {
        capturingShortcut = null;
        renderShortcuts(container);
        return;
      }
      capturingShortcut = id;
      renderShortcuts(container);
      startCapture(id, container);
    });
  });
}

function startCapture(id, container) {
  function handler(e) {
    e.preventDefault();
    e.stopPropagation();

    if (e.key === 'Escape') {
      document.removeEventListener('keydown', handler, true);
      capturingShortcut = null;
      renderShortcuts(container);
      return;
    }

    // Ignore lone modifier keys
    if (['Control', 'Shift', 'Alt', 'Meta'].includes(e.key)) return;

    const parts = [];
    if (e.ctrlKey) parts.push('Ctrl');
    if (e.shiftKey) parts.push('Shift');
    if (e.altKey) parts.push('Alt');

    let key = e.key;
    if (key === ' ') key = 'Space';
    else if (key.length === 1) key = key.toUpperCase();
    else if (key === 'Delete') key = 'Delete';
    parts.push(key);

    const combo = parts.join('+');
    document.removeEventListener('keydown', handler, true);
    capturingShortcut = null;
    rebindShortcut(id, combo);
    renderShortcuts(container);
  }

  document.addEventListener('keydown', handler, true);
}

// ---- Storage ----

function renderStorage(container) {
  const bp = storage.getBasePath() || 'Unknown';
  container.innerHTML = `
    <div class="settings-section">
      <h3>Storage</h3>
      <div class="setting-row">
        <div class="setting-info">
          <div class="setting-label">Notes Location</div>
          <div class="setting-description">${escHtml(bp)}</div>
        </div>
      </div>
      <div class="storage-stats" id="storage-stats">
        <div class="stat-item"><div class="stat-value">--</div><div class="stat-label">Notes</div></div>
        <div class="stat-item"><div class="stat-value">--</div><div class="stat-label">Folders</div></div>
        <div class="stat-item"><div class="stat-value">--</div><div class="stat-label">Size</div></div>
      </div>
      <div class="storage-actions">
        <button class="storage-btn" id="storage-export-all">Export All</button>
        <button class="storage-btn" id="storage-import">Import Notes</button>
      </div>
    </div>
  `;

  // Load stats async
  storage.getStorageStats().then(stats => {
    const el = document.getElementById('storage-stats');
    if (!el) return;
    const sizeKB = (stats.totalSize / 1024).toFixed(1);
    el.innerHTML = `
      <div class="stat-item"><div class="stat-value">${stats.noteCount}</div><div class="stat-label">Notes</div></div>
      <div class="stat-item"><div class="stat-value">${stats.folderCount}</div><div class="stat-label">Folders</div></div>
      <div class="stat-item"><div class="stat-value">${sizeKB} KB</div><div class="stat-label">Size</div></div>
    `;
  });

  container.querySelector('#storage-import')?.addEventListener('click', async () => {
    const activeFolder = store.get('activeFolder') || 'uncategorized';
    await storage.importMultiple(activeFolder);
  });
}

// ---- About ----

function renderAbout(container) {
  container.innerHTML = `
    <div class="about-section">
      <div class="app-name">Pealie Notes</div>
      <div class="app-version">Version 1.0.0</div>
      <div class="app-credits">
        A fast, beautiful note-taking app.<br>
        Built with Electron.<br>
        Made by Pealie.
      </div>
    </div>
  `;
}

// ---- Helpers ----

function setupToggle(container, selector, configKey) {
  const el = container.querySelector(selector);
  if (!el) return;
  el.addEventListener('click', () => {
    const on = !el.classList.contains('active');
    el.classList.toggle('active', on);
    updateConfig(configKey, on);
  });
}

function escHtml(str) {
  const div = document.createElement('div');
  div.textContent = str || '';
  return div.innerHTML;
}
