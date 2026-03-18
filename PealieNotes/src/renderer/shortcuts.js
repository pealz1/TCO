import { store } from './store.js';
import * as storage from './storage.js';

const defaultShortcuts = {
  'new-note': { keys: 'Ctrl+N', action: null },
  'trash-note': { keys: 'Ctrl+Delete', action: null },
  'find': { keys: 'Ctrl+F', action: null },
  'find-replace': { keys: 'Ctrl+H', action: null },
  'search-all': { keys: 'Ctrl+Shift+F', action: null },
  'save': { keys: 'Ctrl+S', action: null },
  'settings': { keys: 'Ctrl+,', action: null },
  'cycle-theme': { keys: 'Ctrl+Shift+T', action: null },
  'export-pdf': { keys: 'Ctrl+P', action: null },
  'heading1': { keys: 'Ctrl+1', action: null },
  'heading2': { keys: 'Ctrl+2', action: null },
  'heading3': { keys: 'Ctrl+3', action: null },
  'normal-text': { keys: 'Ctrl+0', action: null },
};

const shortcuts = {};

function parseCombo(str) {
  const parts = str.split('+').map(p => p.trim());
  const combo = { ctrl: false, shift: false, alt: false, key: '' };
  for (const p of parts) {
    const lower = p.toLowerCase();
    if (lower === 'ctrl') combo.ctrl = true;
    else if (lower === 'shift') combo.shift = true;
    else if (lower === 'alt') combo.alt = true;
    else combo.key = lower;
  }
  return combo;
}

function matchEvent(event, combo) {
  if (event.ctrlKey !== combo.ctrl) return false;
  if (event.shiftKey !== combo.shift) return false;
  if (event.altKey !== combo.alt) return false;
  const evKey = event.key.toLowerCase();
  if (combo.key === 'delete') return evKey === 'delete';
  if (combo.key === ',') return evKey === ',';
  return evKey === combo.key;
}

function handleKeydown(event) {
  for (const id of Object.keys(shortcuts)) {
    const entry = shortcuts[id];
    if (!entry.action) continue;
    const combo = parseCombo(entry.keys);
    if (matchEvent(event, combo)) {
      event.preventDefault();
      event.stopPropagation();
      entry.action();
      return;
    }
  }
}

export function initShortcuts() {
  // Load defaults
  for (const [id, def] of Object.entries(defaultShortcuts)) {
    shortcuts[id] = { keys: def.keys, action: def.action };
  }

  // Load custom bindings from config
  const config = store.get('config');
  if (config && config.shortcuts) {
    for (const [id, keys] of Object.entries(config.shortcuts)) {
      if (shortcuts[id]) {
        shortcuts[id].keys = keys;
      }
    }
  }

  document.addEventListener('keydown', handleKeydown, true);
}

export function registerShortcut(id, action) {
  if (!shortcuts[id]) {
    shortcuts[id] = { keys: '', action: null };
  }
  shortcuts[id].action = action;
}

export async function rebindShortcut(id, newKeys) {
  if (!shortcuts[id]) return;
  shortcuts[id].keys = newKeys;

  // Persist to config
  const config = await storage.loadConfig();
  if (!config.shortcuts) config.shortcuts = {};
  config.shortcuts[id] = newKeys;
  await storage.saveConfig(config);
  store.set('config', config);
}

export function getShortcuts() {
  const result = {};
  for (const [id, entry] of Object.entries(shortcuts)) {
    result[id] = { keys: entry.keys, hasAction: !!entry.action };
  }
  return result;
}

export function getDefaultShortcuts() {
  return { ...defaultShortcuts };
}
