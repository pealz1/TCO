import { store } from '../store.js';
import { soundEngine } from '../sounds.js';
import { showConfirm } from './modal.js';
import * as storage from '../storage.js';
import { getEditor } from './editor.js';

let panel = null;

function formatRelativeTime(timestamp) {
  const now = Date.now();
  const diff = now - timestamp;
  const seconds = Math.floor(diff / 1000);
  const minutes = Math.floor(seconds / 60);
  const hours = Math.floor(minutes / 60);
  const days = Math.floor(hours / 24);

  if (seconds < 60) return 'Just now';
  if (minutes < 60) return `${minutes} min ago`;
  if (hours < 24) return `${hours} hr ago`;
  if (days < 7) return `${days} day${days !== 1 ? 's' : ''} ago`;

  const d = new Date(timestamp);
  return d.toLocaleDateString('en-US', { month: 'short', day: 'numeric', year: 'numeric' });
}

async function loadAndRenderVersions() {
  const noteId = store.get('activeNote');
  if (!noteId) {
    renderEmpty('No note selected');
    return;
  }

  const note = store.get('notes').find(n => n.id === noteId);
  if (!note) {
    renderEmpty('Note not found');
    return;
  }

  const versions = await storage.loadVersions(noteId, note.folderId);
  const list = panel.querySelector('.version-list');
  list.innerHTML = '';

  if (versions.length === 0) {
    list.innerHTML = '<div class="version-empty">No previous versions yet.</div>';
    return;
  }

  versions.forEach(v => {
    const item = document.createElement('div');
    item.className = 'version-item';
    item.innerHTML = `
      <div class="version-time">${formatRelativeTime(v.timestamp)}</div>
      <div class="version-date">${new Date(v.timestamp).toLocaleString()}</div>
      <div class="version-actions">
        <button class="version-preview-btn">Preview</button>
        <button class="version-restore-btn">Restore</button>
      </div>
    `;

    item.querySelector('.version-preview-btn').onclick = async () => {
      soundEngine.play('click');
      const versionsPath = await window.api.joinPath(
        storage.getBasePath(), 'notes', note.folderId, noteId, 'versions', v.filename
      );
      const content = await window.api.readFile(versionsPath);
      showPreview(content, v.timestamp);
    };

    item.querySelector('.version-restore-btn').onclick = async () => {
      if (await showConfirm('Restore this version? Current content will be saved as a version.')) {
        await storage.restoreVersion(noteId, note.folderId, v.filename);
        const content = await storage.loadNoteContent(noteId, note.folderId);
        const ed = getEditor();
        if (ed) ed.commands.setContent(content || '');
        soundEngine.play('pop');
        loadAndRenderVersions();
      }
    };

    list.appendChild(item);
  });
}

function showPreview(html, timestamp) {
  let preview = panel.querySelector('.version-preview');
  if (!preview) {
    preview = document.createElement('div');
    preview.className = 'version-preview';
    panel.appendChild(preview);
  }

  preview.innerHTML = `
    <div class="version-preview-header">
      <span>Version from ${new Date(timestamp).toLocaleString()}</span>
      <button class="version-preview-close">\u2715</button>
    </div>
    <div class="version-preview-content">${html}</div>
  `;
  preview.classList.add('visible');

  preview.querySelector('.version-preview-close').onclick = () => {
    preview.classList.remove('visible');
    soundEngine.play('click');
  };
}

function renderEmpty(msg) {
  const list = panel.querySelector('.version-list');
  list.innerHTML = `<div class="version-empty">${msg}</div>`;
}

export function openVersionHistory() {
  if (panel) {
    panel.classList.add('visible');
    loadAndRenderVersions();
    return;
  }

  panel = document.createElement('div');
  panel.className = 'version-panel visible';
  panel.innerHTML = `
    <div class="version-panel-header">
      <span class="version-panel-title">Version History</span>
      <button class="version-panel-close">\u2715</button>
    </div>
    <div class="version-list"></div>
  `;

  document.getElementById('editor-panel').appendChild(panel);

  panel.querySelector('.version-panel-close').onclick = () => {
    panel.classList.remove('visible');
    const preview = panel.querySelector('.version-preview');
    if (preview) preview.classList.remove('visible');
    soundEngine.play('click');
  };

  loadAndRenderVersions();
}

export function closeVersionHistory() {
  if (panel) panel.classList.remove('visible');
}
