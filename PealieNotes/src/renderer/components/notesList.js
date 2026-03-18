import { store } from '../store.js';
import { soundEngine } from '../sounds.js';
import { showContextMenu } from './contextMenu.js';
import { showConfirm, showPrompt } from './modal.js';
import * as storage from '../storage.js';
import { formatDate, truncate } from '../utils/format.js';
import { debounce } from '../utils/debounce.js';
import { showTemplatePicker } from './templates.js';

let currentSort = 'modified'; // modified, created, title, manual

export function initNotesList() {
  const panel = document.getElementById('notes-list');

  panel.innerHTML = `
    <div class="notes-header">
      <div class="search-container">
        <span class="search-icon">🔍</span>
        <input type="text" class="search-input" placeholder="Search notes...">
      </div>
      <div class="notes-header-actions">
        <select class="sort-select">
          <option value="modified">Date Modified</option>
          <option value="created">Date Created</option>
          <option value="title">Title A-Z</option>
        </select>
        <button class="new-note-btn">+ New Note</button>
      </div>
    </div>
    <div class="notes-container"></div>
  `;

  // Search
  const searchInput = panel.querySelector('.search-input');
  const debouncedSearch = debounce((query) => {
    store.set('searchQuery', query);
    renderNotes();
  }, 150);
  searchInput.addEventListener('input', () => debouncedSearch(searchInput.value));

  // Sort
  panel.querySelector('.sort-select').addEventListener('change', (e) => {
    currentSort = e.target.value;
    renderNotes();
  });

  // New Note — show template picker
  const newNoteBtn = panel.querySelector('.new-note-btn');
  newNoteBtn.onclick = () => {
    showTemplatePicker(newNoteBtn);
  };

  // Subscribe to store changes
  store.on('activeFolder', renderNotes);
  store.on('notes', renderNotes);
  store.on('activeNote', updateActiveNote);
  store.on('searchQuery', renderNotes);

  renderNotes();
}

function renderNotes() {
  const container = document.querySelector('.notes-container');
  const activeFolder = store.get('activeFolder');
  const searchQuery = store.get('searchQuery').toLowerCase();
  let notes = store.get('notes');

  // Filter by folder
  if (activeFolder === 'all') {
    // all non-trashed notes
  } else if (activeFolder === 'favorites') {
    notes = notes.filter(n => n.favorited);
  } else if (activeFolder === 'trash') {
    // TODO: load from trash separately
    notes = [];
  } else if (activeFolder.startsWith('tag:')) {
    const tagId = activeFolder.slice(4);
    notes = notes.filter(n => (n.tags || []).includes(tagId));
  } else {
    notes = notes.filter(n => n.folderId === activeFolder);
  }

  // Filter by search
  if (searchQuery) {
    notes = notes.filter(n => n.title.toLowerCase().includes(searchQuery));
  }

  // Sort
  const pinned = notes.filter(n => n.pinned);
  const unpinned = notes.filter(n => !n.pinned);

  const sortFn = {
    modified: (a, b) => new Date(b.modifiedAt) - new Date(a.modifiedAt),
    created: (a, b) => new Date(b.createdAt) - new Date(a.createdAt),
    title: (a, b) => a.title.localeCompare(b.title)
  }[currentSort];

  pinned.sort(sortFn);
  unpinned.sort(sortFn);
  notes = [...pinned, ...unpinned];

  if (notes.length === 0) {
    container.innerHTML = `<div class="empty-state"><span style="font-size:32px">📝</span><p>No notes yet</p><p style="font-size:12px">Click "+ New Note" to get started</p></div>`;
    return;
  }

  container.innerHTML = '';
  notes.forEach(note => {
    const card = document.createElement('div');
    card.className = 'note-card' + (note.id === store.get('activeNote') ? ' active' : '');
    card.dataset.noteId = note.id;
    card.dataset.folderId = note.folderId;
    card.draggable = true;

    const tags = (note.tags || []).map(tagId => {
      const allTags = store.get('config').tags || [];
      const tag = allTags.find(t => t.id === tagId);
      return tag ? `<span class="tag-pill" style="background:${tag.color}33;color:${tag.color}">${tag.name}</span>` : '';
    }).join('');

    card.innerHTML = `
      ${note.pinned ? '<span class="note-pin">📌</span>' : ''}
      ${note.favorited ? '<span class="note-star">⭐</span>' : ''}
      <div class="note-title">${note.title || 'Untitled'}</div>
      <div class="note-preview">${truncate(note.preview || '', 80)}</div>
      <div class="note-date">${formatDate(new Date(note.modifiedAt))}</div>
      ${tags ? `<div class="note-tags">${tags}</div>` : ''}
    `;

    // Click to select
    card.onclick = () => {
      store.set('activeNote', note.id);
      soundEngine.play('click');
    };

    // Right-click context menu
    card.oncontextmenu = (e) => {
      e.preventDefault();
      showContextMenu(e.clientX, e.clientY, [
        { label: note.pinned ? 'Unpin' : 'Pin', action: async () => {
          await storage.saveNoteMetadata(note.id, note.folderId, { ...note, pinned: !note.pinned });
          const notes = await storage.loadAllNotes();
          store.set('notes', notes);
        }},
        { label: note.favorited ? 'Unfavorite' : 'Favorite', action: async () => {
          await storage.saveNoteMetadata(note.id, note.folderId, { ...note, favorited: !note.favorited });
          const notes = await storage.loadAllNotes();
          store.set('notes', notes);
        }},
        { separator: true },
        { label: 'Add Tag', action: () => showTagPicker(note) },
        { separator: true },
        { label: 'Export as HTML', action: () => storage.exportAsHTML(note.id, note.folderId) },
        { label: 'Export as Text', action: () => storage.exportAsText(note.id, note.folderId) },
        { label: 'Export as Markdown', action: () => storage.exportAsMarkdown(note.id, note.folderId) },
        { label: 'Export as PDF', action: () => storage.exportAsPDF(note.id, note.folderId) },
        { separator: true },
        { label: 'Duplicate', action: async () => {
          await storage.duplicateNote(note.id, note.folderId, note.folderId);
          const notes = await storage.loadAllNotes();
          store.set('notes', notes);
          soundEngine.play('pop');
        }},
        { separator: true },
        { label: 'Move to Trash', action: async () => {
          if (await showConfirm(`Move "${note.title || 'Untitled'}" to trash?`)) {
            await storage.trashNote(note.id, note.folderId);
            const notes = await storage.loadAllNotes();
            store.set('notes', notes);
            if (store.get('activeNote') === note.id) store.set('activeNote', null);
            soundEngine.play('swoosh');
          }
        }}
      ]);
    };

    // Drag start
    card.addEventListener('dragstart', (e) => {
      e.dataTransfer.setData('text/note-id', note.id);
      e.dataTransfer.setData('text/from-folder', note.folderId);
      card.style.opacity = '0.5';
    });
    card.addEventListener('dragend', () => { card.style.opacity = '1'; });

    container.appendChild(card);
  });
}

function updateActiveNote() {
  document.querySelectorAll('.note-card').forEach(card => {
    card.classList.toggle('active', card.dataset.noteId === store.get('activeNote'));
  });
}

const TAG_COLORS = ['#E81123', '#FF6900', '#FCB900', '#00D084', '#0693E3', '#AB149E', '#8B5CF6', '#666666'];

async function showTagPicker(note) {
  const config = store.get('config') || {};
  const tags = config.tags || [];
  const noteTags = note.tags || [];

  const body = document.createElement('div');
  body.className = 'tag-picker-body';

  if (tags.length > 0) {
    tags.forEach(tag => {
      const row = document.createElement('label');
      row.className = 'tag-picker-row';
      const checked = noteTags.includes(tag.id) ? 'checked' : '';
      row.innerHTML = `
        <input type="checkbox" data-tag-id="${tag.id}" ${checked}>
        <span class="tag-dot" style="background:${tag.color}"></span>
        <span>${tag.name}</span>
      `;
      body.appendChild(row);
    });
  } else {
    body.innerHTML = '<p style="color:var(--text-secondary);margin:0 0 8px">No tags yet</p>';
  }

  const newBtn = document.createElement('button');
  newBtn.className = 'modal-btn modal-btn-secondary';
  newBtn.textContent = '+ New Tag';
  newBtn.style.marginTop = '8px';
  newBtn.onclick = async () => {
    const name = await showPrompt('Tag name:', { title: 'New Tag' });
    if (!name) return;
    const color = TAG_COLORS[tags.length % TAG_COLORS.length];
    const tag = await storage.createTag(name, color);
    const updatedConfig = await storage.loadConfig();
    store.set('config', updatedConfig);
    await storage.addTagToNote(note.id, note.folderId, tag.id);
    const notes = await storage.loadAllNotes();
    store.set('notes', notes);
    soundEngine.play('pop');
  };
  body.appendChild(newBtn);

  const { showModal } = await import('./modal.js');
  const result = await showModal({
    title: 'Tags',
    body,
    buttons: [
      { id: 'cancel', label: 'Cancel' },
      { id: 'save', label: 'Save', primary: true }
    ]
  });

  if (result === 'save') {
    const checkboxes = body.querySelectorAll('input[type="checkbox"]');
    for (const cb of checkboxes) {
      const tagId = cb.dataset.tagId;
      if (cb.checked && !noteTags.includes(tagId)) {
        await storage.addTagToNote(note.id, note.folderId, tagId);
      } else if (!cb.checked && noteTags.includes(tagId)) {
        await storage.removeTagFromNote(note.id, note.folderId, tagId);
      }
    }
    const notes = await storage.loadAllNotes();
    store.set('notes', notes);
    soundEngine.play('pop');
  }
}
