import { store } from '../store.js';
import { soundEngine } from '../sounds.js';
import { showContextMenu } from './contextMenu.js';
import { showConfirm } from './modal.js';
import * as storage from '../storage.js';
import { formatDate, truncate } from '../utils/format.js';
import { debounce } from '../utils/debounce.js';

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

  // New Note
  panel.querySelector('.new-note-btn').onclick = async () => {
    const activeFolder = store.get('activeFolder');
    const folderId = (activeFolder === 'all' || activeFolder === 'favorites' || activeFolder === 'trash')
      ? 'uncategorized' : activeFolder;
    const note = await storage.createNote(folderId);
    const notes = await storage.loadAllNotes();
    store.set('notes', notes);
    store.set('activeNote', note.id);
    soundEngine.play('pop');
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
