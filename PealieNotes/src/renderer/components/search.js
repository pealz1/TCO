import { store } from '../store.js';
import { soundEngine } from '../sounds.js';
import * as storage from '../storage.js';
import { debounce } from '../utils/debounce.js';

let searchPanel = null;
let globalMode = false;

function escapeHtml(str) {
  return str.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

function highlightQuery(text, query) {
  if (!query) return escapeHtml(text);
  const escaped = escapeHtml(text);
  const queryEscaped = escapeHtml(query);
  const re = new RegExp(`(${queryEscaped.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')})`, 'gi');
  return escaped.replace(re, '<mark class="search-highlight">$1</mark>');
}

async function searchAllNotes(query) {
  const results = await storage.searchNotes(query);
  return results.map(r => ({
    noteId: r.note.id,
    folderId: r.note.folderId,
    title: r.note.title,
    folderName: r.folder.name,
    context: r.context,
    query
  }));
}

function renderResults(results, query) {
  const container = searchPanel.querySelector('.search-results');
  container.innerHTML = '';

  if (results.length === 0) {
    container.innerHTML = '<div class="search-empty">No results found</div>';
    return;
  }

  results.forEach(r => {
    const item = document.createElement('div');
    item.className = 'search-result-item';
    item.innerHTML = `
      <div class="search-result-title">${highlightQuery(r.title || 'Untitled', query)}</div>
      <div class="search-result-folder">${escapeHtml(r.folderName)}</div>
      ${r.context ? `<div class="search-result-context">${highlightQuery(r.context, query)}</div>` : ''}
    `;

    item.onclick = () => {
      soundEngine.play('click');
      store.set('activeFolder', r.folderId);
      store.set('activeNote', r.noteId);
      closeGlobalSearch();
    };

    container.appendChild(item);
  });
}

const debouncedSearch = debounce(async (query) => {
  if (!query || query.length < 2) {
    const container = searchPanel?.querySelector('.search-results');
    if (container) container.innerHTML = '';
    return;
  }
  const results = await searchAllNotes(query);
  renderResults(results, query);
}, 200);

export function openGlobalSearch() {
  if (searchPanel) {
    searchPanel.classList.add('visible');
    const input = searchPanel.querySelector('.global-search-input');
    input?.focus();
    input?.select();
    return;
  }

  searchPanel = document.createElement('div');
  searchPanel.className = 'global-search-panel visible';
  searchPanel.innerHTML = `
    <div class="global-search-header">
      <span class="global-search-icon">\uD83D\uDD0D</span>
      <input type="text" class="global-search-input" placeholder="Search all notes..." />
      <button class="global-search-close">\u2715</button>
    </div>
    <div class="search-results"></div>
  `;

  // Insert as overlay in the editor panel
  document.getElementById('editor-panel').appendChild(searchPanel);

  const input = searchPanel.querySelector('.global-search-input');
  input.addEventListener('input', () => {
    debouncedSearch(input.value);
  });

  input.addEventListener('keydown', (e) => {
    if (e.key === 'Escape') {
      e.preventDefault();
      closeGlobalSearch();
    }
  });

  searchPanel.querySelector('.global-search-close').onclick = () => {
    soundEngine.play('click');
    closeGlobalSearch();
  };

  input.focus();
  globalMode = true;
}

export function closeGlobalSearch() {
  if (searchPanel) {
    searchPanel.classList.remove('visible');
  }
  globalMode = false;
}

export function initSearch() {
  document.addEventListener('keydown', (e) => {
    if (e.ctrlKey && e.shiftKey && e.key === 'F') {
      e.preventDefault();
      openGlobalSearch();
    }
  });
}
