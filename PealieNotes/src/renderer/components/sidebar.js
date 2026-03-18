import { store } from '../store.js';
import { soundEngine } from '../sounds.js';
import { showContextMenu } from './contextMenu.js';
import { showConfirm, showPrompt } from './modal.js';
import * as storage from '../storage.js';

export function initSidebar() {
  const sidebar = document.getElementById('sidebar');

  // Render initial structure
  sidebar.innerHTML = `
    <div class="sidebar-header">
      <button class="sidebar-collapse-btn" title="Toggle sidebar">☰</button>
      <span class="sidebar-label">Folders</span>
    </div>
    <div class="smart-folders">
      <div class="smart-folder-item active" data-folder="all">
        <span class="sf-icon">📋</span>All Notes
        <span class="folder-count"></span>
      </div>
      <div class="smart-folder-item" data-folder="favorites">
        <span class="sf-icon">⭐</span>Favorites
        <span class="folder-count"></span>
      </div>
    </div>
    <div class="folder-tree"></div>
    <button class="new-folder-btn">+ New Folder</button>
    <div class="trash-item" data-folder="trash">
      <span class="sf-icon">🗑</span>Trash
      <span class="folder-count"></span>
    </div>
  `;

  // Wire collapse button
  sidebar.querySelector('.sidebar-collapse-btn').onclick = () => {
    const collapsed = !store.get('sidebarCollapsed');
    store.set('sidebarCollapsed', collapsed);
    sidebar.classList.toggle('collapsed', collapsed);
    soundEngine.play('click');
  };

  // Wire smart folders and trash
  sidebar.querySelectorAll('.smart-folder-item, .trash-item').forEach(el => {
    el.onclick = () => {
      store.set('activeFolder', el.dataset.folder);
      soundEngine.play('click');
    };
  });

  // New folder button
  sidebar.querySelector('.new-folder-btn').onclick = async () => {
    const name = await showPrompt('Folder name:', { title: 'New Folder', placeholder: 'My Folder' });
    if (name) {
      await storage.createFolder(name);
      soundEngine.play('pop');
      renderFolderTree();
    }
  };

  // Subscribe to store changes
  store.on('activeFolder', renderActiveState);
  store.on('folders', renderFolderTree);
  store.on('notes', updateCounts);

  renderFolderTree();
}

function renderFolderTree() {
  const tree = document.querySelector('.folder-tree');
  const folders = store.get('folders');
  tree.innerHTML = '';

  // Build nested tree from flat array
  const rootFolders = folders.filter(f => !f.parentId).sort((a,b) => a.order - b.order);
  rootFolders.forEach(folder => renderFolderItem(tree, folder, folders, 0));
}

function renderFolderItem(container, folder, allFolders, depth) {
  const children = allFolders.filter(f => f.parentId === folder.id).sort((a,b) => a.order - b.order);
  const hasChildren = children.length > 0;
  const isActive = store.get('activeFolder') === folder.id;

  const item = document.createElement('div');
  item.className = 'folder-item' + (isActive ? ' active' : '');
  item.style.paddingLeft = (16 + depth * 16) + 'px';
  item.dataset.folderId = folder.id;

  item.innerHTML = `
    <span class="folder-chevron ${hasChildren ? '' : 'invisible'} ${folder.expanded ? 'expanded' : ''}">▶</span>
    <span class="folder-color-dot" style="background:${folder.color || '#666'}"></span>
    <span class="folder-name">${folder.name}</span>
    <span class="folder-count">${getNoteCount(folder.id)}</span>
  `;

  // Click to select
  item.onclick = (e) => {
    if (e.target.classList.contains('folder-chevron')) {
      folder.expanded = !folder.expanded;
      renderFolderTree();
      return;
    }
    store.set('activeFolder', folder.id);
    soundEngine.play('click');
  };

  // Right-click context menu
  item.oncontextmenu = (e) => {
    e.preventDefault();
    showContextMenu(e.clientX, e.clientY, [
      { label: 'New Subfolder', action: async () => {
        const name = await showPrompt('Subfolder name:');
        if (name) { await storage.createFolder(name, folder.id); renderFolderTree(); }
      }},
      { label: 'Rename', action: async () => {
        const name = await showPrompt('Rename folder:', { defaultValue: folder.name });
        if (name) { await storage.renameFolder(folder.id, name); renderFolderTree(); }
      }},
      { separator: true },
      { label: 'Delete', action: async () => {
        if (await showConfirm(`Delete "${folder.name}" and all its notes?`)) {
          await storage.deleteFolder(folder.id);
          store.set('activeFolder', 'all');
          renderFolderTree();
        }
      }}
    ]);
  };

  // Drag & drop target
  item.addEventListener('dragover', (e) => { e.preventDefault(); item.classList.add('drop-target'); });
  item.addEventListener('dragleave', () => { item.classList.remove('drop-target'); });
  item.addEventListener('drop', async (e) => {
    e.preventDefault();
    item.classList.remove('drop-target');
    const noteId = e.dataTransfer.getData('text/note-id');
    const fromFolder = e.dataTransfer.getData('text/from-folder');
    if (noteId && fromFolder !== folder.id) {
      await storage.moveNote(noteId, fromFolder, folder.id);
      soundEngine.play('pop');
      // Refresh notes
      const notes = await storage.loadAllNotes();
      store.set('notes', notes);
    }
  });

  container.appendChild(item);

  // Render children if expanded
  if (hasChildren && folder.expanded) {
    children.forEach(child => renderFolderItem(container, child, allFolders, depth + 1));
  }
}

function renderActiveState() {
  document.querySelectorAll('.folder-item, .smart-folder-item, .trash-item').forEach(el => {
    el.classList.toggle('active', (el.dataset.folderId || el.dataset.folder) === store.get('activeFolder'));
  });
}

function getNoteCount(folderId) {
  return store.get('notes').filter(n => n.folderId === folderId).length;
}

function updateCounts() { renderFolderTree(); }
