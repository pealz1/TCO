import { generateId } from './utils/ids.js';

const MAX_VERSIONS = 10;

const DEFAULT_CONFIG = {
  displayName: '',
  theme: 'light',
  accentColor: '#007AFF',
  firstLaunch: true,
  editorFont: 'Segoe UI',
  editorFontSize: 16,
  lineSpacing: 1.6,
  spellcheck: true,
  grammarCheck: true,
  autoSaveInterval: 500,
  markdownShortcuts: true,
  showWordCount: true,
  soundEnabled: true,
  soundVolume: 30,
  minimizeToTray: true,
  openLastNote: true,
  sidebarWidth: 220,
  notesListWidth: 280,
  editorZoom: 100,
  lastActiveFolder: 'all',
  lastActiveNote: null,
  shortcuts: {},
  tags: []
};

let basePath = null;

// --- path helpers ---

function join(...segments) {
  return window.api.joinPath(...segments);
}

function notesDir() {
  return join(basePath, 'notes');
}

function folderDir(folderId) {
  return join(basePath, 'notes', folderId);
}

function noteDir(folderId, noteId) {
  return join(basePath, 'notes', folderId, noteId);
}

function trashDir() {
  return join(basePath, 'trash');
}

function configPath() {
  return join(basePath, 'config.json');
}

// --- Initialization ---

export async function initStorage(path) {
  basePath = path;

  const dirs = [
    basePath,
    join(basePath, 'notes'),
    join(basePath, 'templates'),
    join(basePath, 'trash'),
  ];

  for (const dir of dirs) {
    if (!(await window.api.exists(dir))) {
      await window.api.mkdir(dir);
    }
  }

  // ensure uncategorized folder exists
  const uncatDir = folderDir('uncategorized');
  if (!(await window.api.exists(uncatDir))) {
    await window.api.mkdir(uncatDir);
    const folderMeta = {
      id: 'uncategorized',
      name: 'Uncategorized',
      color: '#666',
      order: 0,
      parentId: null,
      createdAt: new Date().toISOString()
    };
    await window.api.writeFile(join(uncatDir, 'folder.json'), JSON.stringify(folderMeta, null, 2));
  }
}

export function getBasePath() {
  return basePath;
}

// --- Folders ---

export async function loadFolders() {
  const notesPath = notesDir();
  const entries = await window.api.readDir(notesPath);
  const folders = [];

  for (const entry of entries) {
    if (!entry.isDirectory) continue;
    const metaPath = join(notesPath, entry.name, 'folder.json');
    if (!(await window.api.exists(metaPath))) continue;
    const raw = await window.api.readFile(metaPath);
    folders.push(JSON.parse(raw));
  }

  folders.sort((a, b) => (a.order ?? 0) - (b.order ?? 0));
  return folders;
}

export async function createFolder(name, parentId = null, color = '#666') {
  const id = generateId();
  const dir = folderDir(id);
  await window.api.mkdir(dir);

  const folder = {
    id,
    name,
    color,
    order: Date.now(),
    parentId,
    createdAt: new Date().toISOString()
  };

  await window.api.writeFile(join(dir, 'folder.json'), JSON.stringify(folder, null, 2));
  return folder;
}

export async function renameFolder(id, name) {
  return updateFolder(id, { name });
}

export async function updateFolder(id, updates) {
  const metaPath = join(folderDir(id), 'folder.json');
  const raw = await window.api.readFile(metaPath);
  const folder = { ...JSON.parse(raw), ...updates };
  await window.api.writeFile(metaPath, JSON.stringify(folder, null, 2));
  return folder;
}

export async function deleteFolder(id) {
  const src = folderDir(id);
  const dest = join(trashDir(), `folder_${id}_${Date.now()}`);
  await window.api.rename(src, dest);
}

// --- Notes ---

export async function loadNotes(folderId) {
  const dir = folderDir(folderId);
  const entries = await window.api.readDir(dir);
  const notes = [];

  for (const entry of entries) {
    if (!entry.isDirectory) continue;
    const metaPath = join(dir, entry.name, 'note.json');
    if (!(await window.api.exists(metaPath))) continue;
    const raw = await window.api.readFile(metaPath);
    notes.push(JSON.parse(raw));
  }

  return notes;
}

export async function loadAllNotes() {
  const notesPath = notesDir();
  const entries = await window.api.readDir(notesPath);
  const all = [];

  for (const entry of entries) {
    if (!entry.isDirectory) continue;
    const notes = await loadNotes(entry.name);
    all.push(...notes);
  }

  return all;
}

export async function createNote(folderId, template = null) {
  const id = generateId();
  const dir = noteDir(folderId, id);
  await window.api.mkdir(dir);
  await window.api.mkdir(join(dir, 'assets'));
  await window.api.mkdir(join(dir, 'versions'));

  const now = new Date().toISOString();
  const note = {
    id,
    folderId,
    title: 'Untitled',
    createdAt: now,
    modifiedAt: now,
    tags: [],
    pinned: false,
    favorited: false
  };

  await window.api.writeFile(join(dir, 'note.json'), JSON.stringify(note, null, 2));
  await window.api.writeFile(join(dir, 'content.html'), template || '');
  return note;
}

export async function saveNoteContent(noteId, folderId, html) {
  const dir = noteDir(folderId, noteId);
  const contentPath = join(dir, 'content.html');

  // create a version of the previous content if it exists and differs
  if (await window.api.exists(contentPath)) {
    const existing = await window.api.readFile(contentPath);
    if (existing !== html) {
      const timestamp = Date.now();
      const rand = Math.random().toString(36).slice(2, 7);
      const versionPath = join(dir, 'versions', `${timestamp}_${rand}.html`);
      await window.api.writeFile(versionPath, existing);
      await pruneVersions(noteId, folderId);
    }
  }

  await window.api.writeFile(contentPath, html);

  // update modifiedAt
  const metaPath = join(dir, 'note.json');
  if (await window.api.exists(metaPath)) {
    const raw = await window.api.readFile(metaPath);
    const note = JSON.parse(raw);
    note.modifiedAt = new Date().toISOString();
    await window.api.writeFile(metaPath, JSON.stringify(note, null, 2));
  }
}

export async function saveNoteMetadata(noteId, folderId, metadata) {
  const metaPath = join(noteDir(folderId, noteId), 'note.json');
  const raw = await window.api.readFile(metaPath);
  const note = { ...JSON.parse(raw), ...metadata };
  await window.api.writeFile(metaPath, JSON.stringify(note, null, 2));
  return note;
}

export async function loadNoteContent(noteId, folderId) {
  const contentPath = join(noteDir(folderId, noteId), 'content.html');
  if (!(await window.api.exists(contentPath))) return '';
  return window.api.readFile(contentPath);
}

export async function trashNote(noteId, folderId) {
  const src = noteDir(folderId, noteId);
  const dest = join(trashDir(), `note_${noteId}_${Date.now()}`);
  await window.api.rename(src, dest);
}

export async function restoreNote(noteId, targetFolderId) {
  const entries = await window.api.readDir(trashDir());
  const match = entries.find(e => e.name.startsWith(`note_${noteId}_`));
  if (!match) throw new Error(`Note ${noteId} not found in trash`);
  const src = join(trashDir(), match.name);
  const dest = noteDir(targetFolderId, noteId);
  await window.api.rename(src, dest);
}

export async function permanentlyDelete(noteId) {
  const entries = await window.api.readDir(trashDir());
  const match = entries.find(e => e.name.startsWith(`note_${noteId}_`));
  if (!match) throw new Error(`Note ${noteId} not found in trash`);
  await window.api.deleteFile(join(trashDir(), match.name));
}

export async function duplicateNote(noteId, folderId, targetFolderId) {
  const newId = generateId();
  const src = noteDir(folderId, noteId);
  const dest = noteDir(targetFolderId, newId);
  await window.api.copyDir(src, dest);

  // rewrite note.json with the new id and folderId
  const metaPath = join(dest, 'note.json');
  const raw = await window.api.readFile(metaPath);
  const note = JSON.parse(raw);
  const now = new Date().toISOString();
  const newNote = {
    ...note,
    id: newId,
    folderId: targetFolderId,
    createdAt: now,
    modifiedAt: now
  };
  await window.api.writeFile(metaPath, JSON.stringify(newNote, null, 2));
  return newNote;
}

export async function moveNote(noteId, fromFolderId, toFolderId) {
  const src = noteDir(fromFolderId, noteId);
  const dest = noteDir(toFolderId, noteId);
  await window.api.rename(src, dest);

  const metaPath = join(dest, 'note.json');
  const raw = await window.api.readFile(metaPath);
  const note = JSON.parse(raw);
  note.folderId = toFolderId;
  await window.api.writeFile(metaPath, JSON.stringify(note, null, 2));
}

// --- Versioning ---

export async function loadVersions(noteId, folderId) {
  const versionsPath = join(noteDir(folderId, noteId), 'versions');
  if (!(await window.api.exists(versionsPath))) return [];
  const entries = await window.api.readDir(versionsPath);
  return entries
    .filter(e => !e.isDirectory && e.name.endsWith('.html'))
    .map(e => ({
      filename: e.name,
      timestamp: parseInt(e.name.split('_')[0], 10)
    }))
    .sort((a, b) => b.timestamp - a.timestamp);
}

export async function restoreVersion(noteId, folderId, versionFile) {
  const versionsPath = join(noteDir(folderId, noteId), 'versions');
  const versionContent = await window.api.readFile(join(versionsPath, versionFile));
  await saveNoteContent(noteId, folderId, versionContent);
}

async function pruneVersions(noteId, folderId) {
  const versions = await loadVersions(noteId, folderId);
  if (versions.length <= MAX_VERSIONS) return;
  const toDelete = versions.slice(MAX_VERSIONS);
  const versionsPath = join(noteDir(folderId, noteId), 'versions');
  for (const v of toDelete) {
    await window.api.deleteFile(join(versionsPath, v.filename));
  }
}

// --- Search ---

export async function searchNotes(query) {
  if (!query || !query.trim()) return [];
  const lower = query.toLowerCase();
  const folders = await loadFolders();
  const results = [];

  for (const folder of folders) {
    const notes = await loadNotes(folder.id);
    for (const note of notes) {
      const contentPath = join(noteDir(folder.id, note.id), 'content.html');
      if (!(await window.api.exists(contentPath))) continue;
      const html = await window.api.readFile(contentPath);
      const text = html.replace(/<[^>]+>/g, ' ').replace(/\s+/g, ' ').trim();
      const titleMatch = note.title.toLowerCase().includes(lower);
      const bodyLower = text.toLowerCase();
      const idx = bodyLower.indexOf(lower);
      if (titleMatch || idx !== -1) {
        let context = '';
        if (idx !== -1) {
          const start = Math.max(0, idx - 60);
          const end = Math.min(text.length, idx + query.length + 60);
          context = text.slice(start, end);
        }
        results.push({ note, folder, context });
      }
    }
  }

  return results;
}

// --- Config ---

export async function loadConfig() {
  const path = configPath();
  if (!(await window.api.exists(path))) {
    return { ...DEFAULT_CONFIG };
  }
  const raw = await window.api.readFile(path);
  return { ...DEFAULT_CONFIG, ...JSON.parse(raw) };
}

export async function saveConfig(config) {
  await window.api.writeFile(configPath(), JSON.stringify(config, null, 2));
}

// --- Stats ---

export async function getStorageStats() {
  const folders = await loadFolders();
  let noteCount = 0;
  let totalSize = 0;

  for (const folder of folders) {
    const notes = await loadNotes(folder.id);
    noteCount += notes.length;
    for (const note of notes) {
      const contentPath = join(noteDir(folder.id, note.id), 'content.html');
      if (await window.api.exists(contentPath)) {
        const s = await window.api.stat(contentPath);
        totalSize += s.size || 0;
      }
    }
  }

  return {
    noteCount,
    folderCount: folders.length,
    totalSize
  };
}
