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
// Use synchronous path joining (Node fs accepts forward slashes on Windows)

function join(...segments) {
  return segments.filter(Boolean).join('/');
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

// --- Tags ---

export async function createTag(name, color) {
  const config = await loadConfig();
  const id = 'tag_' + Date.now() + '_' + Math.random().toString(36).slice(2, 7);
  const tag = { id, name, color };
  config.tags = config.tags || [];
  config.tags.push(tag);
  await saveConfig(config);
  return tag;
}

export async function deleteTag(id) {
  const config = await loadConfig();
  config.tags = (config.tags || []).filter(t => t.id !== id);
  await saveConfig(config);
}

export async function addTagToNote(noteId, folderId, tagId) {
  const metaPath = join(noteDir(folderId, noteId), 'note.json');
  const raw = await window.api.readFile(metaPath);
  const note = JSON.parse(raw);
  note.tags = note.tags || [];
  if (!note.tags.includes(tagId)) {
    note.tags.push(tagId);
    await window.api.writeFile(metaPath, JSON.stringify(note, null, 2));
  }
  return note;
}

export async function removeTagFromNote(noteId, folderId, tagId) {
  const metaPath = join(noteDir(folderId, noteId), 'note.json');
  const raw = await window.api.readFile(metaPath);
  const note = JSON.parse(raw);
  note.tags = (note.tags || []).filter(t => t !== tagId);
  await window.api.writeFile(metaPath, JSON.stringify(note, null, 2));
  return note;
}

// --- Export / Import ---

export async function exportAsHTML(noteId, folderId) {
  const content = await loadNoteContent(noteId, folderId);
  const metaPath = join(noteDir(folderId, noteId), 'note.json');
  const raw = await window.api.readFile(metaPath);
  const note = JSON.parse(raw);

  const html = `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>${(note.title || 'Untitled').replace(/</g, '&lt;')}</title>
<style>
  body { font-family: 'Segoe UI', sans-serif; max-width: 800px; margin: 40px auto; padding: 0 20px; line-height: 1.6; color: #333; }
  h1, h2, h3 { color: #111; }
  code { background: #f4f4f4; padding: 2px 6px; border-radius: 3px; }
  pre { background: #f4f4f4; padding: 16px; border-radius: 6px; overflow-x: auto; }
  blockquote { border-left: 3px solid #ddd; margin-left: 0; padding-left: 16px; color: #666; }
  table { border-collapse: collapse; width: 100%; }
  td, th { border: 1px solid #ddd; padding: 8px; }
  th { background: #f4f4f4; }
</style>
</head>
<body>
${content}
</body>
</html>`;

  await window.api.showSaveDialog({
    defaultPath: (note.title || 'Untitled') + '.html',
    filters: [{ name: 'HTML', extensions: ['html'] }]
  }).then(async (result) => {
    if (result && !result.canceled && result.filePath) {
      await window.api.writeFile(result.filePath, html);
    }
  });
}

export async function exportAsText(noteId, folderId) {
  const content = await loadNoteContent(noteId, folderId);
  const text = content.replace(/<[^>]+>/g, ' ').replace(/\s+/g, ' ').trim();
  const metaPath = join(noteDir(folderId, noteId), 'note.json');
  const raw = await window.api.readFile(metaPath);
  const note = JSON.parse(raw);

  await window.api.showSaveDialog({
    defaultPath: (note.title || 'Untitled') + '.txt',
    filters: [{ name: 'Text', extensions: ['txt'] }]
  }).then(async (result) => {
    if (result && !result.canceled && result.filePath) {
      await window.api.writeFile(result.filePath, text);
    }
  });
}

export async function exportAsMarkdown(noteId, folderId) {
  const content = await loadNoteContent(noteId, folderId);
  const metaPath = join(noteDir(folderId, noteId), 'note.json');
  const raw = await window.api.readFile(metaPath);
  const note = JSON.parse(raw);

  // Basic HTML to Markdown conversion
  let md = content;
  md = md.replace(/<h1[^>]*>(.*?)<\/h1>/gi, '# $1\n\n');
  md = md.replace(/<h2[^>]*>(.*?)<\/h2>/gi, '## $1\n\n');
  md = md.replace(/<h3[^>]*>(.*?)<\/h3>/gi, '### $1\n\n');
  md = md.replace(/<strong>(.*?)<\/strong>/gi, '**$1**');
  md = md.replace(/<b>(.*?)<\/b>/gi, '**$1**');
  md = md.replace(/<em>(.*?)<\/em>/gi, '*$1*');
  md = md.replace(/<i>(.*?)<\/i>/gi, '*$1*');
  md = md.replace(/<u>(.*?)<\/u>/gi, '$1');
  md = md.replace(/<s>(.*?)<\/s>/gi, '~~$1~~');
  md = md.replace(/<a[^>]*href="([^"]*)"[^>]*>(.*?)<\/a>/gi, '[$2]($1)');
  md = md.replace(/<img[^>]*src="([^"]*)"[^>]*alt="([^"]*)"[^>]*\/?>/gi, '![$2]($1)');
  md = md.replace(/<img[^>]*src="([^"]*)"[^>]*\/?>/gi, '![]($1)');
  md = md.replace(/<blockquote[^>]*>(.*?)<\/blockquote>/gi, '> $1\n');
  md = md.replace(/<code>(.*?)<\/code>/gi, '`$1`');
  md = md.replace(/<pre[^>]*><code[^>]*>(.*?)<\/code><\/pre>/gis, '```\n$1\n```\n');
  md = md.replace(/<hr\s*\/?>/gi, '---\n\n');
  md = md.replace(/<li[^>]*>(.*?)<\/li>/gi, '- $1\n');
  md = md.replace(/<\/?(ul|ol|p|div|br\s*\/?|table|tr|td|th|thead|tbody)[^>]*>/gi, '\n');
  md = md.replace(/<[^>]+>/g, '');
  md = md.replace(/&amp;/g, '&').replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&nbsp;/g, ' ');
  md = md.replace(/\n{3,}/g, '\n\n').trim();

  await window.api.showSaveDialog({
    defaultPath: (note.title || 'Untitled') + '.md',
    filters: [{ name: 'Markdown', extensions: ['md'] }]
  }).then(async (result) => {
    if (result && !result.canceled && result.filePath) {
      await window.api.writeFile(result.filePath, md);
    }
  });
}

export async function exportAsPDF(noteId, folderId) {
  if (window.api.printToPDF) {
    const result = await window.api.printToPDF();
    if (result) {
      const metaPath = join(noteDir(folderId, noteId), 'note.json');
      const raw = await window.api.readFile(metaPath);
      const note = JSON.parse(raw);

      const saveResult = await window.api.showSaveDialog({
        defaultPath: (note.title || 'Untitled') + '.pdf',
        filters: [{ name: 'PDF', extensions: ['pdf'] }]
      });
      if (saveResult && !saveResult.canceled && saveResult.filePath) {
        await window.api.writeFile(saveResult.filePath, result);
      }
    }
  }
}

export async function importFile(filePath, folderId) {
  const content = await window.api.readFile(filePath);
  const ext = filePath.split('.').pop().toLowerCase();
  let html = '';

  if (ext === 'html' || ext === 'htm') {
    html = content;
  } else if (ext === 'md' || ext === 'markdown') {
    // Basic markdown to HTML
    html = content;
    html = html.replace(/^### (.*$)/gm, '<h3>$1</h3>');
    html = html.replace(/^## (.*$)/gm, '<h2>$1</h2>');
    html = html.replace(/^# (.*$)/gm, '<h1>$1</h1>');
    html = html.replace(/\*\*(.*?)\*\*/g, '<strong>$1</strong>');
    html = html.replace(/\*(.*?)\*/g, '<em>$1</em>');
    html = html.replace(/~~(.*?)~~/g, '<s>$1</s>');
    html = html.replace(/`(.*?)`/g, '<code>$1</code>');
    html = html.replace(/\[([^\]]+)\]\(([^)]+)\)/g, '<a href="$2">$1</a>');
    html = html.replace(/^- (.*$)/gm, '<li>$1</li>');
    html = html.replace(/(<li>.*<\/li>\n?)+/g, '<ul>$&</ul>');
    html = html.replace(/^---$/gm, '<hr>');
    html = html.replace(/\n\n/g, '</p><p>');
    html = '<p>' + html + '</p>';
  } else {
    // Plain text
    html = '<p>' + content.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/\n\n/g, '</p><p>').replace(/\n/g, '<br>') + '</p>';
  }

  const note = await createNote(folderId, html);
  return note;
}

export async function importMultiple(folderId) {
  const result = await window.api.showOpenDialog({
    properties: ['openFile', 'multiSelections'],
    filters: [
      { name: 'Documents', extensions: ['html', 'htm', 'md', 'txt', 'markdown'] }
    ]
  });

  if (!result || result.canceled || !result.filePaths) return [];

  const notes = [];
  for (const filePath of result.filePaths) {
    const note = await importFile(filePath, folderId);
    notes.push(note);
  }
  return notes;
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
