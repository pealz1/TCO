import { describe, it, expect, vi, beforeEach } from 'vitest';
import {
  initStorage,
  getBasePath,
  loadFolders,
  createFolder,
  renameFolder,
  updateFolder,
  deleteFolder,
  loadNotes,
  loadAllNotes,
  createNote,
  saveNoteContent,
  saveNoteMetadata,
  loadNoteContent,
  trashNote,
  restoreNote,
  permanentlyDelete,
  duplicateNote,
  moveNote,
  loadVersions,
  restoreVersion,
  searchNotes,
  loadConfig,
  saveConfig,
  getStorageStats
} from '../src/renderer/storage.js';

// --- mock window.api ---

function makeApi(fs = {}) {
  const store = { ...fs };

  const api = {
    joinPath: vi.fn((...segs) => segs.join('/')),
    exists: vi.fn(async (p) => Object.prototype.hasOwnProperty.call(store, p)),
    mkdir: vi.fn(async (p) => { store[p] = { __dir: true }; }),
    writeFile: vi.fn(async (p, c) => { store[p] = c; }),
    readFile: vi.fn(async (p) => {
      if (!Object.prototype.hasOwnProperty.call(store, p)) throw new Error(`ENOENT: ${p}`);
      return store[p];
    }),
    readDir: vi.fn(async (p) => {
      const prefix = p + '/';
      const names = new Set();
      for (const key of Object.keys(store)) {
        if (key.startsWith(prefix)) {
          const rest = key.slice(prefix.length);
          const name = rest.split('/')[0];
          if (name) names.add(name);
        }
      }
      return [...names].map(name => {
        const fullPath = prefix + name;
        return { name, isDirectory: !!(store[fullPath] && store[fullPath].__dir) };
      });
    }),
    rename: vi.fn(async (src, dest) => {
      const keys = Object.keys(store);
      for (const key of keys) {
        if (key === src || key.startsWith(src + '/')) {
          const newKey = dest + key.slice(src.length);
          store[newKey] = store[key];
          delete store[key];
        }
      }
    }),
    deleteFile: vi.fn(async (p) => {
      const keys = Object.keys(store);
      for (const key of keys) {
        if (key === p || key.startsWith(p + '/')) {
          delete store[key];
        }
      }
    }),
    copyDir: vi.fn(async (src, dest) => {
      const keys = Object.keys(store);
      for (const key of keys) {
        if (key === src || key.startsWith(src + '/')) {
          const newKey = dest + key.slice(src.length);
          store[newKey] = typeof store[key] === 'string' ? store[key] : { ...store[key] };
        }
      }
    }),
    stat: vi.fn(async (p) => ({
      size: typeof store[p] === 'string' ? store[p].length : 0,
      mtime: new Date().toISOString(),
      isDirectory: !!(store[p] && store[p].__dir)
    })),
    getDocumentsPath: vi.fn(async () => '/Documents'),
    showSaveDialog: vi.fn(async () => ({ canceled: false, filePath: '/out/file.html' })),
    showOpenDialog: vi.fn(async () => ({ canceled: false, filePaths: ['/some/file.html'] }))
  };

  // expose store for inspection in tests
  api.__store = store;
  return api;
}

const BASE = '/notes-base';

beforeEach(async () => {
  global.window = { api: makeApi() };
  await initStorage(BASE);
});

// --- initStorage ---

describe('initStorage', () => {
  it('sets base path', () => {
    expect(getBasePath()).toBe(BASE);
  });

  it('creates required top-level directories', () => {
    expect(window.api.mkdir).toHaveBeenCalledWith(`${BASE}`);
    expect(window.api.mkdir).toHaveBeenCalledWith(`${BASE}/notes`);
    expect(window.api.mkdir).toHaveBeenCalledWith(`${BASE}/templates`);
    expect(window.api.mkdir).toHaveBeenCalledWith(`${BASE}/trash`);
  });

  it('creates the uncategorized folder on first init', () => {
    const store = window.api.__store;
    expect(store[`${BASE}/notes/uncategorized/folder.json`]).toBeDefined();
    const meta = JSON.parse(store[`${BASE}/notes/uncategorized/folder.json`]);
    expect(meta.id).toBe('uncategorized');
    expect(meta.name).toBe('Uncategorized');
  });

  it('does not recreate uncategorized folder when it already exists', async () => {
    const callsBefore = window.api.mkdir.mock.calls.length;
    await initStorage(BASE);
    // uncategorized dir already in store, mkdir should not be called for it again
    const calls = window.api.mkdir.mock.calls.slice(callsBefore);
    const uncatCalls = calls.filter(c => c[0].includes('uncategorized'));
    expect(uncatCalls).toHaveLength(0);
  });
});

// --- loadFolders ---

describe('loadFolders', () => {
  it('returns the uncategorized folder after init', async () => {
    const folders = await loadFolders();
    expect(folders).toHaveLength(1);
    expect(folders[0].id).toBe('uncategorized');
  });

  it('returns folders sorted by order', async () => {
    const f1 = await createFolder('B');
    const f2 = await createFolder('A');
    // f1 has a smaller order (created earlier) => comes first
    const folders = await loadFolders();
    const ids = folders.map(f => f.id);
    expect(ids.indexOf(f1.id)).toBeLessThan(ids.indexOf(f2.id));
  });

  it('skips entries without folder.json', async () => {
    // manually insert a dir without folder.json
    window.api.__store[`${BASE}/notes/orphan`] = { __dir: true };
    const folders = await loadFolders();
    const ids = folders.map(f => f.id);
    expect(ids).not.toContain('orphan');
  });
});

// --- createFolder ---

describe('createFolder', () => {
  it('returns a folder object with expected shape', async () => {
    const f = await createFolder('Work', null, '#ff0000');
    expect(f).toMatchObject({ name: 'Work', color: '#ff0000', parentId: null });
    expect(typeof f.id).toBe('string');
    expect(typeof f.createdAt).toBe('string');
  });

  it('writes folder.json to the correct path', async () => {
    const f = await createFolder('Personal');
    const stored = window.api.__store[`${BASE}/notes/${f.id}/folder.json`];
    expect(stored).toBeDefined();
    const parsed = JSON.parse(stored);
    expect(parsed.name).toBe('Personal');
  });

  it('defaults color to #666 when not provided', async () => {
    const f = await createFolder('Default Color');
    expect(f.color).toBe('#666');
  });
});

// --- renameFolder / updateFolder ---

describe('renameFolder', () => {
  it('updates the name in folder.json', async () => {
    const f = await createFolder('Old Name');
    await renameFolder(f.id, 'New Name');
    const raw = window.api.__store[`${BASE}/notes/${f.id}/folder.json`];
    expect(JSON.parse(raw).name).toBe('New Name');
  });
});

describe('updateFolder', () => {
  it('merges partial updates into folder.json', async () => {
    const f = await createFolder('Test', null, '#aaa');
    await updateFolder(f.id, { color: '#bbb', order: 5 });
    const raw = window.api.__store[`${BASE}/notes/${f.id}/folder.json`];
    const updated = JSON.parse(raw);
    expect(updated.color).toBe('#bbb');
    expect(updated.order).toBe(5);
    expect(updated.name).toBe('Test');
  });
});

// --- deleteFolder ---

describe('deleteFolder', () => {
  it('moves folder dir to trash', async () => {
    const f = await createFolder('ToDelete');
    await deleteFolder(f.id);
    const remaining = Object.keys(window.api.__store).filter(k =>
      k.startsWith(`${BASE}/notes/${f.id}`)
    );
    expect(remaining).toHaveLength(0);
    const inTrash = Object.keys(window.api.__store).filter(k =>
      k.startsWith(`${BASE}/trash/folder_${f.id}`)
    );
    expect(inTrash.length).toBeGreaterThan(0);
  });
});

// --- loadNotes / createNote ---

describe('createNote', () => {
  it('returns a note with expected shape', async () => {
    const note = await createNote('uncategorized');
    expect(note).toMatchObject({
      folderId: 'uncategorized',
      title: 'Untitled',
      pinned: false,
      favorited: false,
      tags: []
    });
    expect(typeof note.id).toBe('string');
  });

  it('writes note.json and content.html', async () => {
    const note = await createNote('uncategorized');
    const metaKey = `${BASE}/notes/uncategorized/${note.id}/note.json`;
    const contentKey = `${BASE}/notes/uncategorized/${note.id}/content.html`;
    expect(window.api.__store[metaKey]).toBeDefined();
    expect(window.api.__store[contentKey]).toBeDefined();
  });

  it('uses template content when provided', async () => {
    const note = await createNote('uncategorized', '<p>Hello</p>');
    const contentKey = `${BASE}/notes/uncategorized/${note.id}/content.html`;
    expect(window.api.__store[contentKey]).toBe('<p>Hello</p>');
  });
});

describe('loadNotes', () => {
  it('returns all notes in a folder', async () => {
    await createNote('uncategorized');
    await createNote('uncategorized');
    const notes = await loadNotes('uncategorized');
    expect(notes).toHaveLength(2);
  });

  it('returns empty array for folder with no notes', async () => {
    const f = await createFolder('Empty');
    const notes = await loadNotes(f.id);
    expect(notes).toHaveLength(0);
  });
});

describe('loadAllNotes', () => {
  it('aggregates notes from all folders', async () => {
    const f = await createFolder('Other');
    await createNote('uncategorized');
    await createNote(f.id);
    const all = await loadAllNotes();
    expect(all.length).toBeGreaterThanOrEqual(2);
  });
});

// --- saveNoteContent / loadNoteContent ---

describe('saveNoteContent', () => {
  it('writes html to content.html', async () => {
    const note = await createNote('uncategorized');
    await saveNoteContent(note.id, 'uncategorized', '<p>Hello world</p>');
    const stored = window.api.__store[`${BASE}/notes/uncategorized/${note.id}/content.html`];
    expect(stored).toBe('<p>Hello world</p>');
  });

  it('creates a version when content changes', async () => {
    const note = await createNote('uncategorized', '<p>v1</p>');
    await saveNoteContent(note.id, 'uncategorized', '<p>v2</p>');
    const versionsPrefix = `${BASE}/notes/uncategorized/${note.id}/versions/`;
    const versions = Object.keys(window.api.__store).filter(k => k.startsWith(versionsPrefix));
    expect(versions).toHaveLength(1);
    expect(window.api.__store[versions[0]]).toBe('<p>v1</p>');
  });

  it('does not create a version when content is unchanged', async () => {
    const note = await createNote('uncategorized', '<p>same</p>');
    await saveNoteContent(note.id, 'uncategorized', '<p>same</p>');
    const versionsPrefix = `${BASE}/notes/uncategorized/${note.id}/versions/`;
    const versions = Object.keys(window.api.__store).filter(k => k.startsWith(versionsPrefix));
    expect(versions).toHaveLength(0);
  });

  it('prunes versions above 10', async () => {
    const note = await createNote('uncategorized', '<p>init</p>');
    // save 12 distinct versions
    for (let i = 1; i <= 12; i++) {
      await saveNoteContent(note.id, 'uncategorized', `<p>v${i}</p>`);
    }
    const versionsPrefix = `${BASE}/notes/uncategorized/${note.id}/versions/`;
    const versions = Object.keys(window.api.__store).filter(k => k.startsWith(versionsPrefix));
    expect(versions.length).toBeLessThanOrEqual(10);
  });

  it('updates modifiedAt in note.json', async () => {
    const note = await createNote('uncategorized');
    const before = note.modifiedAt;
    await new Promise(r => setTimeout(r, 2)); // ensure time advances
    await saveNoteContent(note.id, 'uncategorized', '<p>changed</p>');
    const raw = window.api.__store[`${BASE}/notes/uncategorized/${note.id}/note.json`];
    const updated = JSON.parse(raw);
    expect(updated.modifiedAt).not.toBe(before);
  });
});

describe('loadNoteContent', () => {
  it('returns html content of a note', async () => {
    const note = await createNote('uncategorized', '<p>content</p>');
    const html = await loadNoteContent(note.id, 'uncategorized');
    expect(html).toBe('<p>content</p>');
  });

  it('returns empty string when content.html does not exist', async () => {
    // write a note.json but no content.html
    const note = await createNote('uncategorized');
    delete window.api.__store[`${BASE}/notes/uncategorized/${note.id}/content.html`];
    const html = await loadNoteContent(note.id, 'uncategorized');
    expect(html).toBe('');
  });
});

// --- trashNote ---

describe('trashNote', () => {
  it('moves note dir to trash', async () => {
    const note = await createNote('uncategorized');
    await trashNote(note.id, 'uncategorized');
    const inFolder = Object.keys(window.api.__store).filter(k =>
      k.startsWith(`${BASE}/notes/uncategorized/${note.id}`)
    );
    expect(inFolder).toHaveLength(0);
    const inTrash = Object.keys(window.api.__store).filter(k =>
      k.startsWith(`${BASE}/trash/note_${note.id}`)
    );
    expect(inTrash.length).toBeGreaterThan(0);
  });
});

// --- restoreNote / permanentlyDelete ---

describe('restoreNote', () => {
  it('moves note from trash back to target folder', async () => {
    const note = await createNote('uncategorized');
    await trashNote(note.id, 'uncategorized');
    await restoreNote(note.id, 'uncategorized');
    const restored = Object.keys(window.api.__store).filter(k =>
      k.startsWith(`${BASE}/notes/uncategorized/${note.id}`)
    );
    expect(restored.length).toBeGreaterThan(0);
  });

  it('throws when note is not in trash', async () => {
    await expect(restoreNote('nonexistent-id', 'uncategorized')).rejects.toThrow();
  });
});

describe('permanentlyDelete', () => {
  it('removes note from trash', async () => {
    const note = await createNote('uncategorized');
    await trashNote(note.id, 'uncategorized');
    await permanentlyDelete(note.id);
    const inTrash = Object.keys(window.api.__store).filter(k =>
      k.startsWith(`${BASE}/trash/note_${note.id}`)
    );
    expect(inTrash).toHaveLength(0);
  });

  it('throws when note is not in trash', async () => {
    await expect(permanentlyDelete('nonexistent-id')).rejects.toThrow();
  });
});

// --- loadVersions / restoreVersion ---

describe('loadVersions', () => {
  it('returns versions sorted newest first', async () => {
    const note = await createNote('uncategorized', '<p>v1</p>');
    await saveNoteContent(note.id, 'uncategorized', '<p>v2</p>');
    await saveNoteContent(note.id, 'uncategorized', '<p>v3</p>');
    const versions = await loadVersions(note.id, 'uncategorized');
    expect(versions.length).toBeGreaterThanOrEqual(2);
    // sorted newest first
    for (let i = 1; i < versions.length; i++) {
      expect(versions[i - 1].timestamp).toBeGreaterThanOrEqual(versions[i].timestamp);
    }
  });

  it('returns empty array when versions dir does not exist', async () => {
    const note = await createNote('uncategorized');
    delete window.api.__store[`${BASE}/notes/uncategorized/${note.id}/versions`];
    const versions = await loadVersions(note.id, 'uncategorized');
    expect(versions).toHaveLength(0);
  });
});

describe('restoreVersion', () => {
  it('restores version content to content.html', async () => {
    const note = await createNote('uncategorized', '<p>original</p>');
    await saveNoteContent(note.id, 'uncategorized', '<p>updated</p>');
    const versions = await loadVersions(note.id, 'uncategorized');
    expect(versions.length).toBeGreaterThan(0);
    await restoreVersion(note.id, 'uncategorized', versions[0].filename);
    const html = await loadNoteContent(note.id, 'uncategorized');
    expect(html).toBe('<p>original</p>');
  });
});

// --- searchNotes ---

describe('searchNotes', () => {
  it('finds notes containing the query in body', async () => {
    const note = await createNote('uncategorized');
    await saveNoteContent(note.id, 'uncategorized', '<p>Find me in the search results</p>');
    const results = await searchNotes('Find me');
    expect(results.length).toBeGreaterThan(0);
    expect(results[0].note.id).toBe(note.id);
  });

  it('finds notes matching query in title via metadata', async () => {
    const note = await createNote('uncategorized');
    await saveNoteMetadata(note.id, 'uncategorized', { title: 'UniqueTitle123' });
    await saveNoteContent(note.id, 'uncategorized', '<p>body text</p>');
    const results = await searchNotes('UniqueTitle123');
    expect(results.map(r => r.note.id)).toContain(note.id);
  });

  it('returns empty array for empty query', async () => {
    await createNote('uncategorized');
    const results = await searchNotes('');
    expect(results).toHaveLength(0);
  });

  it('includes context snippet in results', async () => {
    const note = await createNote('uncategorized');
    await saveNoteContent(note.id, 'uncategorized', '<p>Some text around keyword here</p>');
    const results = await searchNotes('keyword');
    expect(results[0].context).toContain('keyword');
  });

  it('is case-insensitive', async () => {
    const note = await createNote('uncategorized');
    await saveNoteContent(note.id, 'uncategorized', '<p>Hello World</p>');
    const results = await searchNotes('hello world');
    expect(results.map(r => r.note.id)).toContain(note.id);
  });
});

// --- loadConfig / saveConfig ---

describe('loadConfig', () => {
  it('returns default config when config.json does not exist', async () => {
    const config = await loadConfig();
    expect(config.theme).toBe('light');
    expect(config.firstLaunch).toBe(true);
    expect(config.autoSaveInterval).toBe(500);
    expect(Array.isArray(config.tags)).toBe(true);
    expect(typeof config.shortcuts).toBe('object');
  });

  it('merges saved config over defaults', async () => {
    await saveConfig({ theme: 'dark', editorFontSize: 20 });
    const config = await loadConfig();
    expect(config.theme).toBe('dark');
    expect(config.editorFontSize).toBe(20);
    // default fields still present
    expect(config.accentColor).toBe('#007AFF');
  });
});

describe('saveConfig', () => {
  it('writes config to config.json', async () => {
    await saveConfig({ theme: 'dark' });
    const stored = window.api.__store[`${BASE}/config.json`];
    expect(stored).toBeDefined();
    const parsed = JSON.parse(stored);
    expect(parsed.theme).toBe('dark');
  });
});

// --- getStorageStats ---

describe('getStorageStats', () => {
  it('returns correct note and folder counts', async () => {
    const f = await createFolder('Stats');
    await createNote('uncategorized');
    await createNote(f.id);
    const stats = await getStorageStats();
    expect(stats.noteCount).toBe(2);
    expect(stats.folderCount).toBeGreaterThanOrEqual(2);
  });

  it('returns totalSize as a number', async () => {
    await createNote('uncategorized');
    await saveNoteContent((await loadNotes('uncategorized'))[0].id, 'uncategorized', '<p>hi</p>');
    const stats = await getStorageStats();
    expect(typeof stats.totalSize).toBe('number');
    expect(stats.totalSize).toBeGreaterThan(0);
  });
});
