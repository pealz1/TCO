import { Editor } from '@tiptap/core';
import StarterKit from '@tiptap/starter-kit';
import Underline from '@tiptap/extension-underline';
import TextAlign from '@tiptap/extension-text-align';
import TextStyle from '@tiptap/extension-text-style';
import { Color } from '@tiptap/extension-color';
import Highlight from '@tiptap/extension-highlight';
import FontFamily from '@tiptap/extension-font-family';
import Link from '@tiptap/extension-link';
import Image from '@tiptap/extension-image';
import TaskList from '@tiptap/extension-task-list';
import TaskItem from '@tiptap/extension-task-item';
import Table from '@tiptap/extension-table';
import TableRow from '@tiptap/extension-table-row';
import TableCell from '@tiptap/extension-table-cell';
import TableHeader from '@tiptap/extension-table-header';
import Placeholder from '@tiptap/extension-placeholder';
import CharacterCount from '@tiptap/extension-character-count';
import { store } from '../store.js';
import * as storage from '../storage.js';
import { debounce } from '../utils/debounce.js';

// Custom FontSize extension — adds fontSize attribute to TextStyle
const FontSize = TextStyle.extend({
  addAttributes() {
    return {
      ...this.parent?.(),
      fontSize: {
        default: null,
        parseHTML: el => el.style.fontSize?.replace('px', '') || null,
        renderHTML: attrs => {
          if (!attrs.fontSize) return {};
          return { style: `font-size: ${attrs.fontSize}px` };
        }
      }
    };
  }
});

let editor = null;
let currentNoteId = null;
let currentFolderId = null;
let autoSave = null;

export function initEditor() {
  editor = new Editor({
    element: document.getElementById('editor-container'),
    extensions: [
      StarterKit.configure({
        heading: { levels: [1, 2, 3] }
      }),
      Underline,
      TextAlign.configure({ types: ['heading', 'paragraph'] }),
      FontSize,
      Color,
      Highlight.configure({ multicolor: true }),
      FontFamily,
      Link.configure({ openOnClick: false, autolink: true }),
      Image.configure({ inline: true, allowBase64: true }),
      TaskList,
      TaskItem.configure({ nested: true }),
      Table.configure({ resizable: true }),
      TableRow,
      TableCell,
      TableHeader,
      Placeholder.configure({ placeholder: 'Start writing...' }),
      CharacterCount
    ],
    editorProps: {
      attributes: { class: 'ProseMirror' }
    },
    onUpdate: ({ editor: ed }) => {
      if (autoSave) autoSave();
      window.app?.eventBus?.emit('editor:update', {
        wordCount: ed.storage.characterCount.words(),
        charCount: ed.storage.characterCount.characters(),
        html: ed.getHTML()
      });
    },
    onSelectionUpdate: ({ editor: ed }) => {
      window.app?.eventBus?.emit('editor:selection', { editor: ed });
    }
  });

  autoSave = debounce(async () => {
    if (!currentNoteId || !currentFolderId) return;
    window.app?.eventBus?.emit('editor:saving');
    const html = editor.getHTML();
    await storage.saveNoteContent(currentNoteId, currentFolderId, html);
    const title = editor.getText().split('\n')[0]?.substring(0, 100) || 'Untitled';
    const preview = editor.getText().split('\n').slice(1).join(' ').substring(0, 200);
    const notes = store.get('notes').map(n =>
      n.id === currentNoteId ? { ...n, title, preview, modifiedAt: new Date().toISOString() } : n
    );
    store.set('notes', notes);
    window.app?.eventBus?.emit('editor:saved');
  }, 500);

  store.on('activeNote', loadActiveNote);

  return editor;
}

async function loadActiveNote() {
  const noteId = store.get('activeNote');
  if (!noteId) {
    editor?.commands.clearContent();
    currentNoteId = null;
    currentFolderId = null;
    return;
  }

  const note = store.get('notes').find(n => n.id === noteId);
  if (!note) return;

  currentNoteId = note.id;
  currentFolderId = note.folderId;

  const content = await storage.loadNoteContent(note.id, note.folderId);
  editor.commands.setContent(content || '');
  editor.commands.focus('end');
}

export function getEditor() {
  return editor;
}
