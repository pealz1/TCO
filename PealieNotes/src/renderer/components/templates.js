import { store } from '../store.js';
import { soundEngine } from '../sounds.js';
import * as storage from '../storage.js';

function getTodayFormatted() {
  return new Date().toLocaleDateString('en-US', {
    weekday: 'long', year: 'numeric', month: 'long', day: 'numeric'
  });
}

const BUILT_IN_TEMPLATES = [
  {
    id: 'blank',
    name: 'Blank Note',
    icon: '\uD83D\uDCC4',
    content: ''
  },
  {
    id: 'meeting',
    name: 'Meeting Notes',
    icon: '\uD83D\uDCCB',
    getContent() {
      return `<h2>Meeting: [Title]</h2><p><strong>Date:</strong> ${getTodayFormatted()}</p><p><strong>Attendees:</strong></p><ul><li></li></ul><h3>Agenda</h3><ol><li></li></ol><h3>Notes</h3><p></p><h3>Action Items</h3><ul data-type="taskList"><li data-type="taskItem" data-checked="false"><p></p></li></ul>`;
    }
  },
  {
    id: 'todo',
    name: 'To-Do List',
    icon: '\u2705',
    getContent() {
      return `<h2>To-Do</h2><h3>Today</h3><ul data-type="taskList"><li data-type="taskItem" data-checked="false"><p></p></li></ul><h3>Upcoming</h3><ul data-type="taskList"><li data-type="taskItem" data-checked="false"><p></p></li></ul>`;
    }
  },
  {
    id: 'journal',
    name: 'Journal Entry',
    icon: '\uD83D\uDCD3',
    getContent() {
      return `<h2>${getTodayFormatted()}</h2><p><em>How was your day?</em></p><h3>Highlights</h3><ul><li></li></ul><h3>Grateful For</h3><ul><li></li></ul>`;
    }
  }
];

let dropdown = null;

function closeDropdown() {
  if (dropdown) {
    dropdown.remove();
    dropdown = null;
  }
  document.removeEventListener('click', onOutsideClick);
}

function onOutsideClick(e) {
  if (dropdown && !dropdown.contains(e.target)) {
    closeDropdown();
  }
}

export function showTemplatePicker(anchorEl) {
  closeDropdown();

  dropdown = document.createElement('div');
  dropdown.className = 'template-picker';

  BUILT_IN_TEMPLATES.forEach(tmpl => {
    const item = document.createElement('div');
    item.className = 'template-picker-item';
    item.innerHTML = `<span class="template-icon">${tmpl.icon}</span><span class="template-name">${tmpl.name}</span>`;
    item.onclick = async (e) => {
      e.stopPropagation();
      soundEngine.play('click');
      closeDropdown();
      await createFromTemplate(tmpl);
    };
    dropdown.appendChild(item);
  });

  // Position below the anchor
  const rect = anchorEl.getBoundingClientRect();
  dropdown.style.position = 'absolute';
  dropdown.style.left = rect.left + 'px';
  dropdown.style.top = rect.bottom + 4 + 'px';
  dropdown.style.zIndex = '1000';

  document.body.appendChild(dropdown);
  setTimeout(() => document.addEventListener('click', onOutsideClick), 0);
}

async function createFromTemplate(tmpl) {
  const activeFolder = store.get('activeFolder');
  const folderId = (activeFolder === 'all' || activeFolder === 'favorites' || activeFolder === 'trash')
    ? 'uncategorized' : activeFolder;

  const content = tmpl.getContent ? tmpl.getContent() : (tmpl.content || '');
  const note = await storage.createNote(folderId, content);
  const notes = await storage.loadAllNotes();
  store.set('notes', notes);
  store.set('activeNote', note.id);
  soundEngine.play('pop');
}
