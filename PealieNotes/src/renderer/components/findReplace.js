import { getEditor } from './editor.js';
import { soundEngine } from '../sounds.js';

let visible = false;
let showReplace = false;
let matches = [];
let currentIndex = -1;
let bar = null;

function getTextNodes(node) {
  const nodes = [];
  const walker = document.createTreeWalker(node, NodeFilter.SHOW_TEXT, null);
  let n;
  while ((n = walker.nextNode())) nodes.push(n);
  return nodes;
}

function clearHighlights() {
  const container = document.getElementById('editor-container');
  if (!container) return;
  const marks = container.querySelectorAll('mark[data-find-highlight]');
  for (const mark of marks) {
    const parent = mark.parentNode;
    parent.replaceChild(document.createTextNode(mark.textContent), mark);
    parent.normalize();
  }
}

function highlightMatches(query) {
  clearHighlights();
  matches = [];
  currentIndex = -1;
  if (!query) return;

  const container = document.querySelector('#editor-container .ProseMirror');
  if (!container) return;

  const lower = query.toLowerCase();
  const textNodes = getTextNodes(container);

  for (const node of textNodes) {
    const text = node.textContent;
    const textLower = text.toLowerCase();
    let startIdx = 0;
    let pos;

    while ((pos = textLower.indexOf(lower, startIdx)) !== -1) {
      const range = document.createRange();
      range.setStart(node, pos);
      range.setEnd(node, pos + query.length);

      const mark = document.createElement('mark');
      mark.setAttribute('data-find-highlight', '');
      mark.style.background = 'rgba(255, 200, 0, 0.4)';
      mark.style.borderRadius = '2px';
      range.surroundContents(mark);

      matches.push(mark);

      // After wrapping, the text node is split; continue from the remainder
      const remaining = mark.nextSibling;
      if (!remaining) break;
      startIdx = 0;
      // Move to the new text node after the mark
      break; // Re-scan since DOM changed; re-trigger from top
    }
  }

  // Re-scan to collect all highlights (DOM mutations mean we need a fresh list)
  matches = Array.from(container.querySelectorAll('mark[data-find-highlight]'));

  if (matches.length > 0) {
    currentIndex = 0;
    setActiveMark(0);
  }

  updateCount();
}

function setActiveMark(idx) {
  for (const m of matches) {
    m.style.background = 'rgba(255, 200, 0, 0.4)';
  }
  if (idx >= 0 && idx < matches.length) {
    matches[idx].style.background = 'rgba(255, 120, 0, 0.6)';
    matches[idx].scrollIntoView({ block: 'center', behavior: 'smooth' });
  }
}

function updateCount() {
  if (!bar) return;
  const countEl = bar.querySelector('.find-count');
  if (!countEl) return;
  if (matches.length === 0) {
    countEl.textContent = 'No results';
  } else {
    countEl.textContent = `${currentIndex + 1} of ${matches.length}`;
  }
}

function goNext() {
  if (matches.length === 0) return;
  currentIndex = (currentIndex + 1) % matches.length;
  setActiveMark(currentIndex);
  updateCount();
}

function goPrev() {
  if (matches.length === 0) return;
  currentIndex = (currentIndex - 1 + matches.length) % matches.length;
  setActiveMark(currentIndex);
  updateCount();
}

function replaceCurrent() {
  if (currentIndex < 0 || currentIndex >= matches.length) return;
  const replaceInput = bar.querySelector('.replace-input');
  const replaceText = replaceInput?.value || '';

  const mark = matches[currentIndex];
  const textNode = document.createTextNode(replaceText);
  mark.parentNode.replaceChild(textNode, mark);
  textNode.parentNode.normalize();

  // Sync content back to TipTap
  syncEditorContent();

  // Re-search
  const query = bar.querySelector('.find-input')?.value || '';
  highlightMatches(query);
}

function replaceAll() {
  const replaceInput = bar.querySelector('.replace-input');
  const replaceText = replaceInput?.value || '';

  for (const mark of matches) {
    const textNode = document.createTextNode(replaceText);
    mark.parentNode.replaceChild(textNode, mark);
    textNode.parentNode.normalize();
  }

  syncEditorContent();

  matches = [];
  currentIndex = -1;
  updateCount();
}

function syncEditorContent() {
  const ed = getEditor();
  if (!ed) return;
  const container = document.querySelector('#editor-container .ProseMirror');
  if (!container) return;
  // Read the current DOM innerHTML and push back to TipTap
  // Remove any leftover find-highlight marks first
  clearHighlights();
  const html = container.innerHTML;
  ed.commands.setContent(html, false);
}

function open(withReplace) {
  showReplace = withReplace;
  visible = true;
  bar.classList.add('visible');
  bar.classList.toggle('with-replace', showReplace);
  const input = bar.querySelector('.find-input');
  input?.focus();
  input?.select();
}

function close() {
  visible = false;
  bar.classList.remove('visible', 'with-replace');
  clearHighlights();
  matches = [];
  currentIndex = -1;
  getEditor()?.commands.focus();
}

export function initFindReplace() {
  const editorPanel = document.getElementById('editor-panel');
  if (!editorPanel) return;

  bar = document.createElement('div');
  bar.className = 'find-replace-bar';
  bar.innerHTML = `
    <div class="find-row">
      <input type="text" class="find-input" placeholder="Find..." />
      <span class="find-count"></span>
      <button class="find-btn prev-btn" title="Previous">\u25B2</button>
      <button class="find-btn next-btn" title="Next">\u25BC</button>
      <button class="find-btn close-btn" title="Close">\u2715</button>
    </div>
    <div class="replace-row">
      <input type="text" class="replace-input" placeholder="Replace..." />
      <button class="find-btn replace-btn" title="Replace">R</button>
      <button class="find-btn replace-all-btn" title="Replace All">RA</button>
    </div>
  `;

  // Insert before toolbar or at the start of editor panel
  const toolbar = document.getElementById('toolbar');
  if (toolbar) {
    editorPanel.insertBefore(bar, toolbar.nextSibling);
  } else {
    editorPanel.insertBefore(bar, editorPanel.firstChild);
  }

  const findInput = bar.querySelector('.find-input');
  const replaceInput = bar.querySelector('.replace-input');

  findInput.addEventListener('input', () => {
    highlightMatches(findInput.value);
  });

  findInput.addEventListener('keydown', (e) => {
    if (e.key === 'Enter') {
      e.preventDefault();
      if (e.shiftKey) goPrev();
      else goNext();
    }
    if (e.key === 'Escape') {
      e.preventDefault();
      close();
    }
  });

  replaceInput.addEventListener('keydown', (e) => {
    if (e.key === 'Escape') {
      e.preventDefault();
      close();
    }
  });

  bar.querySelector('.prev-btn').addEventListener('click', () => {
    soundEngine.play('click');
    goPrev();
  });
  bar.querySelector('.next-btn').addEventListener('click', () => {
    soundEngine.play('click');
    goNext();
  });
  bar.querySelector('.close-btn').addEventListener('click', () => {
    soundEngine.play('click');
    close();
  });
  bar.querySelector('.replace-btn').addEventListener('click', () => {
    soundEngine.play('click');
    replaceCurrent();
  });
  bar.querySelector('.replace-all-btn').addEventListener('click', () => {
    soundEngine.play('click');
    replaceAll();
  });

  // Keyboard shortcuts
  document.addEventListener('keydown', (e) => {
    if (e.ctrlKey && e.key === 'f') {
      e.preventDefault();
      open(false);
    }
    if (e.ctrlKey && e.key === 'h') {
      e.preventDefault();
      open(true);
    }
  });
}
