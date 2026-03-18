import { checkGrammar } from '../grammar.js';
import { getEditor } from './editor.js';
import { soundEngine } from '../sounds.js';

let panel = null;
let issues = [];
let ignoredOffsets = new Set();

function getPlainText() {
  const ed = getEditor();
  if (!ed) return '';
  return ed.getText();
}

function getLinePreview(text, offset, length) {
  const start = Math.max(0, offset - 20);
  const end = Math.min(text.length, offset + length + 20);
  let preview = text.slice(start, end);
  if (start > 0) preview = '...' + preview;
  if (end < text.length) preview = preview + '...';
  return preview;
}

function renderIssues(text) {
  const list = panel.querySelector('.grammar-issues-list');
  const count = panel.querySelector('.grammar-count');

  const visible = issues.filter(i => !ignoredOffsets.has(i.offset));
  count.textContent = `${visible.length} issue${visible.length !== 1 ? 's' : ''} found`;

  if (visible.length === 0) {
    list.innerHTML = '<div class="grammar-empty">No issues found.</div>';
    return;
  }

  list.innerHTML = '';
  visible.forEach((issue, idx) => {
    const item = document.createElement('div');
    item.className = 'grammar-issue-item';
    item.innerHTML = `
      <div class="grammar-issue-preview">${escapeHtml(getLinePreview(text, issue.offset, issue.length))}</div>
      <div class="grammar-issue-message">${escapeHtml(issue.message)}</div>
      <div class="grammar-issue-suggestion">Suggestion: <strong>${escapeHtml(issue.suggestions[0] || '')}</strong></div>
      <div class="grammar-issue-actions">
        <button class="grammar-fix-btn" data-idx="${idx}">Fix</button>
        <button class="grammar-ignore-btn" data-idx="${idx}">Ignore</button>
      </div>
    `;
    list.appendChild(item);
  });

  list.querySelectorAll('.grammar-fix-btn').forEach(btn => {
    btn.onclick = () => {
      const i = parseInt(btn.dataset.idx);
      fixIssue(visible[i]);
    };
  });

  list.querySelectorAll('.grammar-ignore-btn').forEach(btn => {
    btn.onclick = () => {
      const i = parseInt(btn.dataset.idx);
      ignoredOffsets.add(visible[i].offset);
      renderIssues(text);
    };
  });
}

function fixIssue(issue) {
  const ed = getEditor();
  if (!ed) return;
  const text = ed.getText();
  const before = text.slice(0, issue.offset);
  const after = text.slice(issue.offset + issue.length);
  const fixed = before + issue.suggestions[0] + after;

  // We need to work with the HTML content
  // Simple approach: replace in the HTML using the text offset
  const html = ed.getHTML();
  const plainText = ed.getText();

  // Map text offset to approximate HTML position by counting non-tag chars
  let textIdx = 0;
  let htmlIdx = 0;
  let inTag = false;
  let startHtml = -1;
  let endHtml = -1;

  for (htmlIdx = 0; htmlIdx < html.length; htmlIdx++) {
    if (html[htmlIdx] === '<') { inTag = true; continue; }
    if (html[htmlIdx] === '>') { inTag = false; continue; }
    if (inTag) continue;

    if (textIdx === issue.offset) startHtml = htmlIdx;
    if (textIdx === issue.offset + issue.length) { endHtml = htmlIdx; break; }
    textIdx++;
  }

  if (startHtml === -1) return;
  if (endHtml === -1) endHtml = htmlIdx;

  const newHtml = html.slice(0, startHtml) + issue.suggestions[0] + html.slice(endHtml);
  ed.commands.setContent(newHtml, false);
  soundEngine.play('click');

  // Re-run check
  runCheck();
}

function fixAll() {
  const visible = issues.filter(i => !ignoredOffsets.has(i.offset));
  // Fix from end to start to preserve offsets
  const sorted = [...visible].sort((a, b) => b.offset - a.offset);

  const ed = getEditor();
  if (!ed) return;

  let html = ed.getHTML();

  for (const issue of sorted) {
    const plainText = getPlainTextFromHtml(html);
    let textIdx = 0;
    let htmlIdx = 0;
    let inTag = false;
    let startHtml = -1;
    let endHtml = -1;

    for (htmlIdx = 0; htmlIdx < html.length; htmlIdx++) {
      if (html[htmlIdx] === '<') { inTag = true; continue; }
      if (html[htmlIdx] === '>') { inTag = false; continue; }
      if (inTag) continue;

      if (textIdx === issue.offset) startHtml = htmlIdx;
      if (textIdx === issue.offset + issue.length) { endHtml = htmlIdx; break; }
      textIdx++;
    }

    if (startHtml === -1) continue;
    if (endHtml === -1) endHtml = htmlIdx;

    html = html.slice(0, startHtml) + issue.suggestions[0] + html.slice(endHtml);
  }

  ed.commands.setContent(html, false);
  soundEngine.play('pop');
  runCheck();
}

function getPlainTextFromHtml(html) {
  const div = document.createElement('div');
  div.innerHTML = html;
  return div.textContent || '';
}

function runCheck() {
  const text = getPlainText();
  ignoredOffsets = new Set();
  issues = checkGrammar(text);
  renderIssues(text);
}

function escapeHtml(str) {
  return str.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

export function openGrammarPanel() {
  if (panel) {
    panel.classList.add('visible');
    runCheck();
    return;
  }

  panel = document.createElement('div');
  panel.className = 'grammar-panel visible';
  panel.innerHTML = `
    <div class="grammar-panel-header">
      <span class="grammar-panel-title">Grammar Check</span>
      <button class="grammar-panel-close">\u2715</button>
    </div>
    <div class="grammar-toolbar">
      <button class="grammar-check-btn">Check Grammar</button>
      <button class="grammar-fix-all-btn">Fix All</button>
    </div>
    <div class="grammar-count"></div>
    <div class="grammar-issues-list"></div>
  `;

  document.getElementById('editor-panel').appendChild(panel);

  panel.querySelector('.grammar-panel-close').onclick = () => {
    panel.classList.remove('visible');
    soundEngine.play('click');
  };

  panel.querySelector('.grammar-check-btn').onclick = () => {
    soundEngine.play('click');
    runCheck();
  };

  panel.querySelector('.grammar-fix-all-btn').onclick = () => {
    soundEngine.play('click');
    fixAll();
  };

  runCheck();
}

export function closeGrammarPanel() {
  if (panel) panel.classList.remove('visible');
}
