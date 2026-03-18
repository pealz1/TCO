import { getEditor } from './editor.js';
import { soundEngine } from '../sounds.js';

const PRESET_COLORS = [
  '#000000', '#434343', '#666666', '#999999',
  '#E81123', '#FF6900', '#FCB900', '#7BDCB5',
  '#00D084', '#0693E3', '#AB149E', '#8B5CF6',
  '#EB144C', '#F78DA7', '#9900EF', '#FFFFFF'
];

const FONT_SIZES = [8, 9, 10, 11, 12, 14, 16, 18, 20, 24, 28, 32, 36, 48, 60, 72];

let activeColorPicker = null;

function btn(cls, title, innerHTML, onClick) {
  const el = document.createElement('button');
  el.className = 'toolbar-btn' + (cls ? ' ' + cls : '');
  el.title = title;
  el.innerHTML = innerHTML;
  el.addEventListener('mousedown', e => e.preventDefault());
  el.addEventListener('click', () => {
    soundEngine.play('click');
    onClick(el);
  });
  return el;
}

function divider() {
  const el = document.createElement('div');
  el.className = 'toolbar-divider';
  return el;
}

function group(...children) {
  const el = document.createElement('div');
  el.className = 'toolbar-group';
  for (const child of children) el.appendChild(child);
  return el;
}

function select(cls, options, onChange) {
  const el = document.createElement('select');
  el.className = 'toolbar-select' + (cls ? ' ' + cls : '');
  for (const opt of options) {
    const o = document.createElement('option');
    o.value = opt.value;
    o.textContent = opt.label;
    el.appendChild(o);
  }
  el.addEventListener('mousedown', e => e.stopPropagation());
  el.addEventListener('change', () => {
    soundEngine.play('click');
    onChange(el.value);
  });
  return el;
}

function showColorPicker(anchor, onPick) {
  closeColorPicker();
  const popup = document.createElement('div');
  popup.className = 'color-picker-popup';
  for (const color of PRESET_COLORS) {
    const swatch = document.createElement('div');
    swatch.className = 'color-swatch';
    swatch.style.background = color;
    if (color === '#FFFFFF') {
      swatch.style.border = '1px solid var(--border)';
    }
    swatch.addEventListener('click', () => {
      soundEngine.play('click');
      onPick(color);
      closeColorPicker();
    });
    popup.appendChild(swatch);
  }
  anchor.appendChild(popup);
  activeColorPicker = popup;

  const closeOnClick = (e) => {
    if (!popup.contains(e.target) && !anchor.contains(e.target)) {
      closeColorPicker();
      document.removeEventListener('mousedown', closeOnClick);
    }
  };
  setTimeout(() => document.addEventListener('mousedown', closeOnClick), 0);
}

function closeColorPicker() {
  if (activeColorPicker) {
    activeColorPicker.remove();
    activeColorPicker = null;
  }
}

export function initToolbar() {
  const toolbar = document.getElementById('toolbar');
  if (!toolbar) return;

  let textColor = '#000000';
  let highlightColor = '#FCB900';

  // --- Text style select ---
  const styleSelect = select('style-select', [
    { value: 'paragraph', label: 'Body' },
    { value: '1', label: 'Heading 1' },
    { value: '2', label: 'Heading 2' },
    { value: '3', label: 'Heading 3' },
  ], (val) => {
    const ed = getEditor();
    if (!ed) return;
    if (val === 'paragraph') {
      ed.chain().focus().setParagraph().run();
    } else {
      ed.chain().focus().toggleHeading({ level: parseInt(val) }).run();
    }
  });
  styleSelect.title = 'Text Style';

  // --- Font family select ---
  const fontSelect = select('font-select', [
    { value: 'Segoe UI', label: 'Segoe UI' },
    { value: 'Arial', label: 'Arial' },
    { value: 'Georgia', label: 'Georgia' },
    { value: 'Times New Roman', label: 'Times New Roman' },
    { value: 'Courier New', label: 'Courier New' },
    { value: 'Consolas', label: 'Consolas' },
    { value: 'Comic Sans MS', label: 'Comic Sans MS' },
  ], (val) => {
    const ed = getEditor();
    if (!ed) return;
    ed.chain().focus().setFontFamily(val).run();
  });
  fontSelect.title = 'Font Family';

  // --- Font size select ---
  const sizeSelect = select('size-select',
    FONT_SIZES.map(s => ({ value: String(s), label: s + 'pt' })),
    (val) => {
      const ed = getEditor();
      if (!ed) return;
      ed.chain().focus().setMark('textStyle', { fontSize: val }).run();
    }
  );
  sizeSelect.value = '16';
  sizeSelect.title = 'Font Size';

  // --- Format buttons ---
  const boldBtn = btn('bold-btn', 'Bold (Ctrl+B)', '<b>B</b>', () => {
    getEditor()?.chain().focus().toggleBold().run();
  });
  const italicBtn = btn('italic-btn', 'Italic (Ctrl+I)', '<i>I</i>', () => {
    getEditor()?.chain().focus().toggleItalic().run();
  });
  const underlineBtn = btn('underline-btn', 'Underline (Ctrl+U)', '<u>U</u>', () => {
    getEditor()?.chain().focus().toggleUnderline().run();
  });
  const strikeBtn = btn('strike-btn', 'Strikethrough', '<s>S</s>', () => {
    getEditor()?.chain().focus().toggleStrike().run();
  });

  // --- Text color ---
  const textColorBtn = document.createElement('button');
  textColorBtn.className = 'toolbar-btn toolbar-color-btn';
  textColorBtn.title = 'Text Color';
  textColorBtn.innerHTML = '<span style="font-weight:700">A</span><span class="color-indicator" style="background:#000000"></span>';
  textColorBtn.addEventListener('mousedown', e => e.preventDefault());
  textColorBtn.addEventListener('click', () => {
    soundEngine.play('click');
    showColorPicker(textColorBtn, (color) => {
      textColor = color;
      textColorBtn.querySelector('.color-indicator').style.background = color;
      getEditor()?.chain().focus().setColor(color).run();
    });
  });

  // --- Highlight color ---
  const highlightBtn = document.createElement('button');
  highlightBtn.className = 'toolbar-btn toolbar-color-btn';
  highlightBtn.title = 'Highlight';
  highlightBtn.innerHTML = '<span style="font-weight:700;font-size:12px">H</span><span class="color-indicator" style="background:#FCB900"></span>';
  highlightBtn.addEventListener('mousedown', e => e.preventDefault());
  highlightBtn.addEventListener('click', () => {
    soundEngine.play('click');
    showColorPicker(highlightBtn, (color) => {
      highlightColor = color;
      highlightBtn.querySelector('.color-indicator').style.background = color;
      getEditor()?.chain().focus().toggleHighlight({ color }).run();
    });
  });

  // --- Alignment ---
  const alignLeftBtn = btn('align-left-btn', 'Align Left', '\u2261', () => {
    getEditor()?.chain().focus().setTextAlign('left').run();
  });
  const alignCenterBtn = btn('align-center-btn', 'Align Center', '\u2263', () => {
    getEditor()?.chain().focus().setTextAlign('center').run();
  });
  const alignRightBtn = btn('align-right-btn', 'Align Right', '\u2262', () => {
    getEditor()?.chain().focus().setTextAlign('right').run();
  });
  const alignJustifyBtn = btn('align-justify-btn', 'Justify', '\u2630', () => {
    getEditor()?.chain().focus().setTextAlign('justify').run();
  });

  // --- Lists ---
  const bulletBtn = btn('bullet-btn', 'Bullet List', '\u2022', () => {
    getEditor()?.chain().focus().toggleBulletList().run();
  });
  const orderedBtn = btn('ordered-btn', 'Numbered List', '1.', () => {
    getEditor()?.chain().focus().toggleOrderedList().run();
  });
  const checkBtn = btn('check-btn', 'Checklist', '\u2611', () => {
    getEditor()?.chain().focus().toggleTaskList().run();
  });

  // --- Indent/Outdent ---
  const indentBtn = btn('indent-btn', 'Indent', '\u21E5', () => {
    const ed = getEditor();
    if (!ed) return;
    if (ed.can().sinkListItem('listItem')) {
      ed.chain().focus().sinkListItem('listItem').run();
    } else if (ed.can().sinkListItem('taskItem')) {
      ed.chain().focus().sinkListItem('taskItem').run();
    }
  });
  const outdentBtn = btn('outdent-btn', 'Outdent', '\u21E4', () => {
    const ed = getEditor();
    if (!ed) return;
    if (ed.can().liftListItem('listItem')) {
      ed.chain().focus().liftListItem('listItem').run();
    } else if (ed.can().liftListItem('taskItem')) {
      ed.chain().focus().liftListItem('taskItem').run();
    }
  });

  // --- Link ---
  const linkBtn = btn('link-btn', 'Insert Link', '\uD83D\uDD17', () => {
    const ed = getEditor();
    if (!ed) return;
    const prev = ed.getAttributes('link').href || '';
    const url = prompt('Enter URL:', prev);
    if (url === null) return;
    if (url === '') {
      ed.chain().focus().unsetLink().run();
    } else {
      ed.chain().focus().extendMarkRange('link').setLink({ href: url }).run();
    }
  });

  // --- Insert dropdown ---
  const insertSelect = select('insert-select', [
    { value: '', label: 'Insert...' },
    { value: 'hr', label: 'Horizontal Rule' },
    { value: 'codeblock', label: 'Code Block' },
    { value: 'blockquote', label: 'Blockquote' },
    { value: 'table', label: 'Table 3x3' },
    { value: 'image', label: 'Image' },
  ], (val) => {
    const ed = getEditor();
    if (!ed) return;
    switch (val) {
      case 'hr':
        ed.chain().focus().setHorizontalRule().run();
        break;
      case 'codeblock':
        ed.chain().focus().toggleCodeBlock().run();
        break;
      case 'blockquote':
        ed.chain().focus().toggleBlockquote().run();
        break;
      case 'table':
        ed.chain().focus().insertTable({ rows: 3, cols: 3, withHeaderRow: true }).run();
        break;
      case 'image': {
        const url = prompt('Enter image URL or base64:');
        if (url) ed.chain().focus().setImage({ src: url }).run();
        break;
      }
    }
    insertSelect.value = '';
  });

  // --- Assemble toolbar ---
  toolbar.appendChild(group(styleSelect));
  toolbar.appendChild(group(fontSelect));
  toolbar.appendChild(group(sizeSelect));
  toolbar.appendChild(divider());
  toolbar.appendChild(group(boldBtn, italicBtn, underlineBtn, strikeBtn));
  toolbar.appendChild(group(textColorBtn, highlightBtn));
  toolbar.appendChild(divider());
  toolbar.appendChild(group(alignLeftBtn, alignCenterBtn, alignRightBtn, alignJustifyBtn));
  toolbar.appendChild(divider());
  toolbar.appendChild(group(bulletBtn, orderedBtn, checkBtn));
  toolbar.appendChild(group(indentBtn, outdentBtn));
  toolbar.appendChild(divider());
  toolbar.appendChild(group(linkBtn, insertSelect));

  // --- Update active states on selection change ---
  window.app?.eventBus?.on('editor:selection', (e) => {
    const ed = e.detail?.editor;
    if (!ed) return;

    boldBtn.classList.toggle('active', ed.isActive('bold'));
    italicBtn.classList.toggle('active', ed.isActive('italic'));
    underlineBtn.classList.toggle('active', ed.isActive('underline'));
    strikeBtn.classList.toggle('active', ed.isActive('strike'));

    bulletBtn.classList.toggle('active', ed.isActive('bulletList'));
    orderedBtn.classList.toggle('active', ed.isActive('orderedList'));
    checkBtn.classList.toggle('active', ed.isActive('taskList'));

    alignLeftBtn.classList.toggle('active', ed.isActive({ textAlign: 'left' }));
    alignCenterBtn.classList.toggle('active', ed.isActive({ textAlign: 'center' }));
    alignRightBtn.classList.toggle('active', ed.isActive({ textAlign: 'right' }));
    alignJustifyBtn.classList.toggle('active', ed.isActive({ textAlign: 'justify' }));

    // Update style select
    if (ed.isActive('heading', { level: 1 })) styleSelect.value = '1';
    else if (ed.isActive('heading', { level: 2 })) styleSelect.value = '2';
    else if (ed.isActive('heading', { level: 3 })) styleSelect.value = '3';
    else styleSelect.value = 'paragraph';

    // Update font family
    const fontFamily = ed.getAttributes('textStyle').fontFamily;
    if (fontFamily) fontSelect.value = fontFamily;

    // Update font size
    const fontSize = ed.getAttributes('textStyle').fontSize;
    if (fontSize) sizeSelect.value = String(fontSize);
  });
}
