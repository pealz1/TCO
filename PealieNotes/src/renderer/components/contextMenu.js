import { soundEngine } from '../sounds.js';

let currentMenu = null;

export function showContextMenu(x, y, items) {
  hideContextMenu();

  const menu = document.createElement('div');
  menu.className = 'context-menu animate-context-menu-enter';

  items.forEach(item => {
    if (item.separator) {
      const sep = document.createElement('div');
      sep.className = 'context-menu-separator';
      menu.appendChild(sep);
      return;
    }

    const el = document.createElement('div');
    el.className = 'context-menu-item' + (item.disabled ? ' disabled' : '');
    el.innerHTML = `${item.icon || ''} <span>${item.label}</span>${item.shortcut ? `<span class="shortcut">${item.shortcut}</span>` : ''}`;
    el.onclick = () => {
      if (!item.disabled && item.action) {
        soundEngine.play('click');
        item.action();
      }
      hideContextMenu();
    };
    menu.appendChild(el);
  });

  document.getElementById('context-menu-container').appendChild(menu);

  const rect = menu.getBoundingClientRect();
  const maxX = window.innerWidth - rect.width - 8;
  const maxY = window.innerHeight - rect.height - 8;
  menu.style.left = Math.min(x, maxX) + 'px';
  menu.style.top = Math.min(y, maxY) + 'px';

  currentMenu = menu;

  setTimeout(() => {
    document.addEventListener('click', handleOutsideClick);
    document.addEventListener('keydown', handleEscape);
  }, 0);
}

export function hideContextMenu() {
  if (currentMenu) {
    currentMenu.remove();
    currentMenu = null;
  }
  document.removeEventListener('click', handleOutsideClick);
  document.removeEventListener('keydown', handleEscape);
}

function handleOutsideClick(e) {
  if (currentMenu && !currentMenu.contains(e.target)) hideContextMenu();
}

function handleEscape(e) {
  if (e.key === 'Escape') hideContextMenu();
}
