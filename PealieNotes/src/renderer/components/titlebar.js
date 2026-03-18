import { soundEngine } from '../sounds.js';
import { store } from '../store.js';
import { setTheme, getThemeList, getCurrentTheme } from '../themes.js';

export function initTitlebar() {
  const titlebar = document.getElementById('titlebar');
  titlebar.innerHTML = `
    <div class="titlebar-left">
      <span class="titlebar-icon">📝</span>
      <span class="titlebar-title">Pealie Notes</span>
    </div>
    <div class="titlebar-right">
      <button class="titlebar-btn theme-btn" title="Cycle theme (Ctrl+Shift+T)">🎨</button>
      <button class="titlebar-btn settings-btn" title="Settings (Ctrl+,)">⚙</button>
      <div class="window-controls">
        <button class="window-btn minimize-btn" title="Minimize">─</button>
        <button class="window-btn maximize-btn" title="Maximize">□</button>
        <button class="window-btn close-btn" title="Close">✕</button>
      </div>
    </div>
  `;

  titlebar.querySelector('.minimize-btn').onclick = () => { window.api.minimize(); soundEngine.play('click'); };
  titlebar.querySelector('.maximize-btn').onclick = async () => {
    const isMax = await window.api.isMaximized();
    if (isMax) window.api.unmaximize(); else window.api.maximize();
    soundEngine.play('click');
  };
  titlebar.querySelector('.close-btn').onclick = () => { window.api.close(); };

  titlebar.querySelector('.theme-btn').onclick = () => {
    const themes = getThemeList();
    const current = getCurrentTheme();
    const idx = themes.findIndex(t => t.id === current);
    const next = themes[(idx + 1) % themes.length];
    setTheme(next.id);
    store.set('theme', next.id);
    soundEngine.play('click');
  };

  titlebar.querySelector('.settings-btn').onclick = () => {
    const current = store.get('view');
    store.set('view', current === 'settings' ? 'main' : 'settings');
    soundEngine.play('click');
  };

  if (window.api.onMaximizeChange) {
    window.api.onMaximizeChange((isMax) => {
      titlebar.querySelector('.maximize-btn').textContent = isMax ? '❐' : '□';
      titlebar.querySelector('.maximize-btn').title = isMax ? 'Restore' : 'Maximize';
    });
  }

  titlebar.addEventListener('dblclick', async (e) => {
    if (e.target.closest('.titlebar-right')) return;
    const isMax = await window.api.isMaximized();
    if (isMax) window.api.unmaximize(); else window.api.maximize();
  });
}
