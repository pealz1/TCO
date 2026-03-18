import { store } from '../store.js';
import * as storage from '../storage.js';
import { setTheme, getThemeList, getCurrentTheme } from '../themes.js';

const SPLASH_DURATION = 2000;
const TOTAL_STEPS = 5; // splash + 4 steps

let currentStep = 0;
let selectedName = '';
let selectedTheme = '';

export function initWelcome() {
  const screen = document.getElementById('welcome-screen');
  if (!screen) return;

  // Listen for first launch
  if (window.app && window.app.eventBus) {
    window.app.eventBus.on('app:ready', (e) => {
      if (e.detail && e.detail.firstLaunch) {
        showWelcome();
      }
    });
  }
}

export function showWelcome() {
  const screen = document.getElementById('welcome-screen');
  if (!screen) return;

  currentStep = 0;
  selectedName = '';
  selectedTheme = getCurrentTheme();
  screen.classList.add('visible');
  screen.style.display = 'flex';
  renderStep();
}

function hideWelcome() {
  const screen = document.getElementById('welcome-screen');
  if (!screen) return;
  screen.classList.remove('visible');
  screen.style.display = 'none';
}

function renderStep() {
  const screen = document.getElementById('welcome-screen');
  if (!screen) return;

  switch (currentStep) {
    case 0: renderSplash(screen); break;
    case 1: renderNameStep(screen); break;
    case 2: renderThemeStep(screen); break;
    case 3: renderFeaturesStep(screen); break;
    case 4: renderDoneStep(screen); break;
  }
}

function renderDots(activeIdx) {
  // Steps 1-4 show dots (not splash)
  const total = 4;
  return `<div class="welcome-dots">
    ${Array.from({ length: total }, (_, i) =>
      `<div class="dot${i === activeIdx ? ' active' : ''}"></div>`
    ).join('')}
  </div>`;
}

// ---- Step 0: Splash ----

function renderSplash(screen) {
  screen.innerHTML = `
    <div class="welcome-container welcome-splash">
      <div class="app-icon"><span class="app-icon-glow">&#x1F4DD;</span></div>
      <div class="app-name">Pealie Notes</div>
    </div>
  `;

  setTimeout(() => {
    currentStep = 1;
    renderStep();
  }, SPLASH_DURATION);
}

// ---- Step 1: Name ----

function renderNameStep(screen) {
  screen.innerHTML = `
    <div class="welcome-container">
      <div class="welcome-step active">
        <div class="step-title">Welcome to Pealie Notes</div>
        <div class="step-subtitle">What should we call you?</div>
        <input type="text" class="welcome-input" id="welcome-name" placeholder="Your name" value="${escHtml(selectedName)}" autofocus />
        <br>
        <button class="welcome-btn" id="welcome-continue" ${selectedName ? '' : 'disabled'}>Continue</button>
      </div>
      ${renderDots(0)}
    </div>
  `;

  const input = screen.querySelector('#welcome-name');
  const btn = screen.querySelector('#welcome-continue');

  input.addEventListener('input', () => {
    selectedName = input.value.trim();
    btn.disabled = !selectedName;
  });

  btn.addEventListener('click', () => {
    if (selectedName) goToStep(2);
  });

  input.addEventListener('keydown', (e) => {
    if (e.key === 'Enter' && selectedName) goToStep(2);
  });

  input.focus();
}

// ---- Step 2: Theme ----

function renderThemeStep(screen) {
  const themes = getThemeList();

  screen.innerHTML = `
    <div class="welcome-container">
      <div class="welcome-step active">
        <button class="welcome-back-btn" id="welcome-back">&larr; Back</button>
        <div class="step-title">Pick your style</div>
        <div class="step-subtitle">You can always change this later in settings</div>
        <div class="welcome-theme-grid">
          ${themes.map(t => `
            <div class="welcome-theme-card${t.id === selectedTheme ? ' selected' : ''}" data-theme="${t.id}" title="${t.name}">
              ${t.previewColors.map(c => `<div class="welcome-theme-bar" style="background:${c}"></div>`).join('')}
            </div>
          `).join('')}
        </div>
        <br>
        <button class="welcome-btn" id="welcome-continue">Continue</button>
      </div>
      ${renderDots(1)}
    </div>
  `;

  screen.querySelectorAll('.welcome-theme-card').forEach(card => {
    card.addEventListener('click', () => {
      selectedTheme = card.dataset.theme;
      setTheme(selectedTheme);
      screen.querySelectorAll('.welcome-theme-card').forEach(c => c.classList.remove('selected'));
      card.classList.add('selected');
    });
  });

  screen.querySelector('#welcome-back').addEventListener('click', () => goToStep(1));
  screen.querySelector('#welcome-continue').addEventListener('click', () => goToStep(3));
}

// ---- Step 3: Features ----

function renderFeaturesStep(screen) {
  const features = [
    { icon: '\u270F\uFE0F', title: 'Rich Formatting', desc: 'Bold, italic, headings, lists, code blocks, and more' },
    { icon: '\uD83D\uDCC1', title: 'Folders & Tags', desc: 'Organize your notes with folders and color-coded tags' },
    { icon: '\uD83D\uDCBE', title: 'Auto-Save', desc: 'Your notes are saved automatically as you type' },
    { icon: '\u2328\uFE0F', title: 'Keyboard Shortcuts', desc: 'Quick actions for everything, fully customizable' },
  ];

  screen.innerHTML = `
    <div class="welcome-container">
      <div class="welcome-step active">
        <button class="welcome-back-btn" id="welcome-back">&larr; Back</button>
        <div class="step-title">What you can do</div>
        <div class="step-subtitle">A few highlights to get you started</div>
        <div class="feature-cards">
          ${features.map(f => `
            <div class="feature-card">
              <div class="feature-icon">${f.icon}</div>
              <div class="feature-title">${f.title}</div>
              <div class="feature-desc">${f.desc}</div>
            </div>
          `).join('')}
        </div>
        <br>
        <button class="welcome-btn" id="welcome-continue">Continue</button>
      </div>
      ${renderDots(2)}
    </div>
  `;

  screen.querySelector('#welcome-back').addEventListener('click', () => goToStep(2));
  screen.querySelector('#welcome-continue').addEventListener('click', () => goToStep(4));
}

// ---- Step 4: Done ----

function renderDoneStep(screen) {
  screen.innerHTML = `
    <div class="welcome-container">
      <div class="welcome-step active">
        <div class="step-title">You're all set, ${escHtml(selectedName)}!</div>
        <div class="step-subtitle">Your notes are ready and waiting</div>
        <br>
        <button class="welcome-btn" id="welcome-start">Start Writing</button>
      </div>
      ${renderDots(3)}
    </div>
  `;

  screen.querySelector('#welcome-start').addEventListener('click', async () => {
    try {
      const config = await storage.loadConfig();
      config.firstLaunch = false;
      config.displayName = selectedName;
      config.theme = selectedTheme;
      await storage.saveConfig(config);
      store.set('config', config);
    } catch (err) {
      console.error('Welcome save error:', err);
    }
    hideWelcome();
  });
}

// ---- Navigation ----

function goToStep(step) {
  currentStep = step;
  renderStep();
}

function escHtml(str) {
  const div = document.createElement('div');
  div.textContent = str || '';
  return div.innerHTML;
}
