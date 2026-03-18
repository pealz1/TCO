const themes = [
  {
    id: 'light',
    name: 'Light',
    description: 'Clean and airy',
    category: 'light',
    previewColors: ['#ffffff', '#f5f5f7', '#007AFF'],
  },
  {
    id: 'dark',
    name: 'Dark',
    description: 'Easy on the eyes',
    category: 'dark',
    previewColors: ['#1e1e1e', '#252526', '#4FC3F7'],
  },
  {
    id: 'midnight',
    name: 'Midnight',
    description: 'Pure black for OLED displays',
    category: 'dark',
    previewColors: ['#000000', '#0a0a0a', '#5E9FFF'],
  },
  {
    id: 'burple',
    name: 'Burple',
    description: 'Deep blue-purple with Discord flair',
    category: 'dark',
    previewColors: ['#1a1a2e', '#16213e', '#5865F2'],
  },
  {
    id: 'ocean-gradient',
    name: 'Ocean Gradient',
    description: 'Deep sea blues and teals',
    category: 'gradient',
    previewColors: ['#0f2027', '#2c5364', '#00d2ff'],
  },
  {
    id: 'sunset-gradient',
    name: 'Sunset Gradient',
    description: 'Warm twilight purples and reds',
    category: 'gradient',
    previewColors: ['#1a1a2e', '#3d1c3c', '#ff6b6b'],
  },
  {
    id: 'rose',
    name: 'Rose',
    description: 'Soft pinks and warm blush tones',
    category: 'light',
    previewColors: ['#fff5f5', '#fce4ec', '#e91e63'],
  },
  {
    id: 'forest',
    name: 'Forest',
    description: 'Earthy greens for focused work',
    category: 'dark',
    previewColors: ['#1b2a1b', '#254028', '#4CAF50'],
  },
  {
    id: 'lavender',
    name: 'Lavender',
    description: 'Gentle purples, light and dreamy',
    category: 'light',
    previewColors: ['#f3e5f5', '#e1bee7', '#9C27B0'],
  },
  {
    id: 'nord',
    name: 'Nord',
    description: 'Arctic blues, cool and minimal',
    category: 'dark',
    previewColors: ['#2e3440', '#434c5e', '#88c0d0'],
  },
];

export function setTheme(name) {
  document.documentElement.setAttribute('data-theme', name);
}

export function setAccentColor(color) {
  document.documentElement.style.setProperty('--accent', color);

  const hover = _shiftLightness(color, -12);
  document.documentElement.style.setProperty('--accent-hover', hover);

  const text = _contrastingText(color);
  document.documentElement.style.setProperty('--accent-text', text);
}

export function getThemeList() {
  return themes;
}

export function getCurrentTheme() {
  return document.documentElement.getAttribute('data-theme') || 'light';
}

// ── Internal helpers ──────────────────────────────────────────────────────

function _hexToRgb(hex) {
  const clean = hex.replace('#', '');
  const full = clean.length === 3
    ? clean.split('').map(c => c + c).join('')
    : clean;
  const num = parseInt(full, 16);
  return [(num >> 16) & 255, (num >> 8) & 255, num & 255];
}

function _rgbToHex(r, g, b) {
  return '#' + [r, g, b]
    .map(v => Math.max(0, Math.min(255, Math.round(v))).toString(16).padStart(2, '0'))
    .join('');
}

// Shift perceived lightness by `amount` (negative = darker, positive = lighter)
function _shiftLightness(hex, amount) {
  const [r, g, b] = _hexToRgb(hex);
  const factor = amount / 100;
  if (factor < 0) {
    return _rgbToHex(r * (1 + factor), g * (1 + factor), b * (1 + factor));
  }
  return _rgbToHex(
    r + (255 - r) * factor,
    g + (255 - g) * factor,
    b + (255 - b) * factor,
  );
}

// WCAG relative luminance
function _luminance(r, g, b) {
  const s = [r, g, b].map(v => {
    const c = v / 255;
    return c <= 0.03928 ? c / 12.92 : Math.pow((c + 0.055) / 1.055, 2.4);
  });
  return 0.2126 * s[0] + 0.7152 * s[1] + 0.0722 * s[2];
}

function _contrastingText(hex) {
  const [r, g, b] = _hexToRgb(hex);
  const lum = _luminance(r, g, b);
  const onWhite = (1.05) / (lum + 0.05);
  const onBlack = (lum + 0.05) / (0.05);
  return onWhite >= onBlack ? '#ffffff' : '#000000';
}
