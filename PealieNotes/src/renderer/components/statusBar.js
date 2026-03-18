export function initStatusBar() {
  const bar = document.getElementById('status-bar');
  if (!bar) return;

  bar.innerHTML = `
    <div class="status-left">
      <span class="word-count">0 words</span>
      <span class="char-count">0 chars</span>
    </div>
    <div class="status-center">
      <span class="save-indicator"></span>
    </div>
    <div class="status-right">
      <span class="cursor-pos">Ln 1, Col 1</span>
      <input type="range" class="zoom-slider" min="80" max="200" value="100" title="Zoom">
      <span class="zoom-label">100%</span>
    </div>
  `;

  window.app?.eventBus?.on('editor:update', (e) => {
    const data = e.detail;
    if (!data) return;
    bar.querySelector('.word-count').textContent = data.wordCount + ' words';
    bar.querySelector('.char-count').textContent = data.charCount + ' chars';
  });

  window.app?.eventBus?.on('editor:saving', () => {
    bar.querySelector('.save-indicator').textContent = 'Saving...';
  });

  window.app?.eventBus?.on('editor:saved', () => {
    const indicator = bar.querySelector('.save-indicator');
    indicator.textContent = 'Saved';
    setTimeout(() => { indicator.textContent = ''; }, 2000);
  });

  const zoom = bar.querySelector('.zoom-slider');
  const zoomLabel = bar.querySelector('.zoom-label');
  zoom.addEventListener('input', () => {
    const val = zoom.value;
    zoomLabel.textContent = val + '%';
    document.getElementById('editor-container').style.zoom = val / 100;
  });
}
