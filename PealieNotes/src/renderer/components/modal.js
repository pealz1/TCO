import { soundEngine } from '../sounds.js';

export function showModal({ title, body, buttons = [], persistent = false }) {
  return new Promise((resolve) => {
    const backdrop = document.createElement('div');
    backdrop.className = 'modal-backdrop';

    const content = document.createElement('div');
    content.className = 'modal-content';

    content.innerHTML = `
      <div class="modal-header">
        <h3>${title}</h3>
        <button class="modal-close">✕</button>
      </div>
      <div class="modal-body">${typeof body === 'string' ? body : ''}</div>
      <div class="modal-footer">
        ${buttons.map(b => `<button class="modal-btn ${b.primary ? 'modal-btn-primary' : 'modal-btn-secondary'}" data-id="${b.id}">${b.label}</button>`).join('')}
      </div>
    `;

    if (typeof body !== 'string') {
      content.querySelector('.modal-body').appendChild(body);
    }

    backdrop.appendChild(content);
    document.getElementById('modal-container').appendChild(backdrop);

    const close = (result) => {
      backdrop.style.animation = 'fadeOut 150ms ease forwards';
      content.style.animation = 'scaleOut 150ms ease forwards';
      setTimeout(() => { backdrop.remove(); resolve(result); }, 150);
    };

    content.querySelectorAll('.modal-btn').forEach(btn => {
      btn.onclick = () => { soundEngine.play('click'); close(btn.dataset.id); };
    });

    content.querySelector('.modal-close').onclick = () => close(null);

    if (!persistent) {
      backdrop.addEventListener('click', (e) => { if (e.target === backdrop) close(null); });
    }

    const onEscape = (e) => {
      if (e.key === 'Escape' && !persistent) { document.removeEventListener('keydown', onEscape); close(null); }
    };
    document.addEventListener('keydown', onEscape);

    const firstBtn = content.querySelector('.modal-btn-primary') || content.querySelector('.modal-btn');
    if (firstBtn) firstBtn.focus();
  });
}

export function showConfirm(message, { title = 'Confirm', confirmLabel = 'OK', cancelLabel = 'Cancel' } = {}) {
  return showModal({
    title,
    body: `<p>${message}</p>`,
    buttons: [
      { id: 'cancel', label: cancelLabel },
      { id: 'confirm', label: confirmLabel, primary: true }
    ]
  }).then(result => result === 'confirm');
}

export function showPrompt(message, { title = 'Input', defaultValue = '', placeholder = '' } = {}) {
  const input = document.createElement('input');
  input.type = 'text';
  input.className = 'modal-input';
  input.value = defaultValue;
  input.placeholder = placeholder;

  const body = document.createElement('div');
  body.innerHTML = `<p>${message}</p>`;
  body.appendChild(input);

  return showModal({
    title,
    body,
    buttons: [
      { id: 'cancel', label: 'Cancel' },
      { id: 'confirm', label: 'OK', primary: true }
    ]
  }).then(result => result === 'confirm' ? input.value : null);
}
