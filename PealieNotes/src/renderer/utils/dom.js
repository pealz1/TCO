export function createElement(tag, options = {}) {
  const { className, attrs = {}, children = [], text, onClick } = options;
  const el = document.createElement(tag);

  if (className) {
    el.className = className;
  }

  for (const [key, value] of Object.entries(attrs)) {
    el.setAttribute(key, value);
  }

  if (text != null) {
    el.textContent = text;
  }

  for (const child of children) {
    el.appendChild(child);
  }

  if (onClick) {
    el.addEventListener('click', onClick);
  }

  return el;
}

export function $(selector, context = document) {
  return context.querySelector(selector);
}

export function $$(selector, context = document) {
  return context.querySelectorAll(selector);
}
