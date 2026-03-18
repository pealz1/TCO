const defaultState = {
  folders: [],
  notes: [],
  activeFolder: 'all',
  activeNote: null,
  config: {},
  view: 'main',
  searchQuery: '',
  sidebarCollapsed: false,
  theme: 'light',
  tags: []
};

export class Store {
  constructor(initialState) {
    this._state = { ...initialState };
    this._listeners = {};
  }

  get(key) {
    return this._state[key];
  }

  set(key, value) {
    if (this._state[key] === value) return;
    const prev = this._state[key];
    this._state[key] = value;
    const callbacks = this._listeners[key];
    if (callbacks) {
      for (const cb of callbacks.slice()) {
        cb(value, prev);
      }
    }
  }

  on(key, callback) {
    if (!this._listeners[key]) {
      this._listeners[key] = [];
    }
    this._listeners[key].push(callback);
    return () => this.off(key, callback);
  }

  off(key, callback) {
    const callbacks = this._listeners[key];
    if (!callbacks) return;
    const idx = callbacks.indexOf(callback);
    if (idx !== -1) {
      callbacks.splice(idx, 1);
    }
  }

  getState() {
    return { ...this._state };
  }
}

export const store = new Store(defaultState);
