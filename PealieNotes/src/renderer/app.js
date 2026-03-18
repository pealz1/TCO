class EventBus extends EventTarget {
  emit(event, detail) {
    this.dispatchEvent(new CustomEvent(event, { detail }));
  }

  on(event, handler) {
    this.addEventListener(event, handler);
  }

  off(event, handler) {
    this.removeEventListener(event, handler);
  }

  once(event, handler) {
    this.addEventListener(event, handler, { once: true });
  }
}

const app = {
  eventBus: new EventBus(),

  async init() {
    let firstLaunch = false;

    try {
      const appDataPath = await window.api.getAppDataPath();
      const configPath = await window.api.joinPath(appDataPath, 'pealie-notes', 'config.json');
      const configExists = await window.api.exists(configPath);
      firstLaunch = !configExists;
    } catch {
      firstLaunch = true;
    }

    if (firstLaunch) {
      console.log('Pealie Notes: first launch detected');
    }

    console.log('Pealie Notes initialized');
    app.eventBus.emit('app:ready', { firstLaunch });
  },
};

window.addEventListener('DOMContentLoaded', () => {
  app.init();
});

export { app, EventBus };
