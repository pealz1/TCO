const { globalShortcut } = require('electron');

function registerGlobalShortcuts(mainWindow) {
  try {
    globalShortcut.register('Ctrl+Shift+N', () => {
      mainWindow.webContents.send('quick-note');
      mainWindow.show();
      mainWindow.focus();
    });
  } catch {
    // Global shortcut registration can fail silently
  }
}

function unregisterAll() {
  globalShortcut.unregisterAll();
}

module.exports = { registerGlobalShortcuts, unregisterAll };
