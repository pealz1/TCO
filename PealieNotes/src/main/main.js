const { app, BrowserWindow, ipcMain, dialog, shell } = require('electron');
const path = require('path');
const fs = require('fs');
const os = require('os');
const { createTray, destroyTray } = require('./tray.js');
const { registerGlobalShortcuts, unregisterAll } = require('./globalShortcuts.js');

const gotTheLock = app.requestSingleInstanceLock();

if (!gotTheLock) {
  app.quit();
} else {
  app.on('second-instance', () => {
    if (mainWindow) {
      if (mainWindow.isMinimized()) mainWindow.restore();
      mainWindow.focus();
    }
  });
}

let mainWindow = null;

function createWindow() {
  mainWindow = new BrowserWindow({
    width: 1200,
    height: 800,
    minWidth: 800,
    minHeight: 600,
    frame: false,
    titleBarStyle: 'hidden',
    backgroundColor: '#1e1e1e',
    show: false,
    webPreferences: {
      spellcheck: true,
      contextIsolation: true,
      nodeIntegration: false,
      preload: path.join(__dirname, '../preload/preload.js'),
    },
  });

  mainWindow.loadFile(path.join(__dirname, '../renderer/index.html'));

  mainWindow.once('ready-to-show', () => {
    mainWindow.show();
  });

  mainWindow.on('maximize', () => {
    mainWindow.webContents.send('window-maximized');
  });

  mainWindow.on('unmaximize', () => {
    mainWindow.webContents.send('window-unmaximized');
  });

  // Close-to-tray: hide instead of quit if config says so
  mainWindow.on('close', (event) => {
    if (app.isQuitting) return;
    // Read config to check minimizeToTray
    try {
      const configPath = path.join(app.getPath('userData'), 'config.json');
      if (fs.existsSync(configPath)) {
        const config = JSON.parse(fs.readFileSync(configPath, 'utf-8'));
        if (config.minimizeToTray !== false) {
          event.preventDefault();
          mainWindow.hide();
          return;
        }
      }
    } catch {
      // If config read fails, just close normally
    }
  });
}

app.whenReady().then(() => {
  createWindow();

  // System tray
  const iconPath = path.join(__dirname, '../assets/icon.svg');
  createTray(mainWindow, iconPath);

  // Global shortcuts
  registerGlobalShortcuts(mainWindow);

  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) createWindow();
  });
});

app.on('before-quit', () => {
  app.isQuitting = true;
});

app.on('will-quit', () => {
  unregisterAll();
  destroyTray();
});

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit();
});

// File operations
ipcMain.handle('readFile', async (_event, filePath) => {
  return fs.promises.readFile(filePath, 'utf-8');
});

ipcMain.handle('writeFile', async (_event, filePath, data) => {
  await fs.promises.mkdir(path.dirname(filePath), { recursive: true });
  return fs.promises.writeFile(filePath, data, 'utf-8');
});

ipcMain.handle('readDir', async (_event, dirPath) => {
  const entries = await fs.promises.readdir(dirPath, { withFileTypes: true });
  return entries.map(e => ({
    name: e.name,
    isDirectory: e.isDirectory(),
    isFile: e.isFile(),
  }));
});

ipcMain.handle('mkdir', async (_event, dirPath) => {
  return fs.promises.mkdir(dirPath, { recursive: true });
});

ipcMain.handle('deleteFile', async (_event, filePath) => {
  return fs.promises.rm(filePath, { recursive: true, force: true });
});

ipcMain.handle('rename', async (_event, oldPath, newPath) => {
  return fs.promises.rename(oldPath, newPath);
});

ipcMain.handle('copyDir', async (_event, src, dest) => {
  return fs.promises.cp(src, dest, { recursive: true });
});

ipcMain.handle('stat', async (_event, filePath) => {
  const s = await fs.promises.stat(filePath);
  return {
    size: s.size,
    mtimeMs: s.mtimeMs,
    ctimeMs: s.ctimeMs,
    isDirectory: s.isDirectory(),
    isFile: s.isFile(),
  };
});

ipcMain.handle('exists', async (_event, filePath) => {
  try {
    await fs.promises.access(filePath);
    return true;
  } catch {
    return false;
  }
});

// Dialog operations
ipcMain.handle('showOpenDialog', async (_event, options) => {
  return dialog.showOpenDialog(mainWindow, options);
});

ipcMain.handle('showSaveDialog', async (_event, options) => {
  return dialog.showSaveDialog(mainWindow, options);
});

// Path operations
ipcMain.handle('getAppDataPath', () => {
  return app.getPath('userData');
});

ipcMain.handle('getDocumentsPath', () => {
  return app.getPath('documents');
});

ipcMain.handle('joinPath', (_event, ...parts) => {
  return path.join(...parts);
});

// Window operations
ipcMain.handle('minimize', () => {
  mainWindow.minimize();
});

ipcMain.handle('maximize', () => {
  mainWindow.maximize();
});

ipcMain.handle('unmaximize', () => {
  mainWindow.unmaximize();
});

ipcMain.handle('close', () => {
  mainWindow.close();
});

ipcMain.handle('isMaximized', () => {
  return mainWindow.isMaximized();
});

// Export
ipcMain.handle('printToPDF', async (_event, options) => {
  const pdfData = await mainWindow.webContents.printToPDF(options || {});
  return pdfData;
});
