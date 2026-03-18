const { contextBridge, ipcRenderer } = require('electron');

contextBridge.exposeInMainWorld('api', {
  // File operations
  readFile: (filePath) => ipcRenderer.invoke('readFile', filePath),
  writeFile: (filePath, data) => ipcRenderer.invoke('writeFile', filePath, data),
  readDir: (dirPath) => ipcRenderer.invoke('readDir', dirPath),
  mkdir: (dirPath) => ipcRenderer.invoke('mkdir', dirPath),
  deleteFile: (filePath) => ipcRenderer.invoke('deleteFile', filePath),
  rename: (oldPath, newPath) => ipcRenderer.invoke('rename', oldPath, newPath),
  copyDir: (src, dest) => ipcRenderer.invoke('copyDir', src, dest),
  stat: (filePath) => ipcRenderer.invoke('stat', filePath),
  exists: (filePath) => ipcRenderer.invoke('exists', filePath),

  // Dialog operations
  showOpenDialog: (options) => ipcRenderer.invoke('showOpenDialog', options),
  showSaveDialog: (options) => ipcRenderer.invoke('showSaveDialog', options),

  // Path operations
  getAppDataPath: () => ipcRenderer.invoke('getAppDataPath'),
  getDocumentsPath: () => ipcRenderer.invoke('getDocumentsPath'),
  joinPath: (...parts) => ipcRenderer.invoke('joinPath', ...parts),

  // Window operations
  minimize: () => ipcRenderer.invoke('minimize'),
  maximize: () => ipcRenderer.invoke('maximize'),
  unmaximize: () => ipcRenderer.invoke('unmaximize'),
  close: () => ipcRenderer.invoke('close'),
  isMaximized: () => ipcRenderer.invoke('isMaximized'),

  // Window event listeners
  onMaximized: (cb) => ipcRenderer.on('window-maximized', cb),
  onUnmaximized: (cb) => ipcRenderer.on('window-unmaximized', cb),
  offMaximized: (cb) => ipcRenderer.off('window-maximized', cb),
  offUnmaximized: (cb) => ipcRenderer.off('window-unmaximized', cb),

  // Export
  printToPDF: (options) => ipcRenderer.invoke('printToPDF', options),
});
