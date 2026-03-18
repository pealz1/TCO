const { Tray, Menu, nativeImage, app } = require('electron');
let tray = null;

function createTray(mainWindow, iconPath) {
  let icon;
  if (iconPath) {
    try {
      icon = nativeImage.createFromPath(iconPath);
    } catch {
      icon = nativeImage.createEmpty();
    }
  }

  if (!icon || icon.isEmpty()) {
    // Fallback: 16x16 blue square
    icon = nativeImage.createFromBuffer(
      Buffer.from(
        'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAM0lEQVQ4T2NkYPj/n4EBBJgYSARMo' +
        'AZgYGBkZCTeBQyjBgy9MGBkHE0HpCcERkbS4wIAXH4FEVWbLl0AAAAASUVORK5CYII=',
        'base64'
      )
    );
  }

  tray = new Tray(icon.resize({ width: 16, height: 16 }));
  tray.setToolTip('Pealie Notes');

  const contextMenu = Menu.buildFromTemplate([
    {
      label: 'New Quick Note',
      click: () => {
        mainWindow.webContents.send('quick-note');
        mainWindow.show();
        mainWindow.focus();
      },
    },
    {
      label: 'Show Pealie Notes',
      click: () => {
        mainWindow.show();
        mainWindow.focus();
      },
    },
    { type: 'separator' },
    {
      label: 'Quit',
      click: () => {
        app.isQuitting = true;
        app.quit();
      },
    },
  ]);

  tray.setContextMenu(contextMenu);

  tray.on('click', () => {
    if (mainWindow.isVisible()) {
      mainWindow.hide();
    } else {
      mainWindow.show();
      mainWindow.focus();
    }
  });

  return tray;
}

function destroyTray() {
  if (tray) {
    tray.destroy();
    tray = null;
  }
}

module.exports = { createTray, destroyTray };
