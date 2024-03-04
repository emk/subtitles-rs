import { app, BrowserWindow, dialog, Menu, MenuItemConstructorOptions, OpenDialogOptions } from 'electron'
import { readFile } from 'fs-extra'

const isDevelopment = process.env.NODE_ENV !== 'production'

// Global reference to mainWindow
// Necessary to prevent win from being garbage collected
let mainWindow: BrowserWindow | null = null

function createMainWindow() {
  // Construct new BrowserWindow
  const window = new BrowserWindow({
    webPreferences: {
      // Allowing loading file:// URLs, etc., for now.
      webSecurity: false
    }
  })

  // Set url for `win`
    // points to `webpack-dev-server` in development
    // points to `index.html` in production
  const url = isDevelopment
    ? `http://localhost:${process.env.ELECTRON_WEBPACK_WDS_PORT}`
    : `file://${__dirname}/index.html`

  if (isDevelopment) {
    window.webContents.openDevTools()
  }

  window.loadURL(url)

  window.on('closed', () => {
    mainWindow = null
  })

  window.webContents.on('devtools-opened', () => {
    window.focus()
    setImmediate(() => {
      window.focus()
    })
  })

  return window
}

function ensureMainWindow(): BrowserWindow {
  if (mainWindow === null)
    mainWindow = createMainWindow()
  return mainWindow
}

function openFile() {
  const dialogOpts: OpenDialogOptions = {
    properties: ['openFile'],
    filters: [{ name: 'Output from substudy-backend', extensions: ['json'] }],
  }
  dialog.showOpenDialog(dialogOpts, async (filePaths) => {
    if (filePaths.length == 0) {
      return;
    }
    const path = filePaths[0]

    try {
      const data = await readFile(path, "utf-8")
      const json = JSON.parse(data)
      const window = ensureMainWindow()
      window.webContents.send('open-file', json)
    } catch (err) {
      window.alert(`Error: ${err}`)
    }
  })
}

function setMenu() {
  const template: MenuItemConstructorOptions[] = [
    {
      label: 'File',
      submenu: [
        {
          label: 'Open…',
          accelerator: 'CmdOrCtrl+O',
          click: openFile,
        },
        { role: 'quit' },
      ],
    },
    {
      label: 'Edit',
      submenu: [
        { role: 'undo' },
        { role: 'redo' },
        { type: 'separator' },
        { role: 'cut' },
        { role: 'copy' },
        { role: 'paste' },
        { role: 'pasteandmatchstyle' },
        { role: 'delete' },
        { role: 'selectall' },
      ],
    },
    {
      role: 'window',
      submenu: [
        { role: 'minimize' },
        { role: 'close' },
      ]
    },
  ]
  const menu = Menu.buildFromTemplate(template)
  Menu.setApplicationMenu(menu)
}

// Quit application when all windows are closed
app.on('window-all-closed', () => {
  // On macOS it is common for applications to stay open
  // until the user explicitly quits
  if (process.platform !== 'darwin') app.quit()
})

app.on('activate', () => {
  // On macOS it is common to re-create a window
  // even after all windows have been closed
  ensureMainWindow()
})

// Create main BrowserWindow when electron is ready
app.on('ready', () => {
  app.setName("Substudy")
  setMenu()
  mainWindow = createMainWindow()
})
