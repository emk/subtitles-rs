import { ipcRenderer } from 'electron'

// We include `Index.bs`, which really loads the generated `Index.bs.js` file
// and manually-written `Index.bs.d.ts` file.
import { renderPlaceholder, renderVideoJson } from './Index.bs'

// Receive "Open file..." events from the main process.
ipcRenderer.on('open-file', (_event: any, data: any) => {
    console.log("Open file:", data)
    renderVideoJson(data)
})

renderPlaceholder()
