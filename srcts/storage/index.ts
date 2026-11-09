// Core's own storage plugin, which saves a board as a file that the browser
// downloads and opens a file that the user picks, for every board that holds
// no storage plugin, with the helpers that move a board's text between the page
// and the user's disk.

export { coreStorage, download, fileName, pickFile } from "./storage.ts";
