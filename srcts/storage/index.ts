// Core's own storage plugin, whose buttons save a board as a file that the
// browser downloads and open a file that the user picks, with the helpers that
// move a board's text between the page and the user's disk.

export { coreStorage, download, fileName, pickFile } from "./storage.ts";
