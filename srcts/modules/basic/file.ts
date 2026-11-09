// Saving the board as a file that the browser downloads, and reading a file
// that the user picks, as today's `preserve_board` plugin does.
// The store writes the text of the board's file and opens such a text, so the
// frontend only moves the text between the page and the user's disk.

import type { Board } from "../../types/board.ts";

// The name of the board's file, as today's: the board's id and the local time
// it was saved, as "brisk_otter_2026-11-06_14-03-09.json".
export function fileName(board: Pick<Board, "id">, now: Date = new Date()): string {
  const two = (n: number) => String(n).padStart(2, "0");
  const date = `${String(now.getFullYear())}-${two(now.getMonth() + 1)}-${two(now.getDate())}`;
  const time = `${two(now.getHours())}-${two(now.getMinutes())}-${two(now.getSeconds())}`;
  return `${board.id}_${date}_${time}.json`;
}

// Has the browser download the text as a JSON file of the name given.
export function download(text: string, name: string, doc: Document = document): void {
  const url = URL.createObjectURL(new Blob([text], { type: "application/json" }));
  const link = doc.createElement("a");
  link.href = url;
  link.download = name;
  link.hidden = true;
  doc.body.append(link);
  link.click();
  link.remove();
  // The download has started once click() returns.
  setTimeout(() => {
    URL.revokeObjectURL(url);
  }, 0);
}
