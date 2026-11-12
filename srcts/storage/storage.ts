// The storage of core's own storage plugin, as Storage has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#storage):
// it saves a board as a file that the browser downloads, and opens a file that
// the user picks, as today's `preserve_board` plugin does. The plugin's class,
// blockr.core-file_storage_plugin, keeps no state and has no `ui`, since a
// frontend draws the controls that call it, and an app runs it unless another
// of its plugins is a storage plugin.
// The store writes the text of the board's file and opens such a text, so core
// only moves the text between the page and the user's disk.

import type { Storage } from "../store/index.ts";
import type { Board } from "../types/board.ts";
import { asError, isObject } from "../util/index.ts";

// The storage of core's own storage plugin.
export function coreStorage(doc: Document = document): Storage {
  return {
    save(board) {
      try {
        download(board, fileName({ id: boardIdFromText(board) }), doc);
        return Promise.resolve();
      } catch (error) {
        return Promise.reject(asError(error));
      }
    },
    open: () => pickFile(doc),
  };
}

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

// Asks the user for a JSON file, and resolves with its text, or with null
// where the user picks none. A frontend calls it as the user clicks its open
// control, since the browser shows the picker only within such a click.
export function pickFile(doc: Document = document): Promise<string | null> {
  return new Promise((resolve, reject) => {
    const input = doc.createElement("input");
    input.type = "file";
    input.accept = ".json,application/json";
    input.hidden = true;
    input.addEventListener(
      "change",
      () => {
        const file = input.files?.[0];
        input.remove();
        if (file === undefined) resolve(null);
        else file.text().then(resolve, reject);
      },
      { once: true },
    );
    input.addEventListener(
      "cancel",
      () => {
        input.remove();
        resolve(null);
      },
      { once: true },
    );
    doc.body.append(input);
    input.click();
  });
}

// The id of the board that a JSON board's text holds, or "board" where the
// text holds none, so that a text that the store did not write still saves.
function boardIdFromText(text: string): string {
  let json: unknown;
  try {
    json = JSON.parse(text);
  } catch {
    return "board";
  }
  const board = isObject(json) ? json.board : undefined;
  return isObject(board) && typeof board.id === "string" && board.id !== "" ? board.id : "board";
}
