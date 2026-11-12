// The storage of core's own storage plugin, as Storage has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#storage):
// two buttons, as today's `preserve_board` plugin gives Import and Export.
// Save has the browser download the board's file. Open opens the board in a
// file that the user picks, and the storage leaves it out under the load lock,
// which fixes the app to its opening board.
// The plugin's class, blockr.core-file_storage_plugin, keeps no state and has
// no `ui`, since a frontend draws its controls, and an app runs it unless
// another of its plugins is a storage plugin.
// The store writes the text of the board's file and opens such a text, so core
// only moves the text between the page and the user's disk.

import type { Storage, StorageControl } from "../store/index.ts";
import type { Board } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import { asError } from "../util/index.ts";

// The storage of core's own storage plugin. Its buttons reach for the document
// given, or else for the page's, only as they run.
export function coreStorage(doc?: Document): Storage {
  return {
    controls({ core }) {
      const save: StorageControl = {
        kind: "button",
        id: "save",
        label: "Save",
        run: () => saveBoard(core, doc ?? document),
      };
      const open: StorageControl = {
        kind: "button",
        id: "open",
        label: "Open",
        run: () => openBoard(core, doc ?? document),
      };
      return core.lock.load ? [save] : [save, open];
    },
  };
}

// Has the browser download the board's file, named by the board's id and the
// time.
function saveBoard(core: Pick<CoreApi, "store">, doc: Document): Promise<void> {
  try {
    download(core.store.save(), fileName(core.store.board()), doc);
    return Promise.resolve();
  } catch (error) {
    return Promise.reject(asError(error));
  }
}

// Asks the user for a file, and opens the board that it holds, while nothing
// opens where the user picks none. It asks before it awaits anything, since a
// browser shows its file picker only within the user's click or key press.
// Core starts again for the board that opens, so what the store's open()
// resolves with goes unread here: core names what the store dropped in a note
// to the frontend that draws the board.
async function openBoard(core: Pick<CoreApi, "store">, doc: Document): Promise<void> {
  const text = await pickFile(doc);
  if (text !== null) await core.store.open(text);
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
// where the user picks none. Open calls it as it runs, within the user's click
// or key press, since the browser shows the picker only then.
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
