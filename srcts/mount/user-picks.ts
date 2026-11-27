// The user's picks of the board options that keep their value with the user,
// those whose definition has `scope: "user"`, as Board options that stay with
// the user has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.13#board-options-that-stay-with-the-user).
// Core keeps each pick in the browser's local storage, as JSON, so that it
// holds for every board that the user opens in that browser, and across
// reloads, while no board holds it.
// The key is the board option's id under the prefix `blockr.board_option.`, as
// `blockr.board_option.dark_mode`. Every app on the page's origin shares its
// local storage, as the apps on one Connect server do, so the prefix keeps the
// picks apart from what other apps store there.
// The line that R writes into the page's head reads the pick of dark_mode
// under the same key, before the page first paints (R/utils-serve.R).
//
// Core keeps the picks once per page. Where the browser gives no local
// storage, or refuses to keep a pick, as a browser that blocks storage does,
// core keeps the pick for the page alone, so that it holds for every board
// that opens on the page, and logs a warning.
// A pick holds whatever JSON the user picked. The mounts check it against the
// schema of its board option as a board opens, and forget one that the schema
// refuses (./block.ts).

import { log } from "../log/index.ts";
import type { OptionId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import { errorMessage } from "../util/index.ts";

// What core reads and writes of the browser's local storage.
export type LocalStorage = Pick<WindowLocalStorage["localStorage"], "getItem" | "setItem" | "removeItem">;

export interface UserPicks {
  // The user's pick of the board option, or undefined where they picked none.
  get(id: OptionId): Json | undefined;
  // Keeps the user's pick of the board option.
  set(id: OptionId, value: Json): void;
  // Forgets the user's pick of the board option.
  remove(id: OptionId): void;
}

// The key under which the browser's local storage keeps the user's pick of a
// board option, which R's line in the page's head reads too.
export function pickKey(id: OptionId): string {
  return `blockr.board_option.${id}`;
}

// The picks, which the local storage given keeps. Where none is given, as
// where the browser gives none, core keeps each pick for the page alone.
export function createUserPicks(storage: LocalStorage | undefined): UserPicks {
  // The picks that the user made on the page, which hold for the page where
  // the browser did not keep them.
  const made = new Map<OptionId, Json>();

  function stored(id: OptionId): Json | undefined {
    let text: string | null;
    try {
      text = storage?.getItem(pickKey(id)) ?? null;
    } catch {
      return undefined;
    }
    if (text === null) return undefined;
    try {
      return JSON.parse(text) as Json;
    } catch {
      return undefined;
    }
  }

  return {
    get(id) {
      return made.has(id) ? made.get(id) : stored(id);
    },
    set(id, value) {
      made.set(id, value);
      try {
        if (storage === undefined) throw new Error("The browser gives no local storage.");
        storage.setItem(pickKey(id), JSON.stringify(value));
      } catch (error) {
        log.warn(
          `The browser did not keep the pick of the board option "${id}", which holds for this page alone: ${errorMessage(error)}`,
        );
      }
    },
    remove(id) {
      made.delete(id);
      try {
        storage?.removeItem(pickKey(id));
      } catch {
        // Where the browser cannot remove the pick, the mounts refuse it
        // again as the next board opens.
      }
    },
  };
}

// The page's local storage, or undefined where the browser gives none, as
// where reading it throws, which a browser that blocks storage does.
export function pageLocalStorage(): LocalStorage | undefined {
  try {
    return globalThis.localStorage;
  } catch {
    return undefined;
  }
}

// A local storage that holds its items in memory, as the CoreApi for tests
// gives core, so that each starts with no picks.
export function memoryStorage(items: Readonly<Record<string, string>> = {}): LocalStorage {
  const held = new Map(Object.entries(items));
  return {
    getItem: (key) => held.get(key) ?? null,
    setItem: (key, value) => {
      held.set(key, value);
    },
    removeItem: (key) => {
      held.delete(key);
    },
  };
}
