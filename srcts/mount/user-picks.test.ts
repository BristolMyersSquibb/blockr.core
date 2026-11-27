// The user's picks of the board options that keep their value with the user,
// in the browser's local storage, or for the page where the browser keeps
// none.

import { afterEach, describe, expect, test, vi } from "vitest";
import { logged } from "../testing/logged.ts";
import { createUserPicks, memoryStorage, pageLocalStorage, pickKey } from "./index.ts";
import type { LocalStorage } from "./index.ts";

afterEach(() => {
  vi.unstubAllGlobals();
});

// The lines at warn that `fn` logs.
function warnings(fn: () => void): readonly string[] {
  const reported = logged();
  try {
    fn();
  } finally {
    reported.restore();
  }
  return reported.lines().filter((line) => line.level === "warn").map((line) => line.message);
}

describe("the user's picks", () => {
  test("keep each pick as JSON under the board option's id, under blockr's prefix, and forget it again", () => {
    const storage = memoryStorage();
    const picks = createUserPicks(storage);
    expect(picks.get("dark_mode")).toBeUndefined();

    picks.set("dark_mode", "dark");
    expect(storage.getItem("blockr.board_option.dark_mode")).toBe('"dark"');
    expect(pickKey("dark_mode")).toBe("blockr.board_option.dark_mode");
    // A later page reads it from the browser.
    expect(createUserPicks(storage).get("dark_mode")).toBe("dark");

    picks.remove("dark_mode");
    expect(storage.getItem(pickKey("dark_mode"))).toBeNull();
    expect(picks.get("dark_mode")).toBeUndefined();
  });

  test("read a pick that is no JSON as none", () => {
    const picks = createUserPicks(memoryStorage({ [pickKey("dark_mode")]: "dark" }));
    expect(picks.get("dark_mode")).toBeUndefined();
  });

  test("keep a pick for the page alone where the browser refuses it, or gives no local storage, and warn of it", () => {
    const refusing: LocalStorage = {
      getItem: () => null,
      setItem: () => {
        throw new DOMException("The quota has been exceeded.", "QuotaExceededError");
      },
      removeItem: () => undefined,
    };
    const full = createUserPicks(refusing);
    expect(
      warnings(() => {
        full.set("dark_mode", "dark");
      }),
    ).toEqual([
      'The browser did not keep the pick of the board option "dark_mode", which holds for this page alone: The quota has been exceeded.',
    ]);
    expect(full.get("dark_mode")).toBe("dark");

    const none = createUserPicks(undefined);
    expect(
      warnings(() => {
        none.set("dark_mode", "light");
      }),
    ).toEqual([
      'The browser did not keep the pick of the board option "dark_mode", which holds for this page alone: The browser gives no local storage.',
    ]);
    expect(none.get("dark_mode")).toBe("light");
  });

  test("read none where the browser refuses to read its local storage", () => {
    const blocked: LocalStorage = {
      getItem: () => {
        throw new DOMException("Access is denied.", "SecurityError");
      },
      setItem: () => undefined,
      removeItem: () => {
        throw new DOMException("Access is denied.", "SecurityError");
      },
    };
    const picks = createUserPicks(blocked);
    expect(picks.get("dark_mode")).toBeUndefined();
    expect(() => {
      picks.remove("dark_mode");
    }).not.toThrow();
  });
});

describe("the page's local storage", () => {
  test("is the browser's, or none where reading it throws, as a browser that blocks storage has it", () => {
    expect(pageLocalStorage()).toBe(globalThis.localStorage);

    vi.stubGlobal("localStorage", undefined);
    Object.defineProperty(globalThis, "localStorage", {
      configurable: true,
      get() {
        throw new DOMException("Access is denied.", "SecurityError");
      },
    });
    expect(pageLocalStorage()).toBeUndefined();
  });
});
