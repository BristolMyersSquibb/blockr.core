// The toasts of core's basic frontend, followed from notifications that the
// test sets, as CoreApi's would hold them, with the browser's timers faked.

import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import type { Board } from "../../types/board.ts";
import type { CoreApi, Notification } from "../../types/core-api.ts";
import { concerned, createToasts, FADES_FOR, isReload, packageShown, SHOWN_FOR } from "./toasts.ts";
import type { Toasts } from "./toasts.ts";

// Notifications that hold what the test sets, the same array until it sets
// another, as CoreApi's are.
function holding(initial: readonly Notification[] = []) {
  let all = initial;
  const listeners = new Set<(all: readonly Notification[]) => void>();
  return {
    notifications: {
      get: () => all,
      subscribe(fn: (all: readonly Notification[]) => void) {
        listeners.add(fn);
        return () => {
          listeners.delete(fn);
        };
      },
    } satisfies Pick<CoreApi["notifications"], "get" | "subscribe">,
    set(next: readonly Notification[]): void {
      all = next;
      for (const fn of [...listeners]) fn(all);
    },
    listening: () => listeners.size,
  };
}

// A notification that a plugin of core's raised, of the severity given.
function raised(id: string, severity: Notification["severity"], about?: Notification["about"]): Notification {
  return {
    id,
    severity,
    message: `${severity} ${id}`,
    source: { kind: "module", object: { plugin: "blockr.core-file_storage_plugin" }, code: "saved", package: "blockr.core" },
    ...(about === undefined ? {} : { about }),
  };
}

// The ids of the notifications whose toasts show, each marked where it fades.
const ids = (toasts: Toasts): string[] => toasts.list().map((toast) => `${toast.notification.id}${toast.fading ? " fading" : ""}`);

const made: Toasts[] = [];
function toastsOf(notifications: Pick<CoreApi["notifications"], "get" | "subscribe">): Toasts {
  const toasts = createToasts(notifications);
  made.push(toasts);
  return toasts;
}

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  for (const toasts of made.splice(0)) toasts.stop();
  vi.useRealTimers();
});

describe("the toasts", () => {
  test("show each notification that concerns no block, whatever its severity, in the order they came, and leave those about a block to its card", () => {
    const given = holding([raised("a", "error"), raised("b", "message", { block: "b" }), raised("c", "warning", { stack: "s" })]);
    const toasts = toastsOf(given.notifications);

    expect(ids(toasts)).toEqual(["a", "c"]);
    const shown = toasts.list();
    given.set([...given.notifications.get(), raised("d", "message", { plugin: "blockr.core-edit_board_plugin" })]);
    expect(ids(toasts)).toEqual(["a", "c", "d"]);
    expect(toasts.list()[0]).toBe(shown[0]);
  });

  test("call their subscribers as the notifications change, and keep the same list until a toast comes, goes or starts to fade", () => {
    const given = holding([raised("a", "error")]);
    const toasts = toastsOf(given.notifications);
    const heard = vi.fn();
    const off = toasts.subscribe(heard);
    const before = toasts.list();

    given.set([...given.notifications.get(), raised("b", "error", { block: "b" })]);
    expect(toasts.list()).toBe(before);
    given.set([raised("a", "error"), raised("c", "message")]);
    expect(toasts.list()).not.toBe(before);
    expect(heard).toHaveBeenCalledTimes(2);
    off();
    given.set([]);
    expect(heard).toHaveBeenCalledTimes(2);
  });

  test("fade for a message after five seconds and then go, while a warning's and an error's stay until the user closes them", () => {
    const given = holding([raised("m", "message"), raised("w", "warning"), raised("e", "error")]);
    const toasts = toastsOf(given.notifications);

    vi.advanceTimersByTime(SHOWN_FOR - 1);
    expect(ids(toasts)).toEqual(["m", "w", "e"]);
    vi.advanceTimersByTime(1);
    expect(ids(toasts)).toEqual(["m fading", "w", "e"]);
    vi.advanceTimersByTime(FADES_FOR);
    expect(ids(toasts)).toEqual(["w", "e"]);

    vi.advanceTimersByTime(60_000);
    expect(ids(toasts)).toEqual(["w", "e"]);
    toasts.close("e");
    expect(ids(toasts)).toEqual(["w"]);
  });

  test("keep a message's toast while the pointer rests on it or the focus is in it, and start its five seconds again once neither does", () => {
    const given = holding([raised("m", "message")]);
    const toasts = toastsOf(given.notifications);

    vi.advanceTimersByTime(SHOWN_FOR - 1000);
    toasts.keep("m", "pointer");
    toasts.keep("m", "focus");
    vi.advanceTimersByTime(60_000);
    expect(ids(toasts)).toEqual(["m"]);
    toasts.release("m", "pointer");
    vi.advanceTimersByTime(60_000);
    expect(ids(toasts)).toEqual(["m"]);

    toasts.release("m", "focus");
    vi.advanceTimersByTime(SHOWN_FOR - 1);
    expect(ids(toasts)).toEqual(["m"]);
    vi.advanceTimersByTime(1);
    expect(ids(toasts)).toEqual(["m fading"]);

    // The pointer that comes to rest on a fading toast keeps it.
    toasts.keep("m", "pointer");
    expect(ids(toasts)).toEqual(["m"]);
    vi.advanceTimersByTime(60_000);
    expect(ids(toasts)).toEqual(["m"]);
    toasts.release("m", "pointer");
    vi.advanceTimersByTime(SHOWN_FOR + FADES_FOR);
    expect(ids(toasts)).toEqual([]);
  });

  test("go as soon as core withdraws their notifications, and do not come back while a notification holds once they faded or the user closed them", () => {
    const given = holding([raised("m", "message"), raised("w", "warning"), raised("e", "error")]);
    const toasts = toastsOf(given.notifications);

    toasts.close("w");
    vi.advanceTimersByTime(SHOWN_FOR + FADES_FOR);
    expect(ids(toasts)).toEqual(["e"]);
    // Core holds the same notifications in a new array.
    given.set([...given.notifications.get()]);
    expect(ids(toasts)).toEqual(["e"]);

    given.set([raised("w", "warning")]);
    expect(ids(toasts)).toEqual([]);
    // A notification that core raises again once it withdrew it, with the
    // same id, as one of core's own, shows again.
    given.set([raised("m", "message"), raised("e", "error")]);
    expect(ids(toasts)).toEqual(["m", "e"]);
  });

  test("show inside the element of the dialog that opened last, and back in the page once every dialog closes", () => {
    const toasts = toastsOf(holding([raised("e", "error")]).notifications);
    const heard = vi.fn();
    toasts.subscribe(heard);
    const first = document.createElement("div");
    const second = document.createElement("div");

    expect(toasts.shownIn()).toBeNull();
    const closeFirst = toasts.showIn(first);
    const closeSecond = toasts.showIn(second);
    expect(toasts.shownIn()).toBe(second);
    closeSecond();
    closeSecond();
    expect(toasts.shownIn()).toBe(first);
    closeFirst();
    expect(toasts.shownIn()).toBeNull();
    expect(heard).toHaveBeenCalledTimes(4);
  });

  test("stop following the notifications, and stop their timers, once stopped", () => {
    const given = holding([raised("m", "message")]);
    const toasts = toastsOf(given.notifications);
    toasts.keep("m", "pointer");

    toasts.stop();
    expect(given.listening()).toBe(0);
    toasts.release("m", "pointer");
    vi.advanceTimersByTime(60_000);
    expect(ids(toasts)).toEqual(["m"]);
    expect(vi.getTimerCount()).toBe(0);
  });
});

describe("what a toast names", () => {
  const board: Board = {
    id: "board",
    name: "Iris review",
    blocks: {
      a: { registry_id: "blockr.core-dataset_block", state: {}, name: "Iris", locked: null },
      b: { registry_id: "blockr.core-head_block", state: {}, name: "Head", locked: null },
    },
    links: { ab: { from: "a", to: "b", input: "data" } },
    stacks: { s: { name: "Inputs", blocks: ["a"], color: null } },
    options: {},
    plugins: {},
  };
  const core = {
    options: {
      registered: () =>
        ({ n_rows: { label: "Rows shown" } }) as unknown as ReturnType<CoreApi["options"]["registered"]>,
    },
    plugins: { appPlugins: () => ({ "blockr.core-edit_board_plugin": { name: "Edit board" } }) },
  };

  test("is the board object that the notification concerns, by the name the page shows for it, by its id where core no longer knows it, or the board", () => {
    expect(
      [
        undefined,
        { option: "n_rows" },
        { option: "zoom" },
        { plugin: "blockr.core-edit_board_plugin" },
        { plugin: "blockr.gone-gone_plugin" },
        { stack: "s" },
        { stack: "gone" },
        { link: "ab" },
        { link: "gone" },
        { block: "b" },
      ].map((about) => concerned(about, core, board)),
    ).toEqual([
      "Board",
      'Board option "Rows shown"',
      'Board option "zoom"',
      'Plugin "Edit board"',
      'Plugin "blockr.gone-gone_plugin"',
      'Stack "Inputs"',
      'Stack "gone"',
      'Link from "Iris" to "Head"',
      'Link "gone"',
      'Block "Head"',
    ]);
  });

  test("beside it, is the package whose module failed or raised the notification, but none where a module raised it about another board object than its own", () => {
    // The plugin raised one about itself, one about the board, and one about
    // another plugin.
    const own = raised("own", "error", { plugin: "blockr.core-file_storage_plugin" });
    const theBoard = raised("board", "error");
    const other = raised("other", "error", { plugin: "blockr.test-session_plugin" });
    const failed: Notification = {
      id: "failed",
      severity: "error",
      message: "No module.",
      source: { kind: "core", code: "module_failed", package: "blockr.dag" },
      about: { plugin: "blockr.dag-dag_plugin" },
    };
    const fromR: Notification = {
      id: "r",
      severity: "error",
      message: "No data.",
      source: { kind: "r", class: ["simpleError", "error", "condition"], request: { type: "plugin", name: "list" } },
      about: { plugin: "blockr.dag-dag_plugin" },
    };

    expect([own, theBoard, other, failed, fromR].map(packageShown)).toEqual(["blockr.core", "blockr.core", undefined, "blockr.dag", undefined]);
  });

  test("offers to reload the page for core's page_reload_needed alone, whatever a module's code", () => {
    const reload: Notification = { id: "r", severity: "warning", message: "Reload.", source: { kind: "core", code: "page_reload_needed" } };
    const posing: Notification = {
      ...reload,
      id: "p",
      source: { kind: "module", object: { plugin: "blockr.core-file_storage_plugin" }, code: "page_reload_needed" },
    };
    expect([reload, posing].map(isReload)).toEqual([true, false]);
  });
});
