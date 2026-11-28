// The toasts of core's basic frontend, followed from notifications that the
// test sets, as CoreApi's would hold them. React Aria's queue holds them,
// and its region, which the page tests draw, times them (./index.test.tsx).

import { afterEach, describe, expect, test } from "vitest";
import type { Board, PluginClassId } from "../../types/board.ts";
import type { CoreApi, Notification } from "../../types/core-api.ts";
import { holding } from "../../testing/notifications.ts";
import { interned } from "../../util/index.ts";
import { concerned, createToasts, isReload, SHOWN_FOR, sourcePackage } from "./toasts.ts";
import type { Toasts } from "./toasts.ts";

// The classes of core's storage plugin and of its Edit board plugin.
const STORAGE = interned("blockr.core", "file_storage_plugin");
const EDIT = interned("blockr.core", "edit_board_plugin");

// A notification that a plugin of core's raised, of the severity given.
function raised(id: string, severity: Notification["severity"], about?: Notification["about"]): Notification {
  return {
    id,
    severity,
    message: `${severity} ${id}`,
    source: { kind: "module", module: { object: { plugin: STORAGE }, package: "blockr.core" }, code: "saved" },
    ...(about === undefined ? {} : { about }),
  };
}

// The ids of the notifications whose toasts show, top to bottom, as the
// region lays out the queue, whose latest toast comes first.
const ids = (toasts: Toasts): string[] => toasts.queue.visibleToasts.map((toast) => toast.content.id).reverse();

// Closes the toast of the notification, as the user or its five seconds do.
function close(toasts: Toasts, id: string): void {
  const toast = toasts.queue.visibleToasts.find((shown) => shown.content.id === id);
  if (toast === undefined) throw new Error(`Found no toast of ${id}.`);
  toasts.queue.close(toast.key);
}

// The toasts of the notifications, where the frontend shows the plugins for
// which `sectioned` holds in a section of their own, and no other.
const made: Toasts[] = [];
function toastsFromNotifications(
  notifications: Pick<CoreApi["notifications"], "get" | "subscribe">,
  sectioned: (plugin: PluginClassId) => boolean = () => false,
): Toasts {
  const toasts = createToasts(notifications, sectioned);
  made.push(toasts);
  return toasts;
}

afterEach(() => {
  for (const toasts of made.splice(0)) toasts.stop();
});

describe("the toasts", () => {
  test("show each notification that concerns neither a block nor a plugin with a section, whatever its severity, in the order they came, and leave those to the block's card and the plugin's section", () => {
    const given = holding([raised("a", "error"), raised("b", "message", { block: "b" }), raised("c", "warning", { stack: "s" })]);
    const toasts = toastsFromNotifications(given.notifications, (plugin) => plugin === EDIT);

    expect(ids(toasts)).toEqual(["a", "c"]);
    const [shown] = toasts.queue.visibleToasts;
    given.set([
      ...given.notifications.get(),
      raised("d", "message", { plugin: STORAGE }),
      raised("e", "error", { plugin: EDIT }),
    ]);
    expect(ids(toasts)).toEqual(["a", "c", "d"]);
    expect(toasts.queue.visibleToasts[1]).toBe(shown);
  });

  test("give a message's toast five seconds, and none to a warning's or an error's, which stay until the user closes them", () => {
    const toasts = toastsFromNotifications(holding([raised("m", "message"), raised("w", "warning"), raised("e", "error")]).notifications);

    expect(toasts.queue.visibleToasts.map((toast) => [toast.content.id, toast.timeout])).toEqual([
      ["e", undefined],
      ["w", undefined],
      ["m", SHOWN_FOR],
    ]);
  });

  test("go as soon as core withdraws their notifications, and do not come back while a notification holds once they faded or the user closed them", () => {
    const given = holding([raised("m", "message"), raised("w", "warning"), raised("e", "error")]);
    const toasts = toastsFromNotifications(given.notifications);

    close(toasts, "w");
    close(toasts, "m");
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

  test("stop following the notifications once stopped", () => {
    const given = holding([raised("m", "message")]);
    const toasts = toastsFromNotifications(given.notifications);

    toasts.stop();
    expect(given.listening()).toBe(0);
    given.set([raised("e", "error")]);
    expect(ids(toasts)).toEqual(["m"]);
  });
});

describe("what a toast names", () => {
  const board: Board = {
    id: "board",
    name: "Iris review",
    blocks: {
      a: { registry_id: interned("blockr.core", "dataset_block"), state: {}, name: "Iris", locked: null },
      b: { registry_id: interned("blockr.core", "head_block"), state: {}, name: "Head", locked: null },
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
    plugins: { appPlugins: () => new Map([[EDIT, { name: "Edit board" }]]) },
  };

  test("is the board object that the notification concerns, by the name the page shows for it, by its id where core no longer knows it, or the board", () => {
    expect(
      [
        undefined,
        { option: "n_rows" },
        { option: "zoom" },
        { plugin: EDIT },
        { plugin: interned("blockr.gone", "gone_plugin") },
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
      'Plugin "gone_plugin of blockr.gone"',
      'Stack "Inputs"',
      'Stack "gone"',
      'Link from "Iris" to "Head"',
      'Link "gone"',
      'Block "Head"',
    ]);
  });

  test("beside it, is the package that the notification's source names: the package of the module that made the request, failed or raised it", () => {
    // The plugin raised one about itself, one about the board, and one about
    // another plugin.
    const own = raised("own", "error", { plugin: STORAGE });
    const theBoard = raised("board", "error");
    const other = raised("other", "error", { plugin: interned("blockr.test", "session_plugin") });
    const dag = interned("blockr.dag", "dag_plugin");
    const failed: Notification = {
      id: "failed",
      severity: "error",
      message: "No module.",
      source: { kind: "core", code: "module_failed", module: { object: { plugin: dag }, package: "blockr.dag" } },
      about: { plugin: dag },
    };
    const fromR: Notification = {
      id: "r",
      severity: "error",
      message: "No data.",
      source: {
        kind: "r",
        class: ["simpleError", "error", "condition"],
        request: { type: "plugin", name: "list" },
        module: { object: { plugin: dag }, package: "blockr.dag" },
      },
      about: { plugin: dag },
    };
    // R's notification of an evaluation, which no module asked for.
    const evaluated: Notification = {
      id: "e",
      severity: "error",
      message: "No rows.",
      source: { kind: "r", class: ["simpleError", "error", "condition"], request: { type: "evaluate", phase: "block_eval" } },
    };

    expect([own, theBoard, other, failed, fromR, evaluated].map(sourcePackage)).toEqual([
      "blockr.core",
      "blockr.core",
      "blockr.core",
      "blockr.dag",
      "blockr.dag",
      undefined,
    ]);
  });

  test("offers to reload the page for core's page_reload_needed alone, whatever a module's code", () => {
    const reload: Notification = { id: "r", severity: "warning", message: "Reload.", source: { kind: "core", code: "page_reload_needed" } };
    const posing: Notification = {
      ...reload,
      id: "p",
      source: { kind: "module", module: { object: { plugin: STORAGE }, package: "blockr.core" }, code: "page_reload_needed" },
    };
    expect([reload, posing].map(isReload)).toEqual([true, false]);
  });
});
