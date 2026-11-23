// The advanced actions of board actions on the CoreApi for tests, with a board
// action of the tests' own: opening one from a menu, from trigger() and from
// the open icon of a quick action, what its done() runs, a batch that the
// store refuses, the place that shows it, core's dialog, a caller's place or
// the frontend's, ending one as its target leaves the board or the next board
// opens, and mounting one in a plugin's room.

import { describe, expect, test } from "vitest";
import type { HelloReply } from "../protocol/index.ts";
import type { Delta, PluginClass } from "../store/index.ts";
import { HELLO, stubCore } from "../stubs/index.ts";
import type { StubCoreOptions } from "../stubs/index.ts";
import { logged } from "../testing/logged.ts";
import { held, notificationCode } from "../testing/notifications.ts";
import { classModule, listedModules } from "../testing/plugins.ts";
import { withPlugins } from "../testing/store.ts";
import type { Notification } from "../types/core-api.ts";
import type { BoardAction, CommandCall, Target } from "./actions.ts";
import type { AdvancedModule, PlaceArgs } from "./advanced.ts";

// The element that the user triggers from, which a trigger() needs.
const el = document.body;

type Args = Parameters<AdvancedModule["render"]>[0];

// An advanced action that writes its target into its element, keeps what each
// render() gets, and counts its cleanups.
function form() {
  const renders: Args[] = [];
  let cleanups = 0;
  const module: AdvancedModule = {
    render(args) {
      renders.push(args);
      args.el.textContent = `The form of ${JSON.stringify(args.target)}`;
      return () => {
        cleanups++;
      };
    },
  };
  return {
    module,
    renders,
    cleanups: () => cleanups,
    // What the latest render() got.
    last(): Args {
      const args = renders.at(-1);
      if (args === undefined) throw new Error("The advanced action has not rendered.");
      return args;
    },
  };
}

// A board action of blockr.test on the kinds of target given, labelled by its
// name, with the advanced action given and no quick action unless `more`
// gives one.
function asking(name: string, on: BoardAction["on"], advanced: AdvancedModule, more: Partial<BoardAction> = {}): BoardAction {
  return { id: `blockr.test-${name.toLowerCase().replaceAll(" ", "_")}_action`, label: name, on, category: "test", advanced, ...more };
}

// A plugin class of blockr.test, which lists the board actions given.
function listing(boardActions: readonly BoardAction[], more: Partial<PluginClass> = {}): PluginClass {
  return { id: "blockr.test-forms_plugin", boardActions, ...more };
}

// The CoreApi for tests on the board of the fixture hello, whose app runs core's
// plugins and one of each class given, after core's.
function stubbed(classes: readonly PluginClass[], options: StubCoreOptions = {}, replied: HelloReply = HELLO) {
  return stubCore({
    hello: withPlugins(Object.fromEntries(classes.map(({ id }) => [id, { name: id }])), replied),
    ...options,
    modules: { ...listedModules(classes), ...options.modules },
  });
}

const rename = (name: string): CommandCall[] => [{ id: "blockr.core-rename_block", args: { id: "b", name } }];
const refused: CommandCall[] = [{ id: "blockr.core-remove_link", args: { id: "nowhere" } }];
const REFUSAL = 'The batch was refused at command 1, "blockr.core-remove_link"';

// A place of the test's, which records what core hands it and each time core
// calls what it returned, with the deltas, and mounts the advanced action
// where the test asks.
function place() {
  const shown: PlaceArgs[] = [];
  const ended: (readonly Delta[] | undefined)[][] = [];
  return {
    shown,
    ended,
    advanced: (args: PlaceArgs) => {
      shown.push(args);
      return (...deltas: (readonly Delta[] | undefined)[]) => {
        ended.push(deltas);
      };
    },
    // The args of the latest advanced action that core handed the place.
    last(): PlaceArgs {
      const args = shown.at(-1);
      if (args === undefined) throw new Error("Core handed the place nothing.");
      return args;
    },
  };
}

// The ids of the blocks that the deltas add.
function added(deltas: readonly Delta[] | undefined): string[] {
  return (deltas ?? []).flatMap((delta) => Object.keys(delta.blocks?.add ?? {}));
}

describe("opening an advanced action", () => {
  test("opens it in core's dialog as the user picks the row of a board action with an advanced action alone, titled with its label, and renders it with CoreApi, the target and a signal", async () => {
    const shown = form();
    const edit = asking("Edit form", ["block"], shown.module);
    const s = await stubbed([listing([edit])]);

    const picked = s.menu({ block: "b" }).pick("Edit form");

    expect(s.menu()).toBeUndefined();
    const dialog = s.advanced();
    expect(dialog).toMatchObject({ id: edit.id, title: "Edit form", target: { block: "b" } });
    expect(dialog?.el.textContent).toBe('The form of {"block":"b"}');
    const args = shown.last();
    expect(args.core).toBe(s.core);
    expect(args.target).toEqual({ block: "b" });
    expect(args).not.toHaveProperty("picked");
    expect(args).not.toHaveProperty("filter");
    expect(args.signal.aborted).toBe(false);

    // Closing the dialog, as Escape or its Close button does, ends it, and
    // the pick resolves.
    dialog?.close();
    await picked;
    expect(s.advanced()).toBeUndefined();
    expect(dialog?.closed()).toBe(true);
    expect(dialog?.el.isConnected).toBe(false);
    expect(args.signal.aborted).toBe(true);
    expect(shown.cleanups()).toBe(1);
    expect(s.core.store.canUndo()).toBe(false);
    s.stop();
  });

  test("opens it from trigger() on a board action with an advanced action alone, and from a quick action that gives no level for the target, where trigger() opens no menu", async () => {
    const shown = form();
    const link = asking("Create link", ["block", "board"], shown.module, {
      quick: (_board, target) =>
        "board" in target ? undefined : { rows: [{ kind: "item", label: "Iris", pick: () => [{ id: "blockr.core-rename_block", args: { id: "b", name: "Linked" } }] }] },
    });
    const s = await stubbed([listing([link])]);

    const onBoard = s.core.boardActions.trigger(link.id, { board: true }, { el });
    expect(s.menu()).toBeUndefined();
    expect(s.advanced()?.target).toEqual({ board: true });
    s.advanced()?.close();
    await onBoard;

    // On a block, the quick action gives a level, which shows as a menu.
    const onBlock = s.core.boardActions.trigger(link.id, { block: "b" }, { el });
    expect(s.menu()?.level()).toMatchObject({ advanced: true, rows: [{ label: "Iris" }] });
    expect(s.advanced()).toBeUndefined();
    await s.menu()?.pick("Iris");
    await onBlock;
    expect(s.core.store.board().blocks.b?.name).toBe("Linked");
    s.stop();
  });

  test("opens it from the open icon of a level with the level's picked and the filter's text, and of an item with the item's advanced", async () => {
    const shown = form();
    const add = asking("Append test block", ["block"], shown.module, {
      quick: () => ({
        filter: true,
        picked: { input: "data" },
        rows: [
          { kind: "item", label: "Head", pick: () => rename("Head"), advanced: { class: "head" } },
          { kind: "item", label: "Sort", pick: () => rename("Sort") },
        ],
      }),
    });
    const s = await stubbed([listing([add])]);

    const menu = s.menu({ block: "b" });
    await menu.pick("Append test block");
    menu.type("Hea");
    const fromLevel = menu.openAdvanced();
    expect(menu.closed()).toBe(true);
    expect(shown.last()).toMatchObject({ picked: { input: "data" }, filter: "Hea", target: { block: "b" } });
    s.advanced()?.close();
    await fromLevel;

    const triggered = s.core.boardActions.trigger(add.id, { block: "b" }, { el });
    const opened = s.menu();
    expect(() => opened?.openAdvanced("Sort")).toThrow('The item "Sort" shows no open icon.');
    void opened?.openAdvanced("Head");
    expect(shown.last()).toMatchObject({ picked: { class: "head" }, filter: "" });
    s.advanced()?.close();
    await triggered;
    expect(s.core.store.board().blocks.b?.name).toBe("Head");
    s.stop();
  });

  test("shows one advanced action at a time in core's dialog, so that opening another ends the one before", async () => {
    const shown = form();
    const edit = asking("Edit form", ["block"], shown.module);
    const s = await stubbed([listing([edit])]);

    const first = s.core.boardActions.trigger(edit.id, { block: "b" }, { el });
    const dialog = s.advanced();
    const second = s.core.boardActions.trigger(edit.id, { block: "c" }, { el });

    await first;
    expect(dialog?.closed()).toBe(true);
    expect(shown.renders[0]?.signal.aborted).toBe(true);
    expect(s.advanced()?.target).toEqual({ block: "c" });
    s.advanced()?.close();
    await second;
    s.stop();
  });

  test("raises module_failed about the plugin where the advanced action throws in render(), which core logs, and leaves the element empty", async () => {
    const edit = asking("Edit form", ["block"], {
      render: () => {
        throw new Error("No form.");
      },
    });
    const reported = logged();
    const s = await stubbed([listing([edit])]);

    const triggered = s.core.boardActions.trigger(edit.id, { block: "b" }, { el });
    await s.settled();
    reported.restore();

    const why = `The advanced action of the board action "${edit.id}" failed in render(): No form.`;
    expect(reported.errors().map((line) => line.message)).toEqual([why]);
    expect(held(s.core, (n) => notificationCode(n) === "module_failed")).toEqual([
      {
        severity: "error",
        message: why,
        source: { kind: "core", code: "module_failed", module: { object: { plugin: "blockr.test-forms_plugin" }, package: "blockr.test" } },
        about: { plugin: "blockr.test-forms_plugin" },
      },
    ]);
    expect(s.advanced()?.el.childNodes).toHaveLength(0);
    s.advanced()?.close();
    await triggered;
    s.stop();
  });
});

describe("done()", () => {
  test("runs the batch as one step of the user, with the commands that the caller's extend returns for the deltas that core planned, closes, and resolves trigger() once the advanced action has ended", async () => {
    const shown = form();
    const edit = asking("Edit form", ["block"], shown.module);
    const s = await stubbed([listing([edit])]);
    const board = s.core.store.board();
    const seen: string[][] = [];
    let ended = false;

    const triggered = s.core.boardActions
      .trigger(edit.id, { block: "b" }, {
        el,
        extend(deltas) {
          seen.push(added(deltas));
          return rename("Placed").map((call) => ({ ...call, args: { id: "n", name: "Placed" } }));
        },
      })
      .then(() => {
        ended = true;
      });
    const args = shown.last();
    await args.done([{ id: "blockr.core-add_block", args: { registry_id: "blockr.core-head_block", id: "n" } }]);

    expect(seen).toEqual([["n"]]);
    expect(s.core.store.board().blocks.n?.name).toBe("Placed");
    expect(s.advanced()).toBeUndefined();
    expect(args.signal.aborted).toBe(true);
    await triggered;
    expect(ended).toBe(true);
    s.core.store.undo();
    expect(s.core.store.board()).toEqual(board);
    expect(s.core.store.canUndo()).toBe(false);
    s.stop();
  });

  test("only closes without a batch, or with an empty one, and runs nothing", async () => {
    const shown = form();
    const edit = asking("Edit form", ["block"], shown.module);
    const s = await stubbed([listing([edit])]);
    const board = s.core.store.board();

    for (const batch of [undefined, []]) {
      const triggered = s.core.boardActions.trigger(edit.id, { block: "b" }, { el });
      await shown.last().done(batch);
      await triggered;
      expect(s.advanced()).toBeUndefined();
    }

    expect(s.core.store.board()).toBe(board);
    expect(s.core.store.canUndo()).toBe(false);
    s.stop();
  });

  test("rejects with the refusal where the store refuses the batch, and the advanced action stays open, with its notifier taking the notifications, or else the caller's, or else core raising them", async () => {
    const shown = form();
    const edit = asking("Edit form", ["block"], shown.module);
    const s = await stubbed([listing([edit])]);
    const refusals = () => held(s.core, (n) => notificationCode(n) === "board_edit_failed");
    const own: (readonly Notification[])[] = [];
    const caller: [string[], string][] = [];

    const triggered = s.core.boardActions.trigger(edit.id, { block: "b" }, {
      el,
      notifier(notifications, action) {
        caller.push([notifications.map(notificationCode), action.id]);
      },
    });
    const args = shown.last();
    await expect(
      args.done(refused, {
        notifier(notifications) {
          own.push(notifications);
        },
      }),
    ).rejects.toThrow(REFUSAL);
    expect(own.map((notifications) => notifications.map(notificationCode))).toEqual([["board_edit_failed"]]);
    expect(caller).toEqual([]);
    expect(args.signal.aborted).toBe(false);
    expect(s.advanced()?.closed()).toBe(false);

    // Without a notifier of its own, the caller's takes them, with the board
    // action.
    await expect(args.done(refused)).rejects.toThrow(REFUSAL);
    expect(caller).toEqual([[["board_edit_failed"], edit.id]]);
    expect(refusals()).toEqual([]);
    s.advanced()?.close();
    await triggered;

    // Without either, core raises them.
    const again = s.core.boardActions.trigger(edit.id, { block: "b" }, { el });
    await expect(shown.last().done(refused)).rejects.toThrow(REFUSAL);
    expect(refusals()).toHaveLength(1);
    expect(s.advanced()?.closed()).toBe(false);
    s.advanced()?.close();
    await again;
    s.stop();
  });

  test("rejects what is no batch, a second done() while one runs, one once the advanced action has ended, and a notifier that is no function", async () => {
    const shown = form();
    const edit = asking("Edit form", ["block"], shown.module);
    const s = await stubbed([listing([edit])]);

    const triggered = s.core.boardActions.trigger(edit.id, { block: "b" }, { el });
    const args = shown.last();
    const done = args.done as (...given: unknown[]) => Promise<void>;
    await expect(done("rename")).rejects.toThrow(
      new TypeError(
        `The done() of the advanced action of the board action "${edit.id}" takes a batch of commands, as [{ id: "blockr.core-remove_block", args: { id: "a" } }], or none.`,
      ),
    );
    await expect(done(rename("First"), { notifier: "toast" })).rejects.toThrow(new TypeError("The `notifier` of done() is a function."));

    const first = args.done(rename("First"));
    await expect(args.done(rename("Second"))).rejects.toThrow(`The advanced action of the board action "${edit.id}" runs a done() already.`);
    await first;
    await triggered;
    await expect(args.done(rename("Third"))).rejects.toThrow(
      `The advanced action of the board action "${edit.id}" has ended, so its done() runs nothing.`,
    );
    expect(s.core.store.board().blocks.b?.name).toBe("First");
    s.stop();
  });
});

describe("the place of an advanced action", () => {
  test("is the caller's advanced, in place of the frontend's, which core calls with the board action's id, its label, the target, a mount() and a close()", async () => {
    const shown = form();
    const edit = asking("Edit form", ["block"], shown.module);
    const s = await stubbed([listing([edit])]);
    const room = place();

    const picked = s.menu({ block: "b" }, { advanced: room.advanced }).pick("Edit form");

    expect(s.advanced()).toBeUndefined();
    const args = room.last();
    expect(args).toMatchObject({ id: edit.id, title: "Edit form", target: { block: "b" } });
    expect(shown.renders).toEqual([]);
    const at = document.createElement("div");
    const unmount = args.mount(at);
    expect(at.textContent).toBe('The form of {"block":"b"}');

    // The cleanup that mount() returns unmounts it and empties the element,
    // and a mount afresh renders it again.
    unmount();
    expect(shown.cleanups()).toBe(1);
    expect(at.childNodes).toHaveLength(0);
    args.mount(at);
    expect(shown.renders).toHaveLength(2);

    // The place's close() ends it, and the pick resolves.
    args.close();
    await picked;
    expect(shown.last().signal.aborted).toBe(true);
    expect(room.ended).toEqual([]);
    s.stop();
  });

  test("hears the deltas of the batch that done() ran once done() ends the advanced action, which core planned once with extend, and none for a done() without a batch", async () => {
    const shown = form();
    const add = asking("Add form", ["board"], shown.module);
    const s = await stubbed([listing([add])]);
    const room = place();
    const extended: string[][] = [];

    const triggered = s.core.boardActions.trigger(add.id, { board: true }, {
      el,
      advanced: room.advanced,
      extend(deltas) {
        extended.push(added(deltas));
        return [];
      },
    });
    room.last().mount(document.createElement("div"));
    await shown.last().done([{ id: "blockr.core-add_block", args: { registry_id: "blockr.core-head_block", id: "n" } }]);
    await triggered;

    expect(extended).toEqual([["n"]]);
    expect(room.ended.map((deltas) => deltas.map(added))).toEqual([[["n"]]]);
    expect(shown.last().signal.aborted).toBe(true);

    // A place that the user pinned mounts the advanced action again, which
    // gets neither the caller's extend nor what the open icon handed it. It
    // ends again with its next done().
    room.last().mount(document.createElement("div"));
    await shown.last().done([{ id: "blockr.core-add_block", args: { registry_id: "blockr.core-head_block", id: "m" } }]);
    expect(extended).toHaveLength(1);
    expect(room.ended.map((deltas) => deltas.map(added))).toEqual([[["n"]], [["m"]]]);

    room.last().mount(document.createElement("div"));
    await shown.last().done();
    expect(room.ended.at(-1)).toEqual([]);
    s.stop();
  });

  test("renders on another target that mount() is given where a menu of that target offers the board action, and renders nothing otherwise", async () => {
    const shown = form();
    const edit = asking("Edit form", ["block"], shown.module, { offers: (_board, target) => !("block" in target) || target.block !== "c" });
    const s = await stubbed([listing([edit])]);
    const room = place();

    void s.core.boardActions.trigger(edit.id, { block: "b" }, { el, advanced: room.advanced });
    const args = room.last();
    const at = document.createElement("div");
    args.mount(at, { block: "d" });
    expect(at.textContent).toBe('The form of {"block":"d"}');
    expect(shown.last().target).toEqual({ block: "d" });

    const elsewhere = document.createElement("div");
    args.mount(elsewhere, { block: "c" });
    args.mount(elsewhere, { link: "ab" });
    args.mount(elsewhere, { block: "nowhere" });
    expect(shown.renders).toHaveLength(1);
    expect(elsewhere.childNodes).toHaveLength(0);
    expect(() => args.mount(elsewhere, { blok: "c" } as unknown as Target)).toThrow(TypeError);
    expect(() => args.mount("sidebar" as unknown as HTMLElement)).toThrow(
      new TypeError("The `el` of mount() is the element in which core renders the advanced action."),
    );
    args.close();
    s.stop();
  });

  test("runs a done() on another target that mount() is given without what the user triggered, the caller's extend and notifier and what the open icon handed it, and ends the advanced action", async () => {
    const shown = form();
    const append = asking("Append form", ["block"], shown.module, {
      quick: () => ({ filter: true, picked: { input: "data" }, rows: [{ kind: "item", label: "Head", pick: () => rename("Head") }] }),
    });
    const s = await stubbed([listing([append])]);
    const room = place();
    const extended: string[][] = [];
    const caller: string[][] = [];
    const refusals = () => held(s.core, (n) => notificationCode(n) === "board_edit_failed");

    const menu = s.menu(
      { block: "b" },
      {
        advanced: room.advanced,
        extend(deltas) {
          extended.push(added(deltas));
          return [];
        },
        notifier(notifications) {
          caller.push(notifications.map(notificationCode));
        },
      },
    );
    await menu.pick("Append form");
    menu.type("Hea");
    const opened = menu.openAdvanced();

    // A pinned place that follows the selection moves on to another block
    // before the user submits.
    room.last().mount(document.createElement("div"), { block: "d" });
    const args = shown.last();
    expect(args.target).toEqual({ block: "d" });
    expect(args).not.toHaveProperty("picked");
    expect(args).not.toHaveProperty("filter");

    await expect(args.done(refused)).rejects.toThrow(REFUSAL);
    expect(caller).toEqual([]);
    expect(refusals()).toHaveLength(1);

    await args.done([{ id: "blockr.core-add_block", args: { registry_id: "blockr.core-head_block", id: "n" } }]);
    await opened;
    expect(extended).toEqual([]);
    expect(room.ended.map((deltas) => deltas.map(added))).toEqual([[["n"]]]);
    s.stop();
  });

  test("is the frontend's where the caller gives none, which core calls as a method of the frontend's module, and core's dialog where the frontend gives none", async () => {
    const shown = form();
    const edit = asking("Edit form", ["block"], shown.module);
    const room = place();
    const frontend = {
      render: () => undefined,
      advanced(this: unknown, args: PlaceArgs) {
        expect(this).toBe(frontend);
        return room.advanced(args);
      },
    };
    const s = await stubbed([listing([edit])], {
      el: document.createElement("div"),
      modules: { "blockr.core-basic_frontend_plugin": classModule({ frontend }) },
    });

    const triggered = s.core.boardActions.trigger(edit.id, { block: "b" }, { el });
    expect(s.advanced()).toBeUndefined();
    expect(room.last()).toMatchObject({ id: edit.id, title: "Edit form", target: { block: "b" } });
    room.last().close();
    await triggered;
    s.stop();

    // A frontend that gives no place, as core's basic frontend gives none,
    // leaves the advanced action to core's dialog.
    const none = await stubbed([listing([edit])], {
      el: document.createElement("div"),
      modules: { "blockr.core-basic_frontend_plugin": classModule({ frontend: { render: () => undefined } }) },
    });
    void none.core.boardActions.trigger(edit.id, { block: "b" }, { el });
    expect(none.advanced()?.id).toBe(edit.id);
    none.stop();
  });

  test("falls back to core's dialog where the place throws, which core logs", async () => {
    const shown = form();
    const edit = asking("Edit form", ["block"], shown.module);
    const s = await stubbed([listing([edit])]);
    const reported = logged();

    const triggered = s.core.boardActions.trigger(edit.id, { block: "b" }, {
      el,
      advanced: () => {
        throw new Error("No sidebar.");
      },
    });
    reported.restore();

    expect(reported.errors().map((line) => line.message)).toEqual([
      `The place that was to show the advanced action of the board action "${edit.id}" failed, so core's dialog shows it: No sidebar.`,
    ]);
    expect(s.advanced()?.el.textContent).toBe('The form of {"block":"b"}');
    s.advanced()?.close();
    await triggered;
    s.stop();
  });
});

describe("ending an advanced action", () => {
  test("ends it as a done() without a batch does where a board object that its target names leaves the board", async () => {
    const shown = form();
    const edit = asking("Edit form", ["block", "selection"], shown.module);
    const s = await stubbed([listing([edit])]);
    const room = place();

    const inDialog = s.core.boardActions.trigger(edit.id, { block: "b" }, { el });
    await s.core.commands.run("blockr.core-remove_block", { id: "b" });
    await inDialog;
    expect(s.advanced()).toBeUndefined();
    expect(shown.last().signal.aborted).toBe(true);

    // In a caller's place, core calls what the place returned, with no deltas.
    const inPlace = s.core.boardActions.trigger(edit.id, { selection: [{ block: "c" }, { link: "ad" }] }, { el, advanced: room.advanced });
    room.last().mount(document.createElement("div"));
    await s.core.commands.run("blockr.core-remove_link", { id: "ad" });
    await inPlace;
    expect(room.ended).toEqual([[]]);
    expect(shown.last().signal.aborted).toBe(true);
    s.stop();
  });

  test("leaves it open where a done() runs a batch that removes its target, and tells the place the batch's deltas", async () => {
    const shown = form();
    const edit = asking("Edit form", ["block"], shown.module);
    const s = await stubbed([listing([edit])]);
    const room = place();

    const triggered = s.core.boardActions.trigger(edit.id, { block: "f" }, { el, advanced: room.advanced });
    room.last().mount(document.createElement("div"));
    await shown.last().done([{ id: "blockr.core-remove_block", args: { id: "f" } }]);
    await triggered;

    expect(room.ended).toHaveLength(1);
    expect(room.ended[0]?.[0]).toHaveLength(1);
    s.stop();
  });

  test("ends every advanced action as the next board opens, and core's dialog closes", async () => {
    const shown = form();
    const edit = asking("Edit form", ["block"], shown.module);
    const s = await stubbed([listing([edit])]);

    const triggered = s.core.boardActions.trigger(edit.id, { block: "b" }, { el });
    const dialog = s.advanced();
    const args = shown.last();
    await s.core.store.open(JSON.stringify(HELLO.board));
    await s.settled();
    await triggered;

    expect(dialog?.closed()).toBe(true);
    expect(args.signal.aborted).toBe(true);
    s.stop();
  });
});

describe("mountAdvanced()", () => {
  test("renders the advanced action in the plugin's room with picked, runs the batch of each done() as one step and renders it again, and renders it again for a done() without a batch", async () => {
    const shown = form();
    const add = asking("Add form", ["board"], shown.module);
    const s = await stubbed([listing([add])]);
    const room = document.createElement("div");

    const unmount = s.core.boardActions.mountAdvanced(room, add.id, { board: true }, { class: "head" });
    expect(room.textContent).toBe('The form of {"board":true}');
    expect(shown.last()).toMatchObject({ picked: { class: "head" }, target: { board: true } });
    expect(shown.last()).not.toHaveProperty("filter");

    const first = shown.last();
    await first.done([{ id: "blockr.core-add_block", args: { registry_id: "blockr.core-head_block", id: "n" } }]);
    expect(Object.hasOwn(s.core.store.board().blocks, "n")).toBe(true);
    expect(first.signal.aborted).toBe(true);
    expect(shown.renders).toHaveLength(2);
    expect(shown.last()).toMatchObject({ picked: { class: "head" } });

    await shown.last().done();
    expect(shown.renders).toHaveLength(3);
    expect(room.textContent).toBe('The form of {"board":true}');

    unmount();
    expect(shown.last().signal.aborted).toBe(true);
    expect(shown.cleanups()).toBe(3);
    expect(room.childNodes).toHaveLength(0);
    s.stop();
  });

  test("leaves the advanced action as it is where the store refuses the batch, and its done() rejects with the refusal", async () => {
    const shown = form();
    const add = asking("Add form", ["board"], shown.module);
    const s = await stubbed([listing([add])]);
    const handed: (readonly Notification[])[] = [];

    s.core.boardActions.mountAdvanced(document.createElement("div"), add.id, { board: true });
    const args = shown.last();
    await expect(
      args.done(refused, {
        notifier(notifications) {
          handed.push(notifications);
        },
      }),
    ).rejects.toThrow(REFUSAL);

    expect(args.signal.aborted).toBe(false);
    expect(shown.renders).toHaveLength(1);
    expect(handed.map((notifications) => notifications.map(notificationCode))).toEqual([["board_edit_failed"]]);
    s.stop();
  });

  test("unmounts it and renders it no more where a board object that its target names leaves the board, and as the next board opens", async () => {
    const shown = form();
    const edit = asking("Edit form", ["block"], shown.module);
    const s = await stubbed([listing([edit])]);
    const room = document.createElement("div");

    s.core.boardActions.mountAdvanced(room, edit.id, { block: "b" });
    await s.core.commands.run("blockr.core-remove_block", { id: "b" });
    expect(shown.last().signal.aborted).toBe(true);
    expect(room.childNodes).toHaveLength(0);
    expect(shown.renders).toHaveLength(1);

    s.core.boardActions.mountAdvanced(room, edit.id, { block: "c" });
    const args = shown.last();
    const before = s.core;
    await s.core.store.open(JSON.stringify(HELLO.board));
    await s.settled();
    expect(args.signal.aborted).toBe(true);
    expect(() => before.boardActions.mountAdvanced(room, edit.id, { block: "c" })).toThrow(
      "Another board opened, so core mounts nothing more of the board before.",
    );
    s.stop();
  });

  test("renders nothing where a menu of the target does not offer the board action, and throws for an id that no board action has, a board action without an advanced action, and what is no target or no element", async () => {
    const shown = form();
    const edit = asking("Edit form", ["block"], shown.module, { offers: () => false });
    const s = await stubbed([listing([edit])]);
    const room = document.createElement("div");
    const mount = (...args: unknown[]) => (s.core.boardActions.mountAdvanced as (...given: unknown[]) => () => void)(...args);

    mount(room, edit.id, { block: "b" })();
    expect(shown.renders).toEqual([]);
    expect(() => mount(room, "blockr.test-nowhere_action", { block: "b" })).toThrow('No board action "blockr.test-nowhere_action" is registered.');
    expect(() => mount(room, "blockr.core-remove_block_action", { block: "b" })).toThrow(
      'The board action "blockr.core-remove_block_action" has no advanced action to mount.',
    );
    expect(() => mount(room, edit.id, { blok: "b" })).toThrow(TypeError);
    expect(() => mount("room", edit.id, { block: "b" })).toThrow(
      new TypeError("The `el` of mountAdvanced() is the element in which core renders the advanced action."),
    );
    s.stop();
  });
});
