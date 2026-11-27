// The CoreApi for tests: core's own store, command registry and scheduler on
// the board of the fixture hello, with R played from the protocol's fixtures,
// as a frontend or a plugin would use it.

import { act, createElement } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { describe, expect, test, vi } from "vitest";
import { find } from "@blockr/ui/stubs";
import { blockRegistry } from "../protocol/index.ts";
import type { Condition } from "../protocol/index.ts";
import { outputRenderer } from "../react/index.ts";
import type { Option } from "../store/index.ts";
import { logged } from "../testing/logged.ts";
import { concerning, held } from "../testing/notifications.ts";
import { classModule, optionModule } from "../testing/plugins.ts";
import { withOptions, withoutPlugins, withPlugins } from "../testing/store.ts";
// React's act() environment, for the UIs of core's board options.
import "../testing/react.ts";
import type { PluginClassId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { OutputHost } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import { idText, interned } from "../util/index.ts";
import { HELLO, INFO, stubCore } from "./index.ts";

const TABLE = "blockr.core/basic-table.js?v=b7e2a915";

// The warnings of an update outside act() that React logged as `fn` ran, in
// its act() environment, which blockr.ui's stubs set up for a package's tests.
async function actWarnings(fn: () => unknown): Promise<string[]> {
  const lines: string[] = [];
  const spy = vi.spyOn(console, "error").mockImplementation((...args: unknown[]) => {
    lines.push(args.map(String).join(" "));
  });
  try {
    await fn();
  } finally {
    spy.mockRestore();
  }
  return lines.filter((line) => line.includes("not wrapped in act"));
}

// A renderer drawn with React through core's React SDK, as a package draws its
// own, which shows the id of the renderer that R names in the output.
const REACT_RENDERER = { api: 1, default: outputRenderer(({ output }) => createElement("p", null, output.renderer.id)) };

// A renderer that keeps the hosts it is mounted with.
function renderer() {
  const hosts: OutputHost[] = [];
  return {
    hosts,
    exports: {
      api: 1,
      default: {
        options: ["n_rows"],
        render({ host }: { host: OutputHost }) {
          hosts.push(host);
        },
      },
    },
  };
}

describe("the CoreApi for tests", () => {
  test("opens the board of the fixture hello, with core's store, command registry and scheduler", async () => {
    const { core, sent, stop } = await stubCore();

    expect(core.store.board().name).toBe("Iris review");
    expect([...core.commands.registered().keys()]).toContain(interned("blockr.core", "add_block"));
    expect(Object.keys(core.scheduler).sort()).toEqual(["demand", "evaluate", "invalidate", "status", "subscribe"]);
    expect(core.blockRegistry()).toBe(blockRegistry(HELLO));
    expect(Object.keys(core.options.registered())).toEqual(["n_rows", "dark_mode"]);
    expect(core.lock).toEqual({ board: false, load: false });
    // The second asks for the blockr option behind the default of dark_mode,
    // which core's basic frontend requires and the board lacks.
    expect(sent.map((request) => request.kind)).toEqual(["hello", "blockr_options", "blockr_options"]);
    expect(sent[2]).toMatchObject({ names: ["dark_mode"] });
    stop();
  });

  test("starts core again for a board that the test opens, whose CoreApi `core` then gives, as R drops what it held", async () => {
    const s = await stubCore({ modules: { [TABLE]: renderer().exports } });
    const before = s.core;
    before.scheduler.demand("test", ["b"]);
    await s.settled();
    const outputs = s.sent.filter((request) => request.kind === "output").length;

    await before.store.open(JSON.stringify({ ...HELLO.board, board: { ...HELLO.board.board, name: "Opened" } }));
    const after = s.core;
    expect(after).not.toBe(before);
    expect(after.store.board().name).toBe("Opened");
    expect(s.sent.filter((request) => request.kind === "hello")).toHaveLength(2);

    after.scheduler.demand("test", ["b"]);
    after.mountOutput(document.createElement("div"), "b");
    await s.settled();
    expect(after.scheduler.status("b")?.status).toBe("ready");
    // The output of b at its new revision, which R made from its result at
    // that revision, since it holds none from before.
    const output = s.sent.filter((request) => request.kind === "output").slice(outputs);
    expect(output).toHaveLength(1);
    expect(output[0]).toMatchObject({ block: "b", revision: after.scheduler.status("b")?.revision });
  });

  test("reads a board through the store, as the DAG's Paste reads a copied board, holding nothing", async () => {
    const { core, sent, stop } = await stubCore();
    const board = core.store.board();
    const requests = sent.length;

    const read = await core.store.read(JSON.stringify({ ...HELLO.board, board: { ...HELLO.board.board, name: "Copied" } }));

    expect(read.board.name).toBe("Copied");
    expect(Object.keys(read.board.blocks)).toEqual(Object.keys(HELLO.board.board.blocks));
    expect(read.dropped).toEqual([]);
    expect(core.store.board()).toBe(board);
    expect(sent.slice(requests)).toEqual([]);
    stop();
  });

  test("evaluates what is demanded, as R replies ok to every evaluation", async () => {
    const { core, settled } = await stubCore();
    core.scheduler.demand("test", ["b"]);
    await settled();

    expect(core.scheduler.status("a")?.status).toBe("ready");
    expect(core.scheduler.status("b")?.status).toBe("ready");
    expect(core.scheduler.status("c")?.status).toBe("unevaluated");
  });

  test("mounts a block's output, which R makes from the sample outputs of its class's renderer", async () => {
    const table = renderer();
    const { core, settled, sent } = await stubCore({ modules: { [TABLE]: table.exports } });
    core.scheduler.demand("test", ["b"]);
    core.mountOutput(document.createElement("div"), "b");
    await settled();

    expect(sent.find((request) => request.kind === "output")).toMatchObject({ block: "b", options: { n_rows: 50 }, info: INFO });
    expect(table.hosts[0]?.output()).toMatchObject({ renderer: { package: "blockr.core", id: "table_output" }, total: 7 });
  });

  test("answers a plugin's requests with what the test gives, and reads the blockr options that R holds", async () => {
    // The app runs the plugin of blockr.code, whose class's module the test
    // gives by the class's package and id.
    const code = interned("blockr.code", "code_plugin");
    const { core } = await stubCore({
      hello: withPlugins({ "blockr.code": { code_plugin: { name: "Code" } } }),
      offered: { "blockr.code": { code_plugin: classModule() } },
      blockrOptions: { n_rows: 25 },
      answer: (request) =>
        request.kind === "request" && request.type === "plugin" && request.name === "script" ? ["a <- 1"] : undefined,
    });

    await expect(core.request(code, "script", {}, { reads: ["a"] })).resolves.toEqual(["a <- 1"]);
    await expect(core.request(code, "other")).rejects.toThrow("R replied error");
    await expect(core.request(interned("blockr.dag", "dag_plugin"), "script")).rejects.toThrow(
      'The app runs no plugin "dag_plugin of blockr.dag"',
    );
    await expect(core.blockrOption("n_rows")).resolves.toBe(25);
    await expect(core.blockrOption("gate_visibility")).resolves.toBeUndefined();
  });

  test("replies to a request with the messages and warnings that the test gives, ok or error, before the error that its answer throws", async () => {
    const code = interned("blockr.code", "code_plugin");
    const read: Condition = { severity: "message", class: ["simpleMessage", "message", "condition"], phase: "request", message: "Read 150 rows." };
    const few: Condition = { severity: "warning", class: ["simpleWarning", "warning", "condition"], phase: "request", message: "Few rows." };
    const { core, stop } = await stubCore({
      hello: withPlugins({ "blockr.code": { code_plugin: { name: "Code" } } }),
      offered: { "blockr.code": { code_plugin: classModule() } },
      conditions: () => [read, few],
      answer: (request) => {
        if (request.kind !== "request" || request.name === "other") return undefined;
        if (request.name === "save") throw new Error("The disk is full.");
        return ["a <- 1"];
      },
    });

    await expect(core.request(code, "script")).resolves.toEqual(["a <- 1"]);
    expect(held(core, (n) => n.source.kind === "r").map((n) => [n.severity, n.message])).toEqual([
      ["message", "Read 150 rows."],
      ["warning", "Few rows."],
    ]);
    await expect(core.request(code, "save")).rejects.toMatchObject({
      status: "error",
      conditions: [read, few, { severity: "error", phase: "request", message: "The disk is full." }],
    });
    // R runs no function for a request that the test gives no answer to, so
    // its reply carries only its own error.
    await expect(core.request(code, "other")).rejects.toMatchObject({
      status: "error",
      conditions: [{ severity: "error", phase: "request" }],
    });
    stop();
  });

  test("replies to an output with the messages and warnings that the test gives with it", async () => {
    const table = renderer();
    const few: Condition = { severity: "warning", class: ["simpleWarning", "warning", "condition"], phase: "output", message: "Few rows." };
    const output = { renderer: { package: "blockr.core", id: "table_output", module: TABLE }, columns: [], rows: [], total: 150 };
    const { core, settled, stop } = await stubCore({
      modules: { [TABLE]: table.exports },
      output: () => ({ output, conditions: [few] }),
    });
    core.scheduler.demand("test", ["b"]);
    core.mountOutput(document.createElement("div"), "b");
    await settled();

    expect(table.hosts[0]?.output()).toMatchObject({ total: 150 });
    expect(held(core, concerning("b"))).toEqual([
      { severity: "warning", message: "Few rows.", source: { kind: "r", class: few.class, request: { type: "output" } }, about: { block: "b" } },
    ]);
    stop();
  });

  test("holds what core and the modules on the page log while it runs, and passes each line on to the logger in place before", async () => {
    const outer = logged();
    const s = await stubCore();
    s.core.log("blockr.test").warn("Careful.");
    s.stop();
    s.core.log("blockr.test").info("Stopped.");
    outer.restore();

    expect(s.logged.map((line) => [line.level, line.package, line.message])).toEqual([["warn", "blockr.test", "Careful."]]);
    expect(outer.lines().map((line) => line.message)).toEqual(["Careful.", "Stopped."]);
  });

  test("draws core's board options with their UIs, as the page does", async () => {
    const { core, settled } = await stubCore();
    const el = document.createElement("div");
    await act(async () => {
      core.mountOption(el, "n_rows");
      await settled();
    });

    // The value that the board of the fixture hello holds.
    expect(el.textContent).toContain("Preview rows");
    expect(find(el, "input", HTMLInputElement).value).toBe("50");
  });

  test("waits for R's replies inside React's act(), so that a module drawn with React draws what they bring without a warning", async () => {
    const { core, settled } = await stubCore({ modules: { [TABLE]: REACT_RENDERER } });
    const el = document.createElement("div");
    core.scheduler.demand("test", ["b"]);
    core.mountOutput(el, "b");

    expect(await actWarnings(settled)).toEqual([]);
    expect(el.textContent).toContain("table_output");
  });

  test("starts and stops core inside React's act(), so that a frontend drawn with React draws the board and unmounts without a warning", async () => {
    const el = document.createElement("div");
    const frontend = {
      render({ el: at }: { el: HTMLElement }) {
        const root = createRoot(at);
        flushSync(() => {
          root.render(createElement("p", null, "Drawn with React"));
        });
        return () => {
          root.unmount();
        };
      },
    };
    let stop = (): void => undefined;
    const warnings = await actWarnings(async () => {
      ({ stop } = await stubCore({ el, offered: { "blockr.core": { basic_frontend_plugin: classModule({ frontend }) } } }));
    });

    expect(warnings).toEqual([]);
    expect(el.textContent).toBe("Drawn with React");
    expect(await actWarnings(stop)).toEqual([]);
    expect(el.textContent).not.toContain("Drawn with React");
  });

  test("registers the board options that the reply names before the board opens, and calls a board option's onChange", async () => {
    const changes: Json[] = [];
    const zoom: Omit<Option, "id"> = {
      label: "Zoom",
      category: "View options",
      schema: { type: "number" },
      default: 1,
      ui: { render: () => undefined },
      onChange(value) {
        changes.push(value);
      },
    };
    const { core } = await stubCore({
      hello: withOptions(["zoom"]),
      modules: { zoom: optionModule(zoom) },
    });

    expect(Object.keys(core.options.registered())).toEqual(["n_rows", "dark_mode", "zoom"]);
    // As the board opened, with the default, which core added to the board.
    expect(changes).toEqual([1]);
    expect(core.store.board().options.zoom).toBe(1);
    core.store.apply({ options: { set: { zoom: 2 } } });
    core.store.apply({ options: { set: { n_rows: 10 } } });
    expect(changes).toEqual([1, 2]);
  });

  test("runs the app's plugins, of the classes whose modules the test gives, and draws the board with the app's frontend", async () => {
    const el = document.createElement("div");
    const events: string[] = [];
    const started = ({ plugin }: { plugin: PluginClassId }): undefined => {
      events.push(`${idText(plugin)} starts`);
    };
    // An app whose frontend takes the place of core's basic frontend.
    const rest = withoutPlugins(HELLO, { package: "blockr.core", id: "basic_frontend_plugin" });
    const s = await stubCore({
      el,
      hello: withPlugins(
        {
          "blockr.test": {
            grid_plugin: { name: "Grid" },
            notes_plugin: { name: "Notes" },
          },
        },
        rest,
      ),
      offered: {
        "blockr.test": {
          grid_plugin: classModule({
            frontend: {
              render({ el: at, core, plugin }: { el: HTMLElement; core: CoreApi; plugin: PluginClassId }) {
                events.push(`${idText(plugin)} draws`);
                core.mountPlugin(at.appendChild(document.createElement("section")), interned("blockr.test", "notes_plugin"));
                return undefined;
              },
            },
            start: started,
          }),
          notes_plugin: classModule({
            ui: {
              render({ el: at, plugin }: { el: HTMLElement; plugin: PluginClassId }) {
                at.textContent = `The panel of ${idText(plugin)}`;
                return undefined;
              },
            },
            start: started,
          }),
        },
      },
    });
    await s.settled();

    expect(events).toEqual([
      "grid_plugin of blockr.test draws",
      "grid_plugin of blockr.test starts",
      "notes_plugin of blockr.test starts",
    ]);
    expect(el.textContent).toBe("The panel of notes_plugin of blockr.test");
    s.stop();
  });

  test("draws the board with the frontend given, in the element given, and hears what renderers emit", async () => {
    const el = document.createElement("div");
    const table = renderer();
    const events: unknown[] = [];
    // The test gives a frontend of its own in place of core's basic frontend,
    // by the package and the id of the class of the app's plugin that it
    // replaces.
    const { core, settled } = await stubCore({
      el,
      modules: { [TABLE]: table.exports },
      offered: {
        "blockr.core": {
          basic_frontend_plugin: classModule({
            frontend: {
              render({ el: at, core: given }: { el: HTMLElement; core: CoreApi }) {
                at.textContent = given.store.board().name;
                given.scheduler.demand("frontend", ["b"]);
                given.mountOutput(document.createElement("div"), "b");
                return undefined;
              },
            },
          }),
        },
      },
    });
    core.outputEvents.subscribe((event) => events.push(event));
    await settled();

    expect(el.textContent).toBe("Iris review");
    table.hosts[0]?.emit("select", { rows: [1] });
    expect(events).toEqual([{ block: "b", name: "select", payload: { rows: [1] } }]);
  });
});
