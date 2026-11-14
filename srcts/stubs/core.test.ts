// The CoreApi for tests: core's own store, command registry and scheduler on
// the board of the fixture hello, with R played from the protocol's fixtures,
// as a frontend or a plugin would use it.

import { act } from "react";
import { describe, expect, test } from "vitest";
import type { Option } from "../store/index.ts";
import { find } from "../testing/inputs.ts";
import { logged } from "../testing/logged.ts";
import { classModule, optionModule } from "../testing/plugins.ts";
import { withOptions, withPlugins } from "../testing/store.ts";
// React's act() environment, for the UIs of core's board options.
import "../testing/react.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { OutputHost } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import { HELLO, INFO, stubCore } from "./index.ts";

const TABLE = "blockr.core/basic-table.js?v=b7e2a915";

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
    expect(Object.keys(core.commands.registered())).toContain("blockr.core-add_block");
    expect(Object.keys(core.scheduler).sort()).toEqual(["demand", "evaluate", "invalidate", "status", "subscribe"]);
    expect(core.blockRegistry()).toBe(HELLO.block_registry);
    expect(Object.keys(core.options.registered())).toEqual(["n_rows", "show_conditions", "dark_mode"]);
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
    expect(table.hosts[0]?.output()).toMatchObject({ renderer: { id: "blockr.core-table_output" }, total: 7 });
  });

  test("answers a plugin's requests with what the test gives, and reads the blockr options that R holds", async () => {
    // The app runs the plugin of blockr.code, whose class's module the test
    // gives by the class's id.
    const { core } = await stubCore({
      hello: withPlugins({ "blockr.code-code_plugin": { name: "Code" } }),
      modules: { "blockr.code-code_plugin": classModule() },
      blockrOptions: { n_rows: 25 },
      answer: (request) =>
        request.kind === "request" && request.type === "plugin" && request.name === "script" ? ["a <- 1"] : undefined,
    });

    await expect(core.request("blockr.code-code_plugin", "script", {}, { reads: ["a"] })).resolves.toEqual(["a <- 1"]);
    await expect(core.request("blockr.code-code_plugin", "other")).rejects.toThrow("R replied error");
    await expect(core.request("blockr.dag-dag_plugin", "script")).rejects.toThrow('The app runs no plugin "blockr.dag-dag_plugin"');
    await expect(core.blockrOption("n_rows")).resolves.toBe(25);
    await expect(core.blockrOption("gate_visibility")).resolves.toBeUndefined();
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

    expect(Object.keys(core.options.registered())).toEqual(["n_rows", "show_conditions", "dark_mode", "zoom"]);
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
    const started = ({ plugin }: { plugin: string }): undefined => {
      events.push(`${plugin} starts`);
    };
    // An app whose frontend takes the place of core's basic frontend.
    const rest = Object.fromEntries(Object.entries(HELLO.plugins).filter(([id]) => id !== "blockr.core-basic_frontend_plugin"));
    const s = await stubCore({
      el,
      hello: withPlugins(
        {
          "blockr.test-grid_plugin": { name: "Grid" },
          "blockr.test-notes_plugin": { name: "Notes" },
        },
        { ...HELLO, plugins: rest },
      ),
      modules: {
        "blockr.test-grid_plugin": classModule({
          frontend: {
            render({ el: at, core, plugin }: { el: HTMLElement; core: CoreApi; plugin: string }) {
              events.push(`${plugin} draws`);
              core.mountPlugin(at.appendChild(document.createElement("section")), "blockr.test-notes_plugin");
              return undefined;
            },
          },
          start: started,
        }),
        "blockr.test-notes_plugin": classModule({
          ui: {
            render({ el: at, plugin }: { el: HTMLElement; plugin: string }) {
              at.textContent = `The panel of ${plugin}`;
              return undefined;
            },
          },
          start: started,
        }),
      },
    });
    await s.settled();

    expect(events).toEqual([
      "blockr.test-grid_plugin draws",
      "blockr.test-grid_plugin starts",
      "blockr.test-notes_plugin starts",
    ]);
    expect(el.textContent).toBe("The panel of blockr.test-notes_plugin");
    s.stop();
  });

  test("draws the board with the frontend given, in the element given, and hears what renderers emit", async () => {
    const el = document.createElement("div");
    const table = renderer();
    const events: unknown[] = [];
    // The test gives a frontend of its own in place of core's basic frontend,
    // by the id of the class of the app's plugin that it replaces.
    const { core, settled } = await stubCore({
      el,
      modules: {
        [TABLE]: table.exports,
        "blockr.core-basic_frontend_plugin": classModule({
          options: ["show_conditions", "dark_mode"],
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
    });
    core.outputEvents.subscribe((event) => events.push(event));
    await settled();

    expect(el.textContent).toBe("Iris review");
    table.hosts[0]?.emit("select", { rows: [1] });
    expect(events).toEqual([{ block: "b", name: "select", payload: { rows: [1] } }]);
  });
});
