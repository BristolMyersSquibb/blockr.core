// The CoreApi for tests: core's own store, command registry and scheduler on
// the board of the fixture hello, with R played from the protocol's fixtures,
// as a front-end, an extension or a startup module would use it.

import { describe, expect, test } from "vitest";
import type { Option } from "../store/index.ts";
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
    expect(core.lock).toBeNull();
    expect(sent.map((request) => request.kind)).toEqual(["hello", "blockr_options"]);
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
    expect(table.hosts[0]?.output()).toMatchObject({ renderer: { id: "blockr.core-table_output" }, total: 7 });
  });

  test("answers requests with what the test gives, and reads the blockr options that R holds", async () => {
    const { core } = await stubCore({
      blockrOptions: { n_rows: 25 },
      answer: (request) => (request.kind === "request" && request.type === "code" ? ["a <- 1"] : undefined),
    });

    await expect(core.request("code", { name: "script" }, { reads: ["a"] })).resolves.toEqual(["a <- 1"]);
    await expect(core.request("other")).rejects.toThrow("R replied error");
    await expect(core.blockrOption("n_rows", 50)).resolves.toBe(25);
    await expect(core.blockrOption("gate_visibility", true)).resolves.toBe(true);
  });

  test("initializes the startup modules given before the board opens, and calls a board option's onChange", async () => {
    const changes: Json[] = [];
    let seen: CoreApi | undefined;
    const zoom: Option = {
      id: "zoom",
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
      startup: {
        "blockr.test": {
          api: 1,
          default: {
            initialize({ core: given }: { core: CoreApi }) {
              seen = given;
              expect(given.store.hasBoard()).toBe(false);
              given.options.register(zoom);
            },
          },
        },
      },
    });

    expect(seen).toBe(core);
    core.store.apply({ options: { set: { zoom: 2 } } });
    core.store.apply({ options: { set: { n_rows: 10 } } });
    expect(changes).toEqual([2]);
  });

  test("draws the board with the front-end given, in the element given, and hears what renderers emit", async () => {
    const el = document.createElement("div");
    const table = renderer();
    const events: unknown[] = [];
    const { core, settled } = await stubCore({
      el,
      modules: {
        [TABLE]: table.exports,
        "blockr.core-basic": {
          api: 1,
          default: {
            render({ el: at, core: given }: { el: HTMLElement; core: CoreApi }) {
              at.textContent = given.store.board().name;
              given.scheduler.demand("front-end", ["b"]);
              given.mountOutput(document.createElement("div"), "b");
            },
          },
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
