// Mounting a block's output, on the sample board of the wire protocol's
// fixtures, with the store, the command registry and the scheduler that core
// creates, and R played over the protocol's own client. The head block b reads
// the dataset a, and both draw table outputs, which core's plain table renders
// with the board option n_rows.

import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { createCommands } from "../commands/index.ts";
import type { HelloReply, OutputInfo, OutputRequest, Request } from "../protocol/index.ts";
import { counter, createScheduler } from "../scheduler/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import { createStore } from "../store/index.ts";
import type { Option, Store } from "../store/index.ts";
import { playR } from "../testing/r.ts";
import type { PlayedR } from "../testing/r.ts";
import { reports } from "../testing/reports.ts";
import { coreOptions, hello, noUi } from "../testing/store.ts";
import type { OutputHost, OutputModule } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { OutputRenderer } from "../types/outputs.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { createLoader, createMounts, ModuleError, RequestError } from "./index.ts";
import type { Measure, Mounts, OutputEvent } from "./index.ts";

const TABLE: OutputRenderer = { id: "blockr.core-table_output", module: "blockr.core/basic-table.js?v=b7e2a915" };
const PREVIEW: OutputRenderer = { id: "blockr.ui-table_preview", module: "blockr.ui/table-preview.js?v=1", css: "blockr.ui/table-preview.css?v=2" };
const TEXT: OutputRenderer = { id: "blockr.core-text_output", module: "blockr.core/basic-text.js?v=3c8f61d0" };

const INFO: OutputInfo = {
  width: 640,
  height: 400,
  pixel_ratio: 2,
  bg: "#ffffff",
  fg: "#212529",
  accent: "#0d6efd",
  font: "Inter",
};

// The reply to hello, with a board that holds no board option, so that the
// renderers bring theirs.
const bare: HelloReply = { ...hello, board: { ...hello.board, board: { ...hello.board.board, options: {} } } };

// A board option that a renderer of table outputs may name besides n_rows.
const zoom: Option = { id: "zoom", label: "Zoom", category: "View options", schema: { type: "number" }, default: 1, ui: noUi };

// A renderer that keeps each host it is mounted with, and each output that
// its host held as it announced one.
function renderer(named: readonly string[] = ["n_rows"]) {
  const mounts: { host: OutputHost; el: HTMLElement; signal: AbortSignal; seen: Json[] }[] = [];
  let cleanups = 0;
  const mod: OutputModule = {
    options: named,
    render({ el, host, signal }) {
      const mount = { host, el, signal, seen: [host.output()] };
      mounts.push(mount);
      el.textContent = JSON.stringify(host.output());
      const off = host.on("output", () => {
        mount.seen.push(host.output());
        el.textContent = JSON.stringify(host.output());
      });
      return () => {
        cleanups++;
        off();
      };
    },
  };
  return {
    exports: { api: 1, default: mod },
    mounts,
    cleanups: () => cleanups,
    host(i = 0): OutputHost {
      const mount = mounts[i];
      if (mount === undefined) throw new Error(`The renderer has no mount ${String(i)}.`);
      return mount.host;
    },
  };
}

// What an element measures, which a test changes, and whose watchers it calls.
function measuring(start: OutputInfo | null = INFO) {
  let info = start;
  const watchers = new Set<() => void>();
  const measure: Measure = {
    info: () => info,
    watch: (_el, fn) => {
      watchers.add(fn);
      return () => {
        watchers.delete(fn);
      };
    },
  };
  return {
    measure,
    set(next: OutputInfo | null) {
      info = next;
      for (const fn of [...watchers]) fn();
    },
  };
}

interface Setup {
  r: PlayedR;
  store: Store;
  scheduler: Scheduler;
  mounts: Mounts;
  imported: string[];
  events: OutputEvent[];
  size: ReturnType<typeof measuring>;
  // Ends what core runs for the board, as core does as another board opens,
  // and opens the board of the reply given in a new store with new mounts, on
  // the same R and the same loader, as core starts again.
  readonly next: (reply?: HelloReply) => Promise<Setup>;
}

async function setup(
  modules: Readonly<Record<string, unknown>> = {},
  { reply = bare, info = INFO }: { reply?: HelloReply; info?: OutputInfo | null } = {},
): Promise<Setup> {
  const r = playR();
  r.connect();
  const imported: string[] = [];
  const loader = createLoader({
    importer(url) {
      imported.push(url);
      return Object.hasOwn(modules, url)
        ? Promise.resolve(modules[url])
        : Promise.reject(new TypeError(`Failed to fetch dynamically imported module: ${url}`));
    },
    stylesheet: () => Promise.resolve(),
  });
  const events: OutputEvent[] = [];
  const size = measuring(info);
  const page = counter();

  async function open(given: HelloReply): Promise<Setup> {
    const controller = new AbortController();
    const { signal } = controller;
    const store = createStore({ signal });
    for (const option of [...coreOptions, zoom]) store.options.register(option);
    await store.open(given);
    const commands = createCommands(store, { signal });
    const scheduler = createScheduler({ store, client: r.client, nextRevision: page, signal });
    scheduler.start();
    const mounts = createMounts({
      store,
      commands,
      scheduler,
      loader,
      measure: size.measure,
      emit: (event) => {
        events.push(event);
      },
      signal,
    });
    return {
      r,
      store,
      scheduler,
      mounts,
      imported,
      events,
      size,
      next: (after = bare) => {
        controller.abort();
        return open(after);
      },
    };
  }

  return open(reply);
}

async function tick(ms = 0): Promise<void> {
  await vi.advanceTimersByTimeAsync(ms);
}

async function evaluateAll(r: PlayedR): Promise<void> {
  await tick();
  for (let i = 0; i < 20 && r.pending().length > 0; i++) {
    for (const request of r.pending()) r.evaluated(request);
    await tick();
  }
}

// Mounts b's output with its block demanded and evaluated.
async function ready(s: Setup, block = "b"): Promise<HTMLElement> {
  s.scheduler.demand("test", [block]);
  await evaluateAll(s.r);
  const el = document.createElement("div");
  s.mounts.mountOutput(el, block);
  await tick();
  return el;
}

function outputs(r: PlayedR): OutputRequest[] {
  return r.asked("output") as OutputRequest[];
}

function lastOutput(r: PlayedR): OutputRequest {
  const request = outputs(r).at(-1);
  if (request === undefined) throw new Error("R has been sent no output request.");
  return request;
}

// Replies to an output request with a table output of `total` rows, drawn by
// the renderer given.
function replyTable(r: PlayedR, request: OutputRequest, total = 150, drawn: OutputRenderer = TABLE): void {
  r.reply({ id: request.id, status: "ok", output: { renderer: drawn, columns: [], rows: [], total } });
}

function requests(r: PlayedR): Request[] {
  return r.asked("request");
}

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
});

describe("asking", () => {
  test("loads the renderer that the entry names and adds its board options before it asks R", async () => {
    const table = renderer(["n_rows", "zoom"]);
    const s = await setup({ [TABLE.module]: table.exports });
    const el = await ready(s);

    expect(s.imported).toEqual([TABLE.module]);
    // Beside those that core's basic frontend requires.
    expect(s.store.board().options).toEqual({ show_conditions: ["warning", "error"], dark_mode: "light", n_rows: 50, zoom: 1 });
    expect(outputs(s.r)).toHaveLength(1);
    expect(lastOutput(s.r)).toMatchObject({
      kind: "output",
      block: "b",
      revision: s.scheduler.status("b")?.revision,
      options: { n_rows: 50, zoom: 1 },
      info: INFO,
      params: {},
    });
    // Nothing draws until the output comes.
    expect(table.mounts).toHaveLength(0);
    expect(el.textContent).toBe("");

    replyTable(s.r, lastOutput(s.r));
    await tick();

    expect(table.mounts).toHaveLength(1);
    expect(table.host().output()).toMatchObject({ renderer: TABLE, total: 150 });
    expect(table.host().options()).toEqual({ n_rows: 50, zoom: 1 });
    expect(table.host().params()).toEqual({});
    expect(table.host().loading()).toBe(false);
    expect(el.textContent).toContain('"total":150');
  });

  test("asks for nothing while the block is not ready or the element measures nothing", async () => {
    const table = renderer();
    const s = await setup({ [TABLE.module]: table.exports }, { info: null });
    const el = document.createElement("div");
    s.mounts.mountOutput(el, "b");
    await tick();
    expect(outputs(s.r)).toHaveLength(0);

    s.scheduler.demand("test", ["b"]);
    await evaluateAll(s.r);
    expect(outputs(s.r)).toHaveLength(0);

    // It asks at once as the element first measures a size.
    s.size.set(INFO);
    await tick();
    expect(outputs(s.r)).toHaveLength(1);
  });

  test("asks again once the revision, a board option it names, the info or the parameters change, while the block is ready", async () => {
    const table = renderer();
    const s = await setup({ [TABLE.module]: table.exports });
    await ready(s);
    replyTable(s.r, lastOutput(s.r));
    await tick();

    // A new revision: nothing is asked until the block is ready again, and the
    // last output stays drawn meanwhile.
    s.scheduler.invalidate("b");
    await tick();
    expect(outputs(s.r)).toHaveLength(1);
    expect(table.host().output()).toMatchObject({ total: 150 });
    await evaluateAll(s.r);
    expect(outputs(s.r)).toHaveLength(2);
    expect(table.host().loading()).toBe(true);
    replyTable(s.r, lastOutput(s.r), 149);
    await tick();
    expect(table.host().loading()).toBe(false);

    // A board option that the renderer names, but not one it does not.
    s.store.apply({ options: { set: { show_conditions: ["error"] } } });
    await tick();
    expect(outputs(s.r)).toHaveLength(2);
    s.store.apply({ options: { set: { n_rows: 10 } } });
    await tick();
    expect(lastOutput(s.r).options).toEqual({ n_rows: 10 });
    replyTable(s.r, lastOutput(s.r));
    await tick();

    // The info, once the element has settled on its new size.
    s.size.set({ ...INFO, width: 600 });
    s.size.set({ ...INFO, width: 500 });
    await tick(100);
    expect(outputs(s.r)).toHaveLength(3);
    await tick(100);
    expect(outputs(s.r)).toHaveLength(4);
    expect(lastOutput(s.r).info.width).toBe(500);
    replyTable(s.r, lastOutput(s.r));
    await tick();

    // The parameters, which params() gives once the output asked with them
    // arrives.
    table.host().setParams({ page: 2 });
    await tick();
    expect(lastOutput(s.r).params).toEqual({ page: 2 });
    expect(table.host().params()).toEqual({});
    replyTable(s.r, lastOutput(s.r));
    await tick();
    expect(table.host().params()).toEqual({ page: 2 });
    expect(outputs(s.r)).toHaveLength(5);
  });

  test("gives up an ask that nothing waits on any more, and R gets a cancel", async () => {
    const table = renderer();
    const s = await setup({ [TABLE.module]: table.exports });
    await ready(s);
    replyTable(s.r, lastOutput(s.r));
    await tick();

    table.host().setParams({ page: 2 });
    await tick();
    const first = lastOutput(s.r);
    table.host().setParams({ page: 3 });
    await tick();

    expect(s.r.cancels()).toEqual([first.id]);
    expect(lastOutput(s.r).params).toEqual({ page: 3 });
  });

  test("keeps an error from R while the renderer keeps what it holds, or shows it where no output came before", async () => {
    const table = renderer();
    const s = await setup({ [TABLE.module]: table.exports });
    const el = await ready(s);
    const failure = { severity: "error" as const, class: ["simpleError", "error", "condition"], phase: "output" as const, message: "no page 9" };

    s.r.reply({ id: lastOutput(s.r).id, status: "error", conditions: [failure] });
    await tick();
    expect(el.querySelector("[role=alert]")?.textContent).toBe("no page 9");

    s.scheduler.invalidate("b");
    await evaluateAll(s.r);
    replyTable(s.r, lastOutput(s.r));
    await tick();
    expect(table.host().error()).toBeNull();
    expect(el.querySelector("[role=alert]")).toBeNull();

    table.host().setParams({ page: 9 });
    await tick();
    s.r.reply({ id: lastOutput(s.r).id, status: "error", conditions: [failure] });
    await tick();
    expect(table.host().error()).toEqual(failure);
    expect(table.host().output()).toMatchObject({ total: 150 });
  });
});

describe("what core keeps", () => {
  test("of an output draws a mount that asks with the same, without asking R, and two mounts at once ask once", async () => {
    const table = renderer();
    const s = await setup({ [TABLE.module]: table.exports });
    await ready(s);
    s.mounts.mountOutput(document.createElement("div"), "b");
    await tick();
    expect(outputs(s.r)).toHaveLength(1);

    replyTable(s.r, lastOutput(s.r));
    await tick();
    expect(table.mounts.map(({ host }) => host.output())).toEqual([table.host(0).output(), table.host(0).output()]);

    // A panel that opens again draws at once.
    const again = document.createElement("div");
    const unmount = s.mounts.mountOutput(again, "b");
    await tick();
    expect(outputs(s.r)).toHaveLength(1);
    expect(again.textContent).toContain('"total":150');
    unmount();
  });

  test("are the parameters, which every mount of the block shares", async () => {
    const table = renderer();
    const s = await setup({ [TABLE.module]: table.exports });
    await ready(s);
    s.mounts.mountOutput(document.createElement("div"), "b");
    await tick();
    replyTable(s.r, lastOutput(s.r));
    await tick();

    table.host(1).setParams({ page: 2, sort: { column: "Sepal.Length" } });
    await tick();
    expect(outputs(s.r)).toHaveLength(2);
    replyTable(s.r, lastOutput(s.r));
    await tick();
    expect(table.host(0).params()).toEqual({ page: 2, sort: { column: "Sepal.Length" } });
    expect(table.host(1).params()).toBe(table.host(0).params());
  });

  test("lasts for one opening of a board", async () => {
    const table = renderer();
    const s = await setup({ [TABLE.module]: table.exports });
    await ready(s);
    replyTable(s.r, lastOutput(s.r));
    await tick();
    table.host().setParams({ page: 2 });

    const next = await s.next();
    next.scheduler.demand("again", ["b"]);
    await evaluateAll(s.r);
    const el = document.createElement("div");
    next.mounts.mountOutput(el, "b");
    await tick();

    expect(el.textContent).toBe("");
    expect(lastOutput(s.r).params).toEqual({});
  });
});

describe("the renderer", () => {
  test("that an output names in place of the entry's draws it, with the board options it names", async () => {
    const table = renderer();
    const preview = renderer(["zoom"]);
    const s = await setup({ [TABLE.module]: table.exports, [PREVIEW.module]: preview.exports });
    await ready(s);
    replyTable(s.r, lastOutput(s.r), 150, PREVIEW);
    await tick();

    expect(table.mounts).toHaveLength(0);
    expect(preview.mounts).toHaveLength(1);
    expect(s.store.board().options).toMatchObject({ zoom: 1 });
    // The output was asked without zoom, so core asks again with it.
    expect(lastOutput(s.r).options).toEqual({ zoom: 1 });
  });

  test("is learned from the first output where the entry names none, which goes with every board option the board holds", async () => {
    const text = renderer([]);
    const plain: HelloReply = {
      ...bare,
      board: { ...bare.board, board: { ...bare.board.board, options: { show_conditions: { value: ["error"], version: 1 } } } },
      block_registry: Object.fromEntries(
        Object.entries(bare.block_registry).map(([id, entry]) => [
          id,
          Object.fromEntries(Object.entries(entry).filter(([key]) => key !== "renderer")) as unknown as RegistryEntry,
        ]),
      ),
    };
    const s = await setup({ [TEXT.module]: text.exports }, { reply: plain });
    await ready(s);
    expect(lastOutput(s.r).options).toEqual({ show_conditions: ["error"], dark_mode: "light" });

    s.r.reply({ id: lastOutput(s.r).id, status: "ok", output: { renderer: TEXT, text: "a" } });
    await tick();
    expect(text.host().output()).toEqual({ renderer: TEXT, text: "a" });
    // The renderer names no board option, so what the output was asked with
    // fits, and nothing more is asked.
    expect(outputs(s.r)).toHaveLength(1);
  });

  test("that fails to load, declares another major or throws shows an error in the element", async () => {
    const reported = reports();
    const s = await setup({ [TABLE.module]: { api: 2, default: renderer().exports.default } });
    const el = await ready(s);
    reported.restore();

    expect(el.textContent).toContain(
      "The module blockr.core/basic-table.js of blockr.core was built against major 2 of core's JS API",
    );
    expect(reported.errors().filter((error) => error instanceof ModuleError)).toHaveLength(1);
    expect(outputs(s.r)).toHaveLength(0);
  });

  test("hears a new output in place, and is mounted again by a mount after its cleanup ran", async () => {
    const table = renderer();
    const s = await setup({ [TABLE.module]: table.exports });
    const el = document.createElement("div");
    s.scheduler.demand("test", ["b"]);
    await evaluateAll(s.r);
    const unmount = s.mounts.mountOutput(el, "b");
    await tick();
    replyTable(s.r, lastOutput(s.r));
    await tick();
    s.scheduler.invalidate("b");
    await evaluateAll(s.r);
    replyTable(s.r, lastOutput(s.r), 3);
    await tick();

    // The renderer stays mounted, and hears each output, and each change of
    // whether core asks R, in place.
    expect(table.mounts).toHaveLength(1);
    expect([...new Set(table.mounts[0]?.seen.map((output) => (output as { total: number }).total))]).toEqual([150, 3]);

    unmount();
    expect(table.cleanups()).toBe(1);
    expect(table.mounts[0]?.signal.aborted).toBe(true);
    expect(el.textContent).toBe("");
  });
});

describe("a request", () => {
  // The output of b, drawn by core's plain table.
  async function drawn() {
    const table = renderer();
    const s = await setup({ [TABLE.module]: table.exports });
    await ready(s);
    replyTable(s.r, lastOutput(s.r));
    await tick();
    return { s, table };
  }

  test("of the output class or of the renderer goes for the output it holds, with what that was asked with", async () => {
    const { s, table } = await drawn();
    table.host().setParams({ page: 2 });
    await tick();
    replyTable(s.r, lastOutput(s.r));
    await tick();

    const download = table.host().request("download", { format: "csv" });
    const labels = table.host().request("labels", {}, { scope: "renderer" });
    await tick();
    const revision = s.scheduler.status("b")?.revision ?? 0;
    expect(requests(s.r)).toMatchObject([
      { kind: "request", type: "output_class", output: { block: "b", revision }, name: "download", args: { format: "csv" }, options: { n_rows: 50 }, params: { page: 2 } },
      { kind: "request", type: "renderer", renderer: TABLE.id, output: { block: "b", revision }, name: "labels", args: {}, options: { n_rows: 50 }, params: { page: 2 } },
    ]);
    const [first, second] = requests(s.r);
    s.r.reply({ id: first?.id ?? "", status: "ok", value: ["a,b"] });
    s.r.reply({ id: second?.id ?? "", status: "ok", value: { x: ["X"] } });
    await expect(download).resolves.toEqual(["a,b"]);
    await expect(labels).resolves.toEqual({ x: ["X"] });

    // Core keeps an answer as it keeps the output, unless R marks it volatile.
    await expect(table.host().request("labels", {}, { scope: "renderer" })).resolves.toEqual({ x: ["X"] });
    expect(requests(s.r)).toHaveLength(2);
  });

  test("waits while the block is stale and goes for the output of the current revision", async () => {
    const { s, table } = await drawn();
    s.scheduler.invalidate("b");
    await tick();
    const download = table.host().request("download");
    await tick();
    expect(requests(s.r)).toHaveLength(0);

    await evaluateAll(s.r);
    expect(requests(s.r)).toHaveLength(0);
    replyTable(s.r, lastOutput(s.r));
    await tick();
    const revision = s.scheduler.status("b")?.revision;
    expect(requests(s.r)[0]).toMatchObject({ output: { block: "b", revision } });
    s.r.reply({ id: requests(s.r)[0]?.id ?? "", status: "ok", value: [1] });
    await expect(download).resolves.toEqual([1]);
  });

  test("rejects at once with the block's status where it reads failed", async () => {
    const { s, table } = await drawn();
    s.scheduler.invalidate("b");
    await tick();
    for (const request of s.r.pending()) s.r.evaluated(request, "error");
    await tick();

    const failed = table.host().request("download");
    await expect(failed).rejects.toBeInstanceOf(RequestError);
    await expect(failed).rejects.toMatchObject({ blocks: [{ block: "b", status: { status: "failed" } }] });
    expect(requests(s.r)).toHaveLength(0);
  });

  test("asks again where R replies missing", async () => {
    const { s, table } = await drawn();
    const download = table.host().request("download");
    await tick();
    const first = requests(s.r)[0];
    // R lacks b's result at the revision that reads ready, as only a fault in
    // R makes it, so the scheduler has b evaluated again.
    s.r.reply({ id: first?.id ?? "", status: "missing", missing: [{ block: "b", revision: s.scheduler.status("b")?.revision ?? 0 }] });
    await tick();
    expect(requests(s.r)).toHaveLength(1);
    expect(s.scheduler.status("b")?.status).not.toBe("ready");

    // The scheduler had b evaluated again, and the request goes for the new
    // output.
    await evaluateAll(s.r);
    replyTable(s.r, lastOutput(s.r));
    await tick();
    expect(requests(s.r)).toHaveLength(2);
    s.r.reply({ id: requests(s.r)[1]?.id ?? "", status: "ok", value: [2] });
    await expect(download).resolves.toEqual([2]);
  });

  test("rejects with the reply's status and conditions", async () => {
    const { s, table } = await drawn();
    const failure = { severity: "error" as const, class: ["simpleError", "error", "condition"], phase: "request" as const, message: "no xlsx" };
    const download = table.host().request("download", { format: "xlsx" });
    await tick();
    s.r.reply({ id: requests(s.r)[0]?.id ?? "", status: "error", conditions: [failure] });
    await expect(download).rejects.toMatchObject({ status: "error", conditions: [failure] });
  });
});

describe("a mount", () => {
  test("reports what the renderer emits, by its block", async () => {
    const table = renderer();
    const s = await setup({ [TABLE.module]: table.exports });
    await ready(s);
    replyTable(s.r, lastOutput(s.r));
    await tick();
    table.host().emit("select", { rows: [1] });
    expect(s.events).toEqual([{ block: "b", name: "select", payload: { rows: [1] } }]);
  });

  test("ends once another board opens or its block leaves the board", async () => {
    const table = renderer();
    const s = await setup({ [TABLE.module]: table.exports });
    const el = await ready(s);
    replyTable(s.r, lastOutput(s.r));
    await tick();
    s.store.apply({ blocks: { rm: ["b"] } });
    expect(table.cleanups()).toBe(1);
    expect(el.textContent).toBe("");
    await expect(table.host().request("download")).rejects.toThrow("left the board");

    s.store.undo();
    const other = await ready(s, "a");
    s.r.reply({ id: lastOutput(s.r).id, status: "ok", output: { renderer: TABLE, columns: [], rows: [], total: 150 } });
    await tick();
    expect(other.textContent).toContain("150");
    await s.next();
    expect(table.cleanups()).toBe(2);
    expect(other.textContent).toBe("");
    expect(() => s.mounts.mountOutput(document.createElement("div"), "a")).toThrow(
      "Another board opened, so core mounts nothing more of the board before.",
    );
  });

  test("throws for a block that the board lacks", async () => {
    const s = await setup();
    expect(() => s.mounts.mountOutput(document.createElement("div"), "z")).toThrow('The board holds no block "z" whose output to mount.');
  });
});
