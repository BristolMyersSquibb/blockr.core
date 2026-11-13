// Mounting a block's UI module, on the sample board of the wire protocol's
// fixtures, with the store, the command registry and the scheduler that core
// creates, and R played over the protocol's own client:
// - The dataset a feeds the head b, the subset d and the scatter f.
// - The merge c takes a as x and b as y.
// - The variadic rbind e binds b and d, in that order.

import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { CommandError, createCommands } from "../commands/index.ts";
import type { BlockClassRequest, Condition, HelloReply } from "../protocol/index.ts";
import { counter, createScheduler } from "../scheduler/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import { createStore } from "../store/index.ts";
import type { Option, Store } from "../store/index.ts";
import { playR } from "../testing/r.ts";
import type { PlayedR } from "../testing/r.ts";
import { reports } from "../testing/reports.ts";
import { coreOptions, hello, noCore, noUi } from "../testing/store.ts";
import type { BlockId } from "../types/board.ts";
import type { Owner, Raised } from "../types/core-api.ts";
import type { BlockContext, BlockHost, BlockModule } from "../types/hosts.ts";
import type { RegistryEntry } from "../types/registry.ts";
import type { Replied } from "../util/index.ts";
import { createLoader, createMounts, ModuleError, RequestError } from "./index.ts";
import type { Mounts, Source } from "./index.ts";

const HEAD = "blockr.core/head-block.js?v=9a41f2c7";
const HEAD_CSS = "blockr.core/head-block.css?v=c03e7b58";
const MERGE = "blockr.core/merge-block.js?v=77d0a9e3";
const UPLOAD = "blockr.core/upload-block.js?v=d5932c60";

const head = registryEntry("blockr.core-head_block");

function registryEntry(id: string): RegistryEntry {
  const entry = hello.block_registry[id];
  if (entry === undefined) throw new Error(`The block registry has no class "${id}".`);
  return entry;
}

// The reply to hello, with requests of the head block's class beside n_max: one
// that reads its data, its state arg direction and the board option n_rows,
// and a volatile one.
const withRequests: HelloReply = {
  ...hello,
  block_registry: {
    ...hello.block_registry,
    [head.id]: {
      ...head,
      requests: {
        ...head.requests,
        columns: { inputs: ["data"], state_args: ["direction"], options: ["n_rows"] },
        listing: { inputs: [], state_args: [], options: [], volatile: true },
      },
    },
  },
};

// A board option that no board of the fixtures holds.
const zoom: Option = {
  id: "zoom",
  label: "Zoom",
  category: "View options",
  schema: { type: "number" },
  default: 1,
  ui: noUi,
};

// A UI module that keeps each host it is mounted with and its element and
// signal, draws the block's n, and counts its cleanups.
function recorder(named?: readonly string[]) {
  const mounts: { host: BlockHost; el: HTMLElement; signal: AbortSignal; context: BlockContext }[] = [];
  let cleanups = 0;
  const mod: BlockModule = {
    ...(named === undefined ? {} : { options: named }),
    render({ el, host, signal }) {
      mounts.push({ host, el, signal, context: host.context() });
      el.textContent = `n = ${JSON.stringify(host.state().n)}`;
      return () => {
        cleanups++;
      };
    },
  };
  return {
    exports: { api: 1, default: mod },
    mounts,
    cleanups: () => cleanups,
    host(i = 0): BlockHost {
      const mounted = mounts[i];
      if (mounted === undefined) throw new Error(`The module has no mount ${String(i)}.`);
      return mounted.host;
    },
  };
}

interface Setup {
  r: PlayedR;
  store: Store;
  scheduler: Scheduler;
  mounts: Mounts;
  imported: string[];
  sheets: string[];
  // What the mounts handed on of each of R's replies to a request.
  replies: Parameters<Replied>[];
  // The notifications that modules raised through their hosts, while they
  // hold.
  notified: { owner: Owner; pkg: string | null; notification: Raised }[];
  // Ends what core runs for the board, as core does as another board opens,
  // and opens the board of the reply given in a new store with new mounts, on
  // the same R and the same loader, as core starts again.
  readonly next: (reply?: HelloReply) => Promise<Setup>;
}

async function setup(
  modules: Readonly<Record<string, unknown>> = {},
  {
    reply = hello,
    options = [],
    offered = {},
  }: { reply?: HelloReply; options?: readonly Option[]; offered?: Readonly<Record<string, Source>> } = {},
): Promise<Setup> {
  const r = playR();
  r.connect();
  const imported: string[] = [];
  const sheets: string[] = [];
  const loader = createLoader({
    importer(url) {
      imported.push(url);
      return Object.hasOwn(modules, url)
        ? Promise.resolve(modules[url])
        : Promise.reject(new TypeError(`Failed to fetch dynamically imported module: ${url}`));
    },
    stylesheet(url) {
      sheets.push(url);
      return Promise.resolve();
    },
    modules: () => offered,
  });
  const page = counter();

  async function open(given: HelloReply): Promise<Setup> {
    const controller = new AbortController();
    const { signal } = controller;
    const store = createStore({ signal });
    for (const option of [...coreOptions, ...options]) store.options.register(option);
    await store.open(given);
    const commands = createCommands(store, { signal });
    const scheduler = createScheduler({ store, client: r.client, nextRevision: page, signal });
    scheduler.start();
    const replies: Parameters<Replied>[] = [];
    const notified: { owner: Owner; pkg: string | null; notification: Raised }[] = [];
    const mounts = createMounts({
      core: noCore,
      store,
      commands,
      scheduler,
      loader,
      signal,
      replied: (...args) => {
        replies.push(args);
      },
      notify: (owner, pkg, notification) => {
        const given = { owner, pkg, notification };
        notified.push(given);
        return () => {
          const at = notified.indexOf(given);
          if (at >= 0) notified.splice(at, 1);
        };
      },
    });
    return {
      r,
      store,
      scheduler,
      mounts,
      imported,
      sheets,
      replies,
      notified,
      next: (after = hello) => {
        controller.abort();
        return open(after);
      },
    };
  }

  return open(reply);
}

// Lets the time pass, running the scheduler's passes, the replies that are due
// and whatever waits on them.
async function tick(ms = 0): Promise<void> {
  await vi.advanceTimersByTimeAsync(ms);
}

// Answers every evaluation that waits on R with ok, until none waits.
async function evaluateAll(r: PlayedR): Promise<void> {
  await tick();
  for (let i = 0; i < 20 && r.pending().length > 0; i++) {
    for (const request of r.pending()) r.evaluated(request);
    await tick();
  }
}

// The requests of block classes that R has been sent.
function requests(r: PlayedR): BlockClassRequest[] {
  return r.asked("request") as BlockClassRequest[];
}

function lastRequest(r: PlayedR): BlockClassRequest {
  const request = requests(r).at(-1);
  if (request === undefined) throw new Error("R has been sent no request.");
  return request;
}

function blockRevision(scheduler: Scheduler, id: BlockId): number {
  const status = scheduler.status(id);
  if (status === undefined) throw new Error(`Block "${id}" has no status.`);
  return status.revision;
}

const failure: Condition = {
  severity: "error",
  class: ["simpleError", "error", "condition"],
  phase: "request",
  message: "object 'Species' not found",
};

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
});

describe("loading", () => {
  test("imports a module and its stylesheet once, however many blocks and mounts use it", async () => {
    const ui = recorder();
    const { store, mounts, imported, sheets } = await setup({ [HEAD]: ui.exports });
    store.apply({ blocks: { add: { g: { registry_id: head.id, state: { n: 3, direction: "head" }, name: "Head", locked: null } } } });

    const els = [document.createElement("div"), document.createElement("div"), document.createElement("div")];
    mounts.mountBlock(els[0] as HTMLElement, "b");
    mounts.mountBlock(els[1] as HTMLElement, "b");
    mounts.mountBlock(els[2] as HTMLElement, "g");
    await tick();

    expect(imported).toEqual([HEAD]);
    expect(sheets).toEqual([HEAD_CSS]);
    expect(els.map((el) => el.textContent)).toEqual(["n = 6", "n = 6", "n = 3"]);
    expect(ui.mounts.map(({ host }) => [host.block, host.class])).toEqual([
      ["b", head.id],
      ["b", head.id],
      ["g", head.id],
    ]);
  });

  test.each([
    ["declares another major", { api: 2, default: recorder().exports.default }, "of blockr.core was built against major 2 of core's JS API, where core's is 1."],
    ["declares no major", { default: recorder().exports.default }, "of blockr.core declares no major of core's JS API"],
    ["has no render()", { api: 1, default: {} }, "of blockr.core has no default export with a render() function."],
    ["throws in render()", { api: 1, default: { render: () => { throw new Error("boom"); } } }, "of blockr.core failed in render(): boom"],
    ["rejects in render()", { api: 1, default: { render: () => Promise.reject(new Error("later")) } }, "of blockr.core failed in render(): later"],
  ])("shows an error in the element that names the module and its package, where the module %s", async (_, exports, message) => {
    const reported = reports();
    const { mounts, scheduler, r } = await setup({ [HEAD]: exports, [MERGE]: recorder().exports });
    const el = document.createElement("div");
    const other = document.createElement("div");
    mounts.mountBlock(el, "b");
    mounts.mountBlock(other, "c");
    scheduler.demand("test", ["b"]);
    await evaluateAll(r);
    reported.restore();

    expect(el.querySelector("[role=alert]")?.textContent).toContain(`The module blockr.core/head-block.js ${message}`);
    expect(reported.errors().filter((error) => error instanceof ModuleError)).toHaveLength(1);
    // The rest of the page, the block's evaluation included, carries on.
    expect(other.textContent).toBe("n = undefined");
    expect(scheduler.status("b")?.status).toBe("ready");
  });

  test("shows an error where the module fails to load, and every later mount of it shows it too", async () => {
    const reported = reports();
    const { mounts, imported } = await setup();
    const els = [document.createElement("div"), document.createElement("div")];
    mounts.mountBlock(els[0] as HTMLElement, "b");
    await tick();
    mounts.mountBlock(els[1] as HTMLElement, "b");
    await tick();
    reported.restore();

    expect(imported).toEqual([HEAD]);
    for (const el of els) {
      expect(el.textContent).toBe(
        "The module blockr.core/head-block.js of blockr.core failed to load: Failed to fetch dynamically imported module: blockr.core/head-block.js?v=9a41f2c7",
      );
    }
  });

  test("throws for a block that the board lacks", async () => {
    const { mounts } = await setup();
    expect(() => mounts.mountBlock(document.createElement("div"), "z")).toThrow('The board holds no block "z" to mount.');
  });

  test("resolves a module that another package offers, by id, through the host", async () => {
    const ui = recorder();
    const filter = { api: 1, filter: "blockr.dplyr's" };
    const { mounts, imported, sheets } = await setup(
      { [HEAD]: ui.exports, "blockr.dplyr/filter.js?v=1": filter },
      { offered: { "blockr.dplyr-filter": { module: "blockr.dplyr/filter.js?v=1", css: "blockr.dplyr/filter.css?v=2" } } },
    );
    mounts.mountBlock(document.createElement("div"), "b");
    await tick();

    await expect(ui.host().module("blockr.dplyr-filter")).resolves.toBe(filter);
    await expect(ui.host().module("blockr.dplyr-filter")).resolves.toBe(filter);
    await expect(ui.host().module("blockr.dplyr-select")).rejects.toThrow('No package offers a module "blockr.dplyr-select".');
    expect(imported).toEqual([HEAD, "blockr.dplyr/filter.js?v=1"]);
    expect(sheets).toEqual([HEAD_CSS, "blockr.dplyr/filter.css?v=2"]);
  });
});

describe("board options", () => {
  test("come onto the board as a module that names them loads, each with its default where the board lacks it", async () => {
    const ui = recorder(["zoom", "n_rows"]);
    const { store, mounts } = await setup({ [HEAD]: ui.exports }, { options: [zoom] });
    store.apply({ options: { set: { n_rows: 25 } } });

    mounts.mountBlock(document.createElement("div"), "b");
    await tick();

    expect(store.board().options).toMatchObject({ zoom: 1, n_rows: 25 });
    // The module renders once the board holds them.
    expect(ui.mounts[0]?.context.options).toEqual({ zoom: 1, n_rows: 25 });
    // Undo leaves them on the board, since a step that sets only board option
    // values is no undo step.
    store.undo();
    store.undo("blockr.core");
    expect(store.board().options).toMatchObject({ zoom: 1, n_rows: 25 });
    expect([store.canUndo(), store.canUndo("blockr.core")]).toEqual([false, false]);
  });

  test("take a default that is a function, which the mounts that add it at once call once", async () => {
    const ui = recorder(["zoom"]);
    let calls = 0;
    const lazy: Option = {
      ...zoom,
      default: () => {
        calls++;
        return Promise.resolve(2);
      },
    };
    const s = await setup({ [HEAD]: ui.exports }, { options: [lazy] });

    s.mounts.mountBlock(document.createElement("div"), "b");
    s.mounts.mountBlock(document.createElement("div"), "b");
    await tick();
    expect(s.store.board().options.zoom).toBe(2);

    expect(calls).toBe(1);

    // A board that opens later lacks it, and gets it as a page that starts
    // with that board would.
    const next = await s.next();
    next.mounts.mountBlock(document.createElement("div"), "b");
    await tick();
    expect(next.store.board().options.zoom).toBe(2);
    expect(calls).toBe(2);
  });

  test("whose default resolves once another board opened land on neither board", async () => {
    const ui = recorder(["zoom"]);
    const pending: ((value: number) => void)[] = [];
    const lazy: Option = { ...zoom, default: () => new Promise((done) => pending.push(done)) };
    const s = await setup({ [HEAD]: ui.exports }, { options: [lazy] });

    s.mounts.mountBlock(document.createElement("div"), "b");
    await tick();
    const next = await s.next();
    pending[0]?.(2);
    await tick();
    expect(Object.hasOwn(s.store.board().options, "zoom")).toBe(false);
    expect(Object.hasOwn(next.store.board().options, "zoom")).toBe(false);
    expect(ui.mounts).toHaveLength(0);

    next.mounts.mountBlock(document.createElement("div"), "b");
    await tick();
    pending[1]?.(3);
    await tick();
    expect(next.store.board().options.zoom).toBe(3);
  });

  test("keep a value set while the default resolves", async () => {
    const ui = recorder(["zoom"]);
    let resolve: (value: number) => void = () => undefined;
    const lazy: Option = { ...zoom, default: () => new Promise((done) => (resolve = done)) };
    const { store, mounts } = await setup({ [HEAD]: ui.exports }, { options: [lazy] });

    mounts.mountBlock(document.createElement("div"), "b");
    await tick();
    store.apply({ options: { set: { zoom: 3 } } });
    resolve(2);
    await tick();

    expect(store.board().options.zoom).toBe(3);
  });

  test("come onto the board again once a delta removes them, while a module that names them is mounted", async () => {
    const ui = recorder(["zoom"]);
    const { store, mounts } = await setup({ [HEAD]: ui.exports }, { options: [zoom] });
    store.apply({ options: { set: { zoom: 5 } } });
    const unmount = mounts.mountBlock(document.createElement("div"), "b");
    await tick();
    const host = ui.host();
    expect(host.context().options).toEqual({ zoom: 5 });

    store.apply({ options: { unset: ["zoom"] } });
    await tick();
    expect(store.board().options.zoom).toBe(1);
    expect(host.context().options).toEqual({ zoom: 1 });

    // Once the module is unmounted, nothing adds it again.
    unmount();
    store.apply({ options: { unset: ["zoom"] } });
    await tick();
    expect(Object.hasOwn(store.board().options, "zoom")).toBe(false);
  });

  test("whose default fails or does not go onto the board show why in the block's element, in place of the module", async () => {
    const reported = reports();
    const ui = recorder(["broken", "unfit", "zoom"]);
    const broken: Option = { ...zoom, id: "broken", default: () => Promise.reject(new Error("No zoom today.")) };
    const unfit: Option = { ...zoom, id: "unfit", default: () => Promise.resolve("big") };
    const { store, mounts, scheduler, r } = await setup({ [HEAD]: ui.exports }, { options: [zoom, broken, unfit] });
    const els = [document.createElement("div"), document.createElement("div")];
    for (const el of els) mounts.mountBlock(el, "b");
    scheduler.demand("test", ["b"]);
    await evaluateAll(r);
    reported.restore();

    const what = 'which the UI module of block class "blockr.core-head_block" requires';
    const failed = `The default of the board option "broken", ${what}, failed: No zoom today.`;
    const refused = `The default of the board option "unfit", ${what}, did not go onto the board: `;
    for (const el of els) {
      expect(el.querySelector("[role=alert]")?.textContent).toContain(failed);
      expect(el.querySelector("[role=alert]")?.textContent).toContain(refused);
    }
    // Each is reported once, however many mounts require it.
    expect(reported.errors().map(String)).toEqual([`Error: ${failed}`, expect.stringMatching(new RegExp(`^Error: ${refused}`))]);
    expect(ui.mounts).toHaveLength(0);
    expect(store.board().options).toMatchObject({ zoom: 1 });
    expect(Object.keys(store.board().options)).not.toContain("unfit");
    // The block still evaluates.
    expect(scheduler.status("b")?.status).toBe("ready");
  });

  test("that no package defines show why in the block's element, in place of the module", async () => {
    const reported = reports();
    const ui = recorder(["nowhere"]);
    const { mounts } = await setup({ [HEAD]: ui.exports });
    const el = document.createElement("div");
    mounts.mountBlock(el, "b");
    await tick();
    reported.restore();

    const why = 'The board option "nowhere", which the UI module of block class "blockr.core-head_block" requires, is defined by no package loaded.';
    expect(el.querySelector("[role=alert]")?.textContent).toBe(why);
    expect(reported.errors().map(String)).toEqual([`Error: ${why}`]);
    expect(ui.mounts).toHaveLength(0);
  });

  test("end a module that has drawn once one that it names does not go onto the board again", async () => {
    const reported = reports();
    const ui = recorder(["zoom"]);
    let calls = 0;
    const once: Option = {
      ...zoom,
      default: () => (++calls === 1 ? Promise.resolve(1) : Promise.reject(new Error("No zoom any more."))),
    };
    const { store, mounts } = await setup({ [HEAD]: ui.exports }, { options: [once] });
    const el = document.createElement("div");
    mounts.mountBlock(el, "b");
    await tick();
    expect(ui.mounts).toHaveLength(1);

    store.apply({ options: { unset: ["zoom"] } });
    await tick();
    reported.restore();

    const why = 'The default of the board option "zoom", which the UI module of block class "blockr.core-head_block" requires, failed: No zoom any more.';
    expect(el.querySelector("[role=alert]")?.textContent).toBe(why);
    expect(ui.cleanups()).toBe(1);
    expect(ui.mounts[0]?.signal.aborted).toBe(true);
    expect(reported.errors().map(String)).toEqual([`Error: ${why}`]);
  });

  test("that a request of the class reads refuse a module that leaves one out, with an error in its element, while the page carries on", async () => {
    const reported = reports();
    const ui = recorder(["zoom"]);
    const { store, mounts, scheduler, r } = await setup(
      { [HEAD]: ui.exports, [MERGE]: recorder().exports },
      { reply: withRequests, options: [zoom] },
    );
    const el = document.createElement("div");
    const other = document.createElement("div");
    mounts.mountBlock(el, "b");
    mounts.mountBlock(other, "c");
    scheduler.demand("test", ["b"]);
    await evaluateAll(r);
    reported.restore();

    expect(el.querySelector("[role=alert]")?.textContent).toBe(
      'The module blockr.core/head-block.js of blockr.core leaves out of its `options` board options that requests of block class "blockr.core-head_block" read:\n' +
        '- The request "columns" reads the board option "n_rows".',
    );
    expect(reported.errors().filter((error) => error instanceof ModuleError)).toHaveLength(1);
    // Core calls no render() and adds none of the module's board options,
    // though the board holds a value for the one it leaves out.
    expect(ui.mounts).toHaveLength(0);
    expect(Object.hasOwn(store.board().options, "zoom")).toBe(false);
    // The rest of the page, the block's evaluation included, carries on.
    expect(other.textContent).toBe("n = undefined");
    expect(scheduler.status("b")?.status).toBe("ready");
  });

  test("left out are named with each request that reads them", async () => {
    const reported = reports();
    const reply: HelloReply = {
      ...hello,
      block_registry: {
        ...hello.block_registry,
        [head.id]: {
          ...head,
          requests: {
            ...head.requests,
            columns: { inputs: ["data"], state_args: ["direction"], options: ["n_rows"] },
            page: { inputs: ["data"], state_args: [], options: ["n_rows", "zoom"] },
          },
        },
      },
    };
    const { mounts } = await setup({ [HEAD]: recorder().exports }, { reply, options: [zoom] });
    const el = document.createElement("div");
    mounts.mountBlock(el, "b");
    await tick();
    reported.restore();

    expect(el.textContent).toBe(
      'The module blockr.core/head-block.js of blockr.core leaves out of its `options` board options that requests of block class "blockr.core-head_block" read:\n' +
        '- The request "columns" reads the board option "n_rows".\n' +
        '- The request "page" reads the board options "n_rows" and "zoom".',
    );
  });

  test("that a request reads are checked against the class of each block that a module mounts for", async () => {
    const reported = reports();
    const ui = recorder();
    // The merge block's class has the head block's module draw it too.
    const merge = registryEntry("blockr.core-merge_block");
    const reply: HelloReply = {
      ...withRequests,
      block_registry: { ...withRequests.block_registry, [merge.id]: { ...merge, module: HEAD } },
    };
    const { mounts } = await setup({ [HEAD]: ui.exports }, { reply });
    const [b, c] = [document.createElement("div"), document.createElement("div")];
    mounts.mountBlock(b, "b");
    mounts.mountBlock(c, "c");
    await tick();
    reported.restore();

    expect(b.querySelector("[role=alert]")?.textContent).toContain('The request "columns" reads the board option "n_rows".');
    expect(ui.mounts.map(({ host }) => host.block)).toEqual(["c"]);
  });
});

describe("the host", () => {
  test("gives the block's state and context, the same objects until it announces a change", async () => {
    const ui = recorder(["n_rows"]);
    const { mounts, scheduler } = await setup({ [HEAD]: ui.exports });
    mounts.mountBlock(document.createElement("div"), "b");
    await tick();
    const host = ui.host();

    expect(host.state()).toEqual({ n: 6, direction: "head" });
    expect(host.state()).toBe(host.state());
    expect(host.context()).toBe(host.context());
    expect(host.context()).toEqual({
      name: "Head",
      status: scheduler.status("b"),
      inputs: [{ input: "data", revision: blockRevision(scheduler, "a") }],
      options: { n_rows: 50 },
    });
  });

  test("announces a change it did not make, the state before the context", async () => {
    const ui = recorder();
    const { store, mounts } = await setup({ [HEAD]: ui.exports });
    mounts.mountBlock(document.createElement("div"), "b");
    await tick();
    const host = ui.host();
    const heard: string[] = [];
    host.on("state", () => heard.push(`state ${JSON.stringify(host.state())}`));
    host.on("context", () => heard.push(`context ${String(host.context().status.revision)}`));
    const revision = host.context().status.revision;

    store.apply({ blocks: { mod: { b: { state: { n: 10 } } } } }, "assistant");

    expect(heard).toEqual([
      'state {"n":10,"direction":"head"}',
      `context ${String(host.context().status.revision)}`,
    ]);
    expect(host.context().status.revision).toBeGreaterThan(revision);
  });

  test("announces a new status from R, a new revision of an input, a new name and a new value of a board option it names", async () => {
    const ui = recorder(["n_rows"]);
    const { r, store, scheduler, mounts } = await setup({ [HEAD]: ui.exports });
    mounts.mountBlock(document.createElement("div"), "b");
    await tick();
    const host = ui.host();
    const contexts: BlockContext[] = [];
    host.on("context", () => contexts.push(host.context()));
    host.on("state", () => {
      throw new Error("The state did not change.");
    });

    scheduler.demand("test", ["b"]);
    await evaluateAll(r);
    expect(contexts.at(-1)?.status.status).toBe("ready");

    scheduler.invalidate("a");
    expect(contexts.at(-1)?.inputs).toEqual([{ input: "data", revision: blockRevision(scheduler, "a") }]);

    store.apply({ blocks: { mod: { b: { name: "First rows" } } } });
    expect(contexts.at(-1)?.name).toBe("First rows");

    store.apply({ options: { set: { n_rows: 10 } } });
    expect(contexts.at(-1)?.options).toEqual({ n_rows: 10 });

    // A board option the module does not name is no news to it.
    const heard = contexts.length;
    store.apply({ options: { set: { show_conditions: ["error"] } } });
    expect(contexts).toHaveLength(heard);
  });

  test("submits through core's command, as one step of the user, and announces it before submit() resolves", async () => {
    const ui = recorder();
    const { store, mounts } = await setup({ [HEAD]: ui.exports });
    mounts.mountBlock(document.createElement("div"), "b");
    await tick();
    const host = ui.host();
    const heard: unknown[] = [];
    host.on("state", () => heard.push(host.state()));

    const submitted = host.submit({ n: 10 });
    expect(heard).toEqual([{ n: 10, direction: "head" }]);
    await submitted;

    expect(store.board().blocks.b?.state).toEqual({ n: 10, direction: "head" });
    store.undo();
    expect(store.board().blocks.b?.state).toEqual({ n: 6, direction: "head" });
  });

  test("rejects a submit with what refused it", async () => {
    const ui = recorder();
    const { mounts } = await setup({ [HEAD]: ui.exports });
    mounts.mountBlock(document.createElement("div"), "b");
    await tick();

    const refused = ui.host().submit({ n: -1 });
    await expect(refused).rejects.toBeInstanceOf(CommandError);
    await expect(refused).rejects.toThrow('The state of block "b" fails its schema at /n');
  });

  test("of each mount of one block is its own, and each hears every change", async () => {
    const ui = recorder();
    const { mounts } = await setup({ [HEAD]: ui.exports });
    const els = [document.createElement("div"), document.createElement("div")];
    mounts.mountBlock(els[0] as HTMLElement, "b");
    mounts.mountBlock(els[1] as HTMLElement, "b");
    await tick();
    const [one, two] = [ui.host(0), ui.host(1)];
    expect(one).not.toBe(two);
    const heard: string[] = [];
    one.on("state", () => heard.push("one"));
    two.on("state", () => heard.push("two"));

    await one.submit({ n: 2 });

    expect(heard).toEqual(["one", "two"]);
    expect(two.state()).toEqual({ n: 2, direction: "head" });
  });

  test("runs a one-time evaluation of its block with parameters, which rejects where the block reads waiting", async () => {
    const ui = recorder();
    const { r, store, mounts } = await setup({ [HEAD]: ui.exports });
    mounts.mountBlock(document.createElement("div"), "b");
    await tick();
    const host = ui.host();

    const saved = host.evaluate({ save: true });
    await evaluateAll(r);
    expect(r.evaluations().find((request) => request.block === "b")?.params).toEqual({ save: true });
    await expect(saved).resolves.toMatchObject({ status: "ready" });

    store.apply({ links: { rm: ["ab"] } });
    await expect(host.evaluate({ save: true })).rejects.toThrow(
      'Block "b" is waiting for its input "data" to be linked. Its evaluation with parameters is not sent.',
    );
    await expect(host.evaluate(undefined as unknown as Record<string, never>)).rejects.toThrow(TypeError);
  });
});

describe("a mount", () => {
  test("raises what the module gives its host's notify(), from the block, with the package of its class, until the mount ends", async () => {
    const ui = recorder();
    const { mounts, notified, store } = await setup({ [HEAD]: ui.exports });
    const cleanup = mounts.mountBlock(document.createElement("div"), "b");
    mounts.mountBlock(document.createElement("div"), "b");
    await tick();
    const given: Raised = { severity: "warning", code: "few_rows", message: "Few rows.", about: { block: "a" } };
    const withdraw = ui.host(0).notify(given);
    ui.host(1).notify({ ...given, code: "other" });
    expect(notified).toEqual([
      { owner: { block: "b" }, pkg: "blockr.core", notification: given },
      { owner: { block: "b" }, pkg: "blockr.core", notification: { ...given, code: "other" } },
    ]);
    withdraw();
    expect(notified.map((n) => n.notification.code)).toEqual(["other"]);
    expect(() => ui.host(0).notify({ ...given, message: 1 } as unknown as Raised)).toThrow(
      "The message of a notification is a string, not 1.",
    );

    // The first mount ends, and the second as its block leaves the board.
    ui.host(0).notify(given);
    cleanup();
    expect(notified.map((n) => n.notification.code)).toEqual(["other"]);
    ui.host(0).notify(given);
    store.apply({ blocks: { rm: ["b"] } });
    expect(notified).toEqual([]);
  });

  test("ends as the frontend calls its cleanup, which aborts the signal, runs the module's cleanup and empties the element", async () => {
    const ui = recorder();
    const { mounts } = await setup({ [HEAD]: ui.exports });
    const el = document.createElement("div");
    const cleanup = mounts.mountBlock(el, "b");
    await tick();

    cleanup();
    cleanup();

    expect(ui.mounts[0]?.signal.aborted).toBe(true);
    expect(ui.cleanups()).toBe(1);
    expect(el.childNodes).toHaveLength(0);
  });

  test("lets a module submit an edit still pending as it unmounts", async () => {
    const pending: BlockModule = {
      render({ host, signal }) {
        signal.addEventListener("abort", () => {
          void host.submit({ n: 7 });
        });
        return undefined;
      },
    };
    const { store, mounts } = await setup({ [HEAD]: { api: 1, default: pending } });
    const cleanup = mounts.mountBlock(document.createElement("div"), "b");
    await tick();

    cleanup();

    expect(store.board().blocks.b?.state.n).toBe(7);
  });

  test("runs at once the cleanup that render() resolves with after the mount ended", async () => {
    let cleaned = false;
    let finish: () => void = () => undefined;
    const late: BlockModule = {
      render: () =>
        new Promise((resolve) => {
          finish = () => {
            resolve(() => {
              cleaned = true;
            });
          };
        }),
    };
    const { mounts } = await setup({ [HEAD]: { api: 1, default: late } });
    const cleanup = mounts.mountBlock(document.createElement("div"), "b");
    await tick();
    cleanup();
    finish();
    await tick();

    expect(cleaned).toBe(true);
  });

  test("ends once another board opens, after which submit() and evaluate() reject and core mounts nothing more", async () => {
    const ui = recorder();
    const s = await setup({ [HEAD]: ui.exports });
    const { store, mounts } = s;
    const el = document.createElement("div");
    mounts.mountBlock(el, "b");
    await tick();
    const host = ui.host();
    const heard: string[] = [];
    host.on("state", () => heard.push("state"));
    host.on("context", () => heard.push("context"));

    await s.next();

    expect(ui.mounts[0]?.signal.aborted).toBe(true);
    expect(ui.cleanups()).toBe(1);
    expect(el.childNodes).toHaveLength(0);
    expect(heard).toEqual([]);
    await expect(host.submit({ n: 2 })).rejects.toThrow(
      'Another board opened, so the host of block "b" no longer changes it.',
    );
    await expect(host.evaluate({ save: true })).rejects.toThrow("Another board opened");
    expect(store.board().blocks.b?.state.n).toBe(6);
    expect(() => mounts.mountBlock(document.createElement("div"), "b")).toThrow(
      "Another board opened, so core mounts nothing more of the board before.",
    );
  });

  test("ends once its block leaves the board, after which submit() rejects, though undo brings the block back", async () => {
    const ui = recorder();
    const { store, mounts } = await setup({ [HEAD]: ui.exports });
    mounts.mountBlock(document.createElement("div"), "b");
    await tick();
    const host = ui.host();

    store.apply({ blocks: { rm: ["b"] } });
    expect(ui.mounts[0]?.signal.aborted).toBe(true);
    store.undo();

    await expect(host.submit({ n: 2 })).rejects.toThrow('Block "b" left the board, so its host no longer changes it.');
    expect(store.board().blocks.b?.state.n).toBe(6);
  });

  test("that the board ends while the module loads never renders", async () => {
    const ui = recorder();
    let load: (exports: unknown) => void = () => undefined;
    const r = playR();
    r.connect();
    const store = createStore();
    for (const option of coreOptions) store.options.register(option);
    await store.open(hello);
    const scheduler = createScheduler({ store, client: r.client });
    const mounts = createMounts({ core: noCore,
      store,
      commands: createCommands(store),
      scheduler,
      loader: createLoader({ importer: () => new Promise((resolve) => (load = resolve)), stylesheet: () => Promise.resolve() }),
    });
    mounts.mountBlock(document.createElement("div"), "b");
    await tick();

    store.apply({ blocks: { rm: ["b"] } });
    load(ui.exports);
    await tick();

    expect(ui.mounts).toHaveLength(0);
  });
});

describe("a request", () => {
  async function mounted(reply: HelloReply = withRequests) {
    const ui = recorder(["n_rows"]);
    const all = await setup({ [HEAD]: ui.exports, [UPLOAD]: ui.exports }, { reply });
    all.mounts.mountBlock(document.createElement("div"), "b");
    await tick();
    return { ...all, ui, host: ui.host() };
  }

  test("has the inputs it reads evaluated first, though nothing demands them, and goes to R at their revisions", async () => {
    const { r, scheduler, host } = await mounted();

    const max = host.request("n_max");
    await tick();
    expect(requests(r)).toHaveLength(0);
    expect(r.pending().map((request) => request.block)).toEqual(["a"]);
    await evaluateAll(r);

    expect(lastRequest(r)).toEqual({
      id: lastRequest(r).id,
      kind: "request",
      type: "block_class",
      registry_id: head.id,
      block: "b",
      name: "n_max",
      args: {},
      reads: [{ input: "data", block: "a", revision: blockRevision(scheduler, "a") }],
    });
    r.reply({ id: lastRequest(r).id, status: "ok", value: [150] });
    await expect(max).resolves.toEqual([150]);
  });

  test("sends the state args its function names, from args or else from the block's state, its parameters, and the board options it names", async () => {
    const { r, host } = await mounted();

    void host.request("columns", { limit: 10 });
    await evaluateAll(r);
    expect(lastRequest(r)).toMatchObject({ args: { direction: "head", limit: 10 }, options: { n_rows: 50 } });

    void host.request("columns", { direction: "tail" });
    await tick();
    expect(lastRequest(r)).toMatchObject({ args: { direction: "tail" }, options: { n_rows: 50 } });
  });

  test("that reads no inputs goes to R at once", async () => {
    const dataset = recorder();
    const { r, mounts } = await setup({ "blockr.core/dataset-block.js?v=5e0c8b1d": dataset.exports });
    mounts.mountBlock(document.createElement("div"), "a");
    await tick();

    void dataset.host().request("datasets");
    await tick();
    expect(lastRequest(r)).toMatchObject({ name: "datasets", args: { package: "datasets" }, reads: [] });
  });

  test("is answered from what core keeps, by class, name, args, board options and revisions, for every block of the class", async () => {
    const { r, store, host, mounts, ui } = await mounted();
    store.apply({
      blocks: { add: { g: { registry_id: head.id, state: { n: 3, direction: "head" }, name: "Head", locked: null } } },
      links: { add: { ag: { from: "a", to: "g", input: "data" } } },
    });
    mounts.mountBlock(document.createElement("div"), "g");
    await tick();

    const first = host.request("n_max");
    await evaluateAll(r);
    r.reply({ id: lastRequest(r).id, status: "ok", value: [150] });
    await expect(first).resolves.toEqual([150]);

    await expect(ui.host(1).request("n_max")).resolves.toEqual([150]);
    await expect(host.request("n_max")).resolves.toEqual([150]);
    expect(requests(r)).toHaveLength(1);

    // The same request with other args goes to R.
    void host.request("columns");
    void host.request("columns", { direction: "tail" });
    await evaluateAll(r);
    expect(requests(r)).toHaveLength(3);
  });

  test("merges with an identical one in flight", async () => {
    const { r, host } = await mounted();

    const one = host.request("n_max");
    const two = host.request("n_max");
    await evaluateAll(r);
    expect(requests(r)).toHaveLength(1);
    r.reply({ id: lastRequest(r).id, status: "ok", value: [150] });

    await expect(Promise.all([one, two])).resolves.toEqual([[150], [150]]);
  });

  test("goes to R again once the revisions it read are superseded", async () => {
    const { r, scheduler, host } = await mounted();
    const first = host.request("n_max");
    await evaluateAll(r);
    r.reply({ id: lastRequest(r).id, status: "ok", value: [150] });
    await first;

    scheduler.invalidate("a");
    const second = host.request("n_max");
    await evaluateAll(r);
    expect(requests(r)).toHaveLength(2);
    expect(lastRequest(r).reads).toEqual([{ input: "data", block: "a", revision: blockRevision(scheduler, "a") }]);
    r.reply({ id: lastRequest(r).id, status: "ok", value: [120] });
    await expect(second).resolves.toEqual([120]);
  });

  test("of a volatile request merges in flight, but nothing of it is kept", async () => {
    const { r, host } = await mounted();

    const one = host.request("listing");
    const two = host.request("listing");
    await tick();
    expect(requests(r)).toHaveLength(1);
    r.reply({ id: lastRequest(r).id, status: "ok", value: ["a.csv"] });
    await expect(Promise.all([one, two])).resolves.toEqual([["a.csv"], ["a.csv"]]);

    void host.request("listing");
    await tick();
    expect(requests(r)).toHaveLength(2);
  });

  test("of an action runs once per call, with the file it sends, and is neither kept nor merged", async () => {
    const json = hello.board;
    const reply: HelloReply = {
      ...hello,
      board: {
        ...json,
        board: {
          ...json.board,
          blocks: {
            ...json.board.blocks,
            u: { registry_id: "blockr.core-upload_block", version: 1, state: {}, name: "Upload", locked: null },
          },
        },
      },
    };
    const ui = recorder();
    const { r, mounts } = await setup({ [UPLOAD]: ui.exports }, { reply });
    mounts.mountBlock(document.createElement("div"), "u");
    await tick();
    const file = new File(["a,b\n1,2\n"], "sales.csv", { type: "text/csv" });

    const one = ui.host().request("upload", {}, { file });
    const two = ui.host().request("upload", {}, { file });
    await tick();

    expect(requests(r).map((request) => request.body)).toEqual([
      { type: "text/csv", name: "sales.csv" },
      { type: "text/csv", name: "sales.csv" },
    ]);
    for (const request of requests(r)) r.reply({ id: request.id, status: "ok", value: ["uploads/sales.csv"] });
    await expect(Promise.all([one, two])).resolves.toEqual([["uploads/sales.csv"], ["uploads/sales.csv"]]);
  });

  test("resolves a download with the File that the reply names", async () => {
    const { r, host } = await mounted();
    const file = new File(["report"], "report.pptx", { type: "application/vnd.ms-powerpoint" });

    const download = host.request("listing");
    await tick();
    r.reply({ id: lastRequest(r).id, status: "ok", body: { type: file.type, name: file.name } }, file);

    await expect(download).resolves.toBe(file);
  });

  test("calls progress for each note R sends", async () => {
    const { r, host } = await mounted();
    const notes: unknown[] = [];

    const listing = host.request("listing", {}, { progress: (note) => notes.push(note) });
    await tick();
    r.note(lastRequest(r).id, { value: 0.5, message: "Listing" });
    r.reply({ id: lastRequest(r).id, status: "ok", value: [] });
    await listing;

    expect(notes).toEqual([{ value: 0.5, message: "Listing" }]);
  });

  test("rejects with the reply's status and conditions", async () => {
    const { r, host } = await mounted();

    const listing = host.request("listing");
    await tick();
    r.reply({ id: lastRequest(r).id, status: "error", conditions: [failure] });

    await expect(listing).rejects.toBeInstanceOf(RequestError);
    await expect(listing).rejects.toMatchObject({
      status: "error",
      conditions: [failure],
      message: `R replied error to the request "listing" of block "b".\n- ${failure.message}`,
    });
  });

  test("hands on each of R's replies as its latest to a request of the block of its name, with the conditions of an error and none of an ok", async () => {
    const { r, host, replies } = await mounted();

    const failing = host.request("listing");
    await tick();
    r.reply({ id: lastRequest(r).id, status: "error", conditions: [failure] });
    await expect(failing).rejects.toBeInstanceOf(RequestError);
    const listing = host.request("listing");
    await tick();
    r.reply({ id: lastRequest(r).id, status: "ok", value: [] });
    await listing;

    expect(replies).toEqual([
      [{ block: "b" }, { type: "block_class", name: "listing" }, [failure]],
      [{ block: "b" }, { type: "block_class", name: "listing" }, []],
    ]);
  });

  test("rejects with the statuses of the inputs that could not be evaluated, and does not go to R", async () => {
    const { r, host } = await mounted();

    const max = host.request("n_max");
    const rejected = expect(max).rejects.toMatchObject({
      status: null,
      inputs: [{ input: "data", status: { status: "failed", conditions: [failure] } }],
      message: 'The request "n_max" of block "b" did not go to R, since an input it reads is not ready: "data" reads failed.',
    });
    await tick();
    for (const request of r.pending()) r.evaluated(request, "error", { conditions: [failure] });
    await tick();

    await rejected;
    expect(requests(r)).toHaveLength(0);
  });

  test("rejects where no link feeds an input it reads", async () => {
    const { store, host } = await mounted();
    store.apply({ links: { rm: ["ab"] } });

    await expect(host.request("n_max")).rejects.toThrow(
      'The request "n_max" of block "b" did not go to R, since no link feeds its input "data".',
    );
  });

  test("asks again where R replies missing, once the inputs are evaluated again", async () => {
    const { r, scheduler, host } = await mounted();
    const max = host.request("n_max");
    await evaluateAll(r);
    const first = lastRequest(r);

    // R lost the result of a, which the scheduler then invalidates.
    r.reply({ id: first.id, status: "missing", missing: [{ block: "a", revision: blockRevision(scheduler, "a") }] });
    await evaluateAll(r);

    expect(requests(r)).toHaveLength(2);
    expect(lastRequest(r).reads[0]?.revision).toBeGreaterThan(first.reads[0]?.revision ?? 0);
    r.reply({ id: lastRequest(r).id, status: "ok", value: [150] });
    await expect(max).resolves.toEqual([150]);
  });

  test("gives up through its signal, and R gets a cancel once no caller that merged with it waits", async () => {
    const { r, host } = await mounted();
    const one = new AbortController();
    const two = new AbortController();

    const first = host.request("listing", {}, { signal: one.signal });
    const second = host.request("listing", {}, { signal: two.signal });
    await tick();
    const sent = lastRequest(r);

    one.abort();
    await expect(first).rejects.toMatchObject({ name: "AbortError" });
    await tick();
    expect(r.cancels()).toEqual([]);

    two.abort(new Error("The user typed on."));
    await expect(second).rejects.toThrow("The user typed on.");
    await tick();
    expect(r.cancels()).toEqual([sent.id]);

    // A request asked anew starts another.
    void host.request("listing");
    await tick();
    expect(requests(r)).toHaveLength(2);
  });

  test("is kept for one opening of a board, as a page that starts with the next board holds none", async () => {
    const dataset = recorder();
    const s = await setup({ "blockr.core/dataset-block.js?v=5e0c8b1d": dataset.exports });
    const { r, mounts } = s;
    mounts.mountBlock(document.createElement("div"), "a");
    await tick();
    const first = dataset.host().request("datasets");
    await tick();
    r.reply({ id: lastRequest(r).id, status: "ok", value: ["iris"] });
    await first;
    await expect(dataset.host().request("datasets")).resolves.toEqual(["iris"]);
    expect(requests(r)).toHaveLength(1);

    const next = await s.next();
    next.mounts.mountBlock(document.createElement("div"), "a");
    await tick();
    void dataset.host(1).request("datasets");
    await tick();

    expect(requests(r)).toHaveLength(2);
  });

  test("stops once another board opens, and asks nothing of the next board, though it holds a block of the same id", async () => {
    const { r, host, next } = await mounted();
    const max = host.request("n_max");
    const stopped = expect(max).rejects.toThrow('Another board opened, so the host of block "b" no longer changes it.');
    await evaluateAll(r);
    const sent = lastRequest(r);

    await next(withRequests);
    expect(r.cancels()).toEqual([sent.id]);
    r.reply({ id: sent.id, status: "cancelled" });
    await stopped;
    await evaluateAll(r);

    expect(requests(r)).toHaveLength(1);
  });

  test("that R answers missing stops once another board opens, rather than asking again on it", async () => {
    const { r, scheduler, host, next } = await mounted();
    const max = host.request("n_max");
    const stopped = expect(max).rejects.toThrow("Another board opened.");
    await evaluateAll(r);
    const sent = lastRequest(r);

    r.reply({ id: sent.id, status: "missing", missing: [{ block: "a", revision: blockRevision(scheduler, "a") }] });
    await tick();
    await next(withRequests);
    await stopped;
    await evaluateAll(r);

    expect(requests(r)).toHaveLength(1);
  });

  test("of a request the class lacks, or of a block that left the board, rejects", async () => {
    const { store, host } = await mounted();

    await expect(host.request("nothing")).rejects.toThrow('The block class "blockr.core-head_block" has no request "nothing".');
    store.apply({ blocks: { rm: ["b"] } });
    await expect(host.request("n_max")).rejects.toThrow('Block "b" left the board');
  });
});
