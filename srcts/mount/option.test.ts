// Mounting a board option's UI, or the renderer that the app names for it, on
// the sample board of the wire protocol's fixtures, which holds n_rows at 50,
// with R played over the protocol's own client.

import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { createCommands } from "../commands/index.ts";
import type { HelloReply, Renderer } from "../protocol/index.ts";
import { createScheduler } from "../scheduler/index.ts";
import { createStore } from "../store/index.ts";
import type { Option, Store } from "../store/index.ts";
import { playR } from "../testing/r.ts";
import type { PlayedR } from "../testing/r.ts";
import { reports } from "../testing/reports.ts";
import { coreOptions, hello } from "../testing/store.ts";
import type { OptionHost, OptionModule } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import { createLoader, createMounts, ModuleError, RequestError } from "./index.ts";
import type { Mounts } from "./index.ts";

const DARK = "blockr.test/js/dark.js?v=1";

// A UI that keeps each host it is mounted with, and each value its host held
// as it announced one.
function ui(named?: readonly string[]) {
  const mounts: { host: OptionHost; el: HTMLElement; signal: AbortSignal; seen: Json[] }[] = [];
  let cleanups = 0;
  const mod: OptionModule = {
    ...(named === undefined ? {} : { options: named }),
    render({ el, host, signal }) {
      const mount = { host, el, signal, seen: [host.value()] };
      mounts.push(mount);
      el.textContent = JSON.stringify(host.value());
      const off = host.on("value", () => {
        mount.seen.push(host.value());
        el.textContent = JSON.stringify(host.value());
      });
      return () => {
        cleanups++;
        off();
      };
    },
  };
  return {
    mod,
    exports: { api: 1, default: mod },
    mounts,
    cleanups: () => cleanups,
    host(i = 0): OptionHost {
      const mount = mounts[i];
      if (mount === undefined) throw new Error(`The UI has no mount ${String(i)}.`);
      return mount.host;
    },
  };
}

interface Setup {
  r: PlayedR;
  store: Store;
  mounts: Mounts;
  imported: string[];
  // Ends what core runs for the board, as core does as another board opens.
  end(): void;
}

async function setup(
  options: readonly Option[],
  { modules = {}, renderers = {}, reply = hello }: { modules?: Readonly<Record<string, unknown>>; renderers?: Readonly<Record<string, Renderer>>; reply?: HelloReply } = {},
): Promise<Setup> {
  const r = playR();
  r.connect();
  const controller = new AbortController();
  const { signal } = controller;
  const store = createStore({ signal });
  for (const option of options) store.options.register(option);
  await store.open(reply);
  const commands = createCommands(store, { signal });
  const scheduler = createScheduler({ store, client: r.client, signal });
  scheduler.start();
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
  const mounts = createMounts({ store, commands, scheduler, loader, renderers: () => renderers, signal });
  return {
    r,
    store,
    mounts,
    imported,
    end: () => {
      controller.abort();
    },
  };
}

async function tick(ms = 0): Promise<void> {
  await vi.advanceTimersByTimeAsync(ms);
}

// The board options n_rows, show_conditions and dark_mode of core, with the UI
// given for n_rows, and a board option theme whose default is a function,
// which core's basic front-end does not require.
function options(nRows: OptionModule, calls: { count: number } = { count: 0 }, dark: OptionModule = nRows): Option[] {
  const [rows, conditions, darkMode] = coreOptions;
  if (rows === undefined || conditions === undefined || darkMode === undefined) {
    throw new Error("The fixtures lack core's board options.");
  }
  return [
    { ...rows, ui: nRows },
    conditions,
    darkMode,
    {
      id: "theme",
      label: "Theme",
      category: "Theme options",
      schema: { enum: ["light", "dark"] },
      default: () => {
        calls.count++;
        return Promise.resolve("light");
      },
      ui: dark,
    },
  ];
}

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
});

describe("the host", () => {
  test("gives the value that the board holds, and announces each change of it", async () => {
    const rows = ui();
    const s = await setup(options(rows.mod));
    const el = document.createElement("div");
    s.mounts.mountOption(el, "n_rows");
    await tick();

    expect(rows.host().option).toBe("n_rows");
    expect(rows.host().value()).toBe(50);
    expect(el.textContent).toBe("50");

    s.store.apply({ options: { set: { n_rows: 25 } } });
    s.store.apply({ options: { set: { n_rows: 50 } } });
    expect(rows.mounts[0]?.seen).toEqual([50, 25, 50]);
  });

  test("gives the default for this opening where the board lacks the value, which core resolves once per opening", async () => {
    const calls = { count: 0 };
    const dark = ui();
    const s = await setup(options(ui().mod, calls, dark.mod));
    s.mounts.mountOption(document.createElement("div"), "theme");
    s.mounts.mountOption(document.createElement("div"), "theme");
    await tick();

    expect(dark.mounts.map(({ host }) => host.value())).toEqual(["light", "light"]);
    expect(calls.count).toBe(1);
    // The board takes the value only once something sets it.
    expect(Object.hasOwn(s.store.board().options, "theme")).toBe(false);

    await dark.host().submit("dark");
    expect(s.store.board().options).toMatchObject({ theme: "dark" });
    expect(dark.mounts.map(({ seen }) => seen)).toEqual([
      ["light", "dark"],
      ["light", "dark"],
    ]);
    // The submit is no undo step, since undo leaves board option values as
    // they are.
    expect(s.store.canUndo()).toBe(false);
    // Removing the value gives the default again, which core does not resolve
    // again.
    s.store.apply({ options: { unset: ["theme"] } });
    expect(dark.host().value()).toBe("light");
    expect(calls.count).toBe(1);
  });

  test("rejects a submit with what refused it", async () => {
    const rows = ui();
    const s = await setup(options(rows.mod));
    s.mounts.mountOption(document.createElement("div"), "n_rows");
    await tick();

    await expect(rows.host().submit(0)).rejects.toThrow();
    expect(s.store.board().options).toMatchObject({ n_rows: 50 });
  });

  test("sends the board option's requests to R, whose answers core never keeps", async () => {
    const rows = ui();
    const s = await setup(options(rows.mod));
    s.mounts.mountOption(document.createElement("div"), "n_rows");
    await tick();

    const first = rows.host().request("list_dir", { path: "a" });
    const second = rows.host().request("list_dir", { path: "a" });
    await tick();
    const sent = s.r.asked("request");
    expect(sent).toMatchObject([
      { kind: "request", type: "option", option: "n_rows", name: "list_dir", args: { path: "a" } },
      { kind: "request", type: "option", option: "n_rows", name: "list_dir", args: { path: "a" } },
    ]);
    s.r.reply({ id: sent[0]?.id ?? "", status: "ok", value: ["x"] });
    s.r.reply({
      id: sent[1]?.id ?? "",
      status: "error",
      conditions: [{ severity: "error", class: ["simpleError", "error", "condition"], phase: "request", message: "gone" }],
      omitted: { warning: 0, message: 0 },
    });
    await expect(first).resolves.toEqual(["x"]);
    await expect(second).rejects.toBeInstanceOf(RequestError);
  });
});

describe("a mount", () => {
  test("draws the renderer that the app names for the board option in place of its UI", async () => {
    const own = ui();
    const dark = ui(["n_rows"]);
    const s = await setup(options(ui().mod, { count: 0 }, own.mod), {
      modules: { [DARK]: dark.exports },
      renderers: { "blockr.test-dark": { module: DARK, option: "theme" } },
    });
    s.mounts.mountOption(document.createElement("div"), "theme");
    await tick();

    expect(s.imported).toEqual([DARK]);
    expect(own.mounts).toHaveLength(0);
    expect(dark.host().value()).toBe("light");
  });

  test("adds the board options that its module names to the board", async () => {
    const dark = ui(["n_rows"]);
    const s = await setup(options(ui().mod, { count: 0 }, dark.mod), {
      reply: { ...hello, board: { ...hello.board, board: { ...hello.board.board, options: {} } } },
    });
    s.mounts.mountOption(document.createElement("div"), "theme");
    await tick();

    // Beside those that core's basic front-end requires.
    expect(s.store.board().options).toEqual({ show_conditions: ["warning", "error"], dark_mode: "light", n_rows: 50 });
  });

  test("shows an error where the renderer cannot be used", async () => {
    const reported = reports();
    const s = await setup(options(ui().mod), {
      modules: { [DARK]: { api: 2, default: ui().mod } },
      renderers: { "blockr.test-dark": { module: DARK, option: "theme" } },
    });
    const el = document.createElement("div");
    s.mounts.mountOption(el, "theme");
    await tick();
    reported.restore();

    expect(el.textContent).toContain("The module blockr.test/js/dark.js of blockr.test was built against major 2");
    expect(reported.errors().filter((error) => error instanceof ModuleError)).toHaveLength(1);
  });

  test("ends once another board opens, after which submit() rejects and core mounts nothing more", async () => {
    const rows = ui();
    const s = await setup(options(rows.mod));
    const el = document.createElement("div");
    s.mounts.mountOption(el, "n_rows");
    await tick();
    const host = rows.host();

    s.end();
    expect(rows.cleanups()).toBe(1);
    expect(rows.mounts[0]?.signal.aborted).toBe(true);
    expect(el.textContent).toBe("");
    await expect(host.submit(10)).rejects.toThrow("Another board opened");
    expect(() => s.mounts.mountOption(document.createElement("div"), "n_rows")).toThrow(
      "Another board opened, so core mounts nothing more of the board before.",
    );
  });

  test("ends as its cleanup runs, and throws for a board option that no package registers", async () => {
    const rows = ui();
    const s = await setup(options(rows.mod));
    const unmount = s.mounts.mountOption(document.createElement("div"), "n_rows");
    await tick();
    unmount();
    expect(rows.cleanups()).toBe(1);

    expect(() => s.mounts.mountOption(document.createElement("div"), "zoom")).toThrow(
      'No package registers the board option "zoom" whose UI to mount.',
    );
  });
});
