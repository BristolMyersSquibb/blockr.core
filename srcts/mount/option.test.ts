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
import { logged } from "../testing/logged.ts";
import { coreOptions, hello, noCore } from "../testing/store.ts";
import type { Notification, Raised, SourceModule } from "../types/core-api.ts";
import type { OptionHost, OptionModule } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { Replied } from "../util/index.ts";
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
  // What the mounts handed on of each of R's replies to a request.
  replies: Parameters<Replied>[];
  // The notifications that modules raised through their hosts, while they
  // hold.
  notified: { module: SourceModule; notification: Raised }[];
  // Each of core's notifications that the mounts raised, while it holds.
  raised: Omit<Notification, "id">[];
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
  const replies: Parameters<Replied>[] = [];
  const notified: { module: SourceModule; notification: Raised }[] = [];
  const raised: Omit<Notification, "id">[] = [];
  const mounts = createMounts({
    core: noCore,
    store,
    commands,
    scheduler,
    loader,
    renderers: () => renderers,
    signal,
    raise: (notification) => {
      raised.push(notification);
      return () => {
        const at = raised.indexOf(notification);
        if (at >= 0) raised.splice(at, 1);
      };
    },
    replied: (...args) => {
      replies.push(args);
    },
    notify: (module, notification) => {
      const given = { module, notification };
      notified.push(given);
      return () => {
        const at = notified.indexOf(given);
        if (at >= 0) notified.splice(at, 1);
      };
    },
    optionPackage: (option) => (option === "n_rows" ? "blockr.core" : undefined),
  });
  return {
    r,
    store,
    mounts,
    imported,
    replies,
    notified,
    raised,
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
// which core's basic frontend does not require.
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

  test("adds the board option to the board with its default where the board lacks it, and again once a delta removes it", async () => {
    const calls = { count: 0 };
    const dark = ui();
    const s = await setup(options(ui().mod, calls, dark.mod));
    s.mounts.mountOption(document.createElement("div"), "theme");
    s.mounts.mountOption(document.createElement("div"), "theme");
    await tick();

    // The mounts that add it at once share one call of the default.
    expect(dark.mounts.map(({ host }) => host.value())).toEqual(["light", "light"]);
    expect(s.store.board().options).toMatchObject({ theme: "light" });
    expect(calls.count).toBe(1);

    await dark.host().submit("dark");
    expect(s.store.board().options).toMatchObject({ theme: "dark" });
    // The submit is no undo step, since undo leaves board option values as
    // they are, and neither is what core adds.
    expect(s.store.canUndo()).toBe(false);
    expect(s.store.canUndo("blockr.core")).toBe(false);

    // Core keeps no default, so it calls the default again, and the host
    // keeps the value it gave meanwhile.
    s.store.apply({ options: { unset: ["theme"] } });
    await tick();
    expect(s.store.board().options).toMatchObject({ theme: "light" });
    expect(dark.mounts.map(({ seen }) => seen)).toEqual([
      ["light", "dark", "light"],
      ["light", "dark", "light"],
    ]);
    expect(calls.count).toBe(2);
  });

  test("leaves its element empty, and renders nothing, where the board lacks the value and the default fails, which the board option's notification says", async () => {
    const reported = logged();
    const broken = ui();
    const s = await setup([
      ...options(ui().mod),
      {
        id: "broken",
        label: "Broken",
        category: "Test options",
        schema: { type: "number" },
        default: () => Promise.reject(new Error("No value today.")),
        ui: broken.mod,
      },
    ]);
    const el = document.createElement("div");
    s.mounts.mountOption(el, "broken");
    await tick();
    reported.restore();

    const why = 'The default of the board option "broken", which the UI of the board option "broken" requires, failed: No value today.';
    expect(el.textContent).toBe("");
    expect(s.raised).toEqual([
      {
        severity: "error",
        message: why,
        source: { kind: "core", code: "board_option_failed", module: { object: { option: "broken" } } },
        about: { option: "broken" },
      },
    ]);
    expect(reported.errors().map((line) => line.message)).toEqual([why]);
    expect(broken.mounts).toHaveLength(0);
    expect(Object.hasOwn(s.store.board().options, "broken")).toBe(false);
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
    const warning = { severity: "warning" as const, class: ["simpleWarning", "warning", "condition"], phase: "request" as const, message: "NAs introduced by coercion" };
    s.r.reply({ id: sent[0]?.id ?? "", status: "ok", value: ["x"], conditions: [warning] });
    s.r.reply({
      id: sent[1]?.id ?? "",
      status: "error",
      conditions: [{ severity: "error", class: ["simpleError", "error", "condition"], phase: "request", message: "gone" }],
    });
    await expect(first).resolves.toEqual(["x"]);
    await expect(second).rejects.toBeInstanceOf(RequestError);
    // Each reply goes on as R's latest to a request of its name.
    expect(s.replies).toEqual([
      [{ object: { option: "n_rows" }, package: "blockr.core" }, { type: "option", name: "list_dir" }, [warning]],
      [
        { object: { option: "n_rows" }, package: "blockr.core" },
        { type: "option", name: "list_dir" },
        [{ severity: "error", class: ["simpleError", "error", "condition"], phase: "request", message: "gone" }],
      ],
    ]);
  });

  test("raises what the module gives its notify(), naming the board option's module, with the package that lists it, until the mount ends", async () => {
    const rows = ui();
    const s = await setup(options(rows.mod));
    const unmount = s.mounts.mountOption(document.createElement("div"), "n_rows");
    await tick();
    const given: Raised = { severity: "warning", code: "large", message: "Many rows slow the table.", about: null };
    const withdraw = rows.host().notify(given);
    rows.host().notify({ ...given, code: "other" });
    expect(s.notified).toEqual([
      { module: { object: { option: "n_rows" }, package: "blockr.core" }, notification: given },
      { module: { object: { option: "n_rows" }, package: "blockr.core" }, notification: { ...given, code: "other" } },
    ]);
    withdraw();
    withdraw();
    expect(s.notified.map((n) => n.notification.code)).toEqual(["other"]);
    expect(() => rows.host().notify({ ...given, severity: "fatal" } as unknown as Raised)).toThrow(
      'The severity of a notification is "message", "warning" or "error", not "fatal".',
    );

    unmount();
    expect(s.notified).toEqual([]);
    rows.host().notify(given);
    expect(s.notified).toEqual([]);
  });

  test("logs what the module gives its host's log, naming the package that lists the board option", async () => {
    const rows = ui();
    const s = await setup(options(rows.mod));
    s.mounts.mountOption(document.createElement("div"), "n_rows");
    await tick();
    const reported = logged();
    rows.host().log.debug("Paged.", { n: 10 });
    reported.restore();
    expect(reported.lines().map(({ level, package: pkg, message, attributes }) => ({ level, package: pkg, message, attributes }))).toEqual([
      { level: "debug", package: "blockr.core", message: "Paged.", attributes: { n: 10 } },
    ]);
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

  test("names the renderer that it draws in what the renderer raises and in R's replies to its requests", async () => {
    const dark = ui(["n_rows"]);
    const s = await setup(options(ui().mod, { count: 0 }), {
      modules: { [DARK]: dark.exports },
      renderers: { "blockr.test-dark": { module: DARK, option: "theme" } },
    });
    s.mounts.mountOption(document.createElement("div"), "theme");
    await tick();
    const given: Raised = { severity: "message", code: "dimmed", message: "Dimmed the page." };
    dark.host().notify(given);
    const listing = dark.host().request("palettes");
    await tick();
    s.r.reply({ id: s.r.asked("request")[0]?.id ?? "", status: "ok", value: [], conditions: [] });
    await listing;

    const module = { object: { option: "theme" }, renderer: "blockr.test-dark", package: "blockr.test" };
    expect(s.notified).toEqual([{ module, notification: given }]);
    expect(s.replies).toEqual([[module, { type: "option", name: "palettes" }, []]]);
  });

  test("adds no other board option for its module, which reads its own value alone", async () => {
    // A module in plain JS may still name board options, which core ignores.
    const dark = ui(["n_rows"]);
    const s = await setup(options(ui().mod, { count: 0 }, dark.mod), {
      reply: { ...hello, board: { ...hello.board, board: { ...hello.board.board, options: {} } } },
    });
    s.mounts.mountOption(document.createElement("div"), "theme");
    await tick();

    // The board holds the value of theme alone.
    expect(s.store.board().options).toEqual({ theme: "light" });
  });

  test("ends its UI and leaves its element empty once a delta removes the value and the default does not go onto the board again, which the board option's notification says", async () => {
    const reported = logged();
    const shown = ui();
    let calls = 0;
    const once: Option = {
      id: "once",
      label: "Once",
      category: "Test options",
      schema: { type: "number" },
      default: () => (++calls === 1 ? Promise.resolve(1) : Promise.reject(new Error("No value any more."))),
      ui: shown.mod,
    };
    const s = await setup([...options(ui().mod), once]);
    const el = document.createElement("div");
    s.mounts.mountOption(el, "once");
    await tick();
    expect(shown.host().value()).toBe(1);

    s.store.apply({ options: { unset: ["once"] } });
    await tick();
    reported.restore();

    const why = 'The default of the board option "once", which the UI of the board option "once" requires, failed: No value any more.';
    expect(el.textContent).toBe("");
    expect(s.raised.map((n) => [n.source, n.about, n.message])).toEqual([
      [{ kind: "core", code: "board_option_failed", module: { object: { option: "once" } } }, { option: "once" }, why],
    ]);
    expect(shown.cleanups()).toBe(1);
    expect(reported.errors().map((line) => line.message)).toEqual([why]);
  });

  test("leaves its element empty, and raises why, naming the renderer, where the renderer cannot be used", async () => {
    const reported = logged();
    const s = await setup(options(ui().mod), {
      modules: { [DARK]: { api: 2, default: ui().mod } },
      renderers: { "blockr.test-dark": { module: DARK, option: "theme" } },
    });
    const el = document.createElement("div");
    s.mounts.mountOption(el, "theme");
    await tick();
    reported.restore();

    expect(el.textContent).toBe("");
    expect(s.raised.map((n) => [n.source, n.about])).toEqual([
      [
        {
          kind: "core",
          code: "module_failed",
          module: { object: { option: "theme" }, renderer: "blockr.test-dark", package: "blockr.test" },
        },
        { option: "theme" },
      ],
    ]);
    expect(s.raised[0]?.message).toContain("The module blockr.test/js/dark.js of blockr.test was built against major 2");
    expect(reported.errors().filter((line) => line.error instanceof ModuleError)).toHaveLength(1);
  });

  test("names the board option where the UI of its definition fails in render()", async () => {
    const failing: OptionModule = {
      render() {
        throw new Error("boom");
      },
    };
    const reported = logged();
    const s = await setup(options(failing));
    const el = document.createElement("div");
    s.mounts.mountOption(el, "n_rows");
    await tick();
    reported.restore();

    const message = 'The UI of the board option "n_rows" failed in render(): boom';
    expect(el.textContent).toBe("");
    expect(s.raised).toEqual([
      {
        severity: "error",
        message,
        source: { kind: "core", code: "module_failed", module: { object: { option: "n_rows" }, package: "blockr.core" } },
        about: { option: "n_rows" },
      },
    ]);
    expect(reported.errors().map((line) => line.message)).toEqual([message]);
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
