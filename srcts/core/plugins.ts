// Running the plugins of the open board, as Running a plugin has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#running-a-plugin):
// 1. As the store opens the board, core adds to the board the board options
//    that the class of each of its plugins names in `options`, and those of
//    core's basic frontend where the board holds no frontend, each with its
//    default where the board lacks it. Until the next board opens, it adds
//    again each that a delta removes, so that the board holds the value of
//    every board option that a plugin requires, as it holds those of a
//    mounted module (../mount/block.ts). For a block that serve() serves
//    alone, which core's basic frontend does not draw, core adds none of them
//    for the basic frontend.
// 2. Core draws the board with its frontend, the plugin whose class has
//    `frontend`, or core's basic frontend where the board holds none, once the
//    board options that the frontend's class names are on the board
//    (./start.ts).
// 3. Once the frontend's render() has returned, core calls the `start` of each
//    plugin whose class has one, in the order of the board's plugins, with
//    CoreApi, the plugin's id and a signal, once the board options that the
//    class names are on the board.
// 4. As the next board opens, core aborts each plugin's signal and runs the
//    cleanup that its `start` returned.
// Where a board option that a class names does not go onto the board, as where
// its default fails, nothing that requires it draws or runs: the element of the
// frontend or of the plugin's UI shows why, and core calls no `start` of the
// plugin, which it reports.
// A frontend places each plugin whose class has a `ui` with mountPlugin(),
// which mounts the module in the element that the frontend gives, as
// mountBlock() mounts a block's UI.
// No change adds a plugin to the open board or removes one, so what core runs
// for the plugins ends only as the next board opens.

import { attempt, ModuleError, mountableModule, onAbort, REPLACED, showError } from "../mount/index.ts";
import type { Loader, Mounts } from "../mount/index.ts";
import { inRole } from "../store/index.ts";
import type { PluginClass, Storage, Store } from "../store/index.ts";
import { coreStorage } from "../storage/index.ts";
import type { OptionId, Plugin, PluginClassId, PluginId } from "../types/board.ts";
import type { CoreApi, PluginModule } from "../types/core-api.ts";
import { errorMessage, own, packageFromId, report } from "../util/index.ts";

// Core's basic frontend, which draws every board that holds no frontend. No
// board holds it, and it keeps no state. It requires the severities of the
// conditions that its cards show and the theme that it draws in.
export const BASIC_FRONTEND = {
  frontend: "blockr.core-basic",
  options: ["show_conditions", "dark_mode"],
} as const satisfies Pick<PluginClass, "frontend" | "options">;

// What the render() of a plugin's module gets.
type PluginArgs = Parameters<PluginModule["render"]>[0];

// What a frontend's render() gets: what a plugin's module gets, but for core's
// basic frontend, which no board holds, and so gets no plugin's id.
export type FrontendArgs = Omit<PluginArgs, "plugin"> & { plugin?: PluginId };

// The frontend that draws the open board.
export interface Frontend {
  // The id of its module: its class's `frontend`, or "blockr.core-basic".
  readonly module: string;
  // The plugin that it is, or undefined for core's basic frontend.
  readonly plugin: PluginId | undefined;
  // Resolves with why board options that it requires did not go onto the
  // board, once those that do are on it.
  readonly added: Promise<readonly string[]>;
}

// What core runs for the plugins of the open board: the members of CoreApi
// that reach them, and what starts them, which core calls as the board opens.
export interface RunningPlugins {
  running(): Readonly<Record<PluginId, Plugin>>;
  pluginClass(plugin: PluginId): PluginClass;
  storage(): Storage;
  mountPlugin(el: HTMLElement, plugin: PluginId): () => void;
  // The frontend that draws the open board.
  frontend(): Frontend;
  // Calls the `start` of each plugin whose class has one, as step 3 above has
  // it, and resolves once it has called each, once.
  start(): Promise<void>;
}

export interface RunningDeps {
  readonly store: Store;
  readonly loader: Loader;
  readonly keepOptions: Mounts["keepOptions"];
  // The CoreApi that the plugins get, which core makes with what runPlugins()
  // returns.
  readonly core: () => CoreApi;
  // Aborts as the next board opens, which ends what core runs for the plugins.
  readonly signal?: AbortSignal;
  // Whether the board is a block that serve() serves alone, which no frontend
  // draws.
  readonly alone?: boolean;
}

const NONE: Readonly<Record<PluginId, Plugin>> = Object.freeze({});
const NOTHING_FAILED: Promise<readonly string[]> = Promise.resolve([]);

export function runPlugins(deps: RunningDeps): RunningPlugins {
  const { store, loader, keepOptions } = deps;
  const replaced = deps.signal ?? new AbortController().signal;

  const registeredClass = (id: PluginClassId): PluginClass | undefined => own(store.plugins.registered(), id);

  // Why board options that the class of each plugin names did not go onto the
  // board, by the plugin's id, and why those of core's basic frontend did not,
  // where it draws the board. Each resolves once the board options that do go
  // onto the board are on it.
  const added = new Map<PluginId, Promise<readonly string[]>>();
  let basic = NOTHING_FAILED;
  // What stops keeping the board options on the board, as the next board
  // opens.
  const stops: (() => void)[] = [];

  function keep(ids: readonly OptionId[], by: string): Promise<readonly string[]> {
    if (ids.length === 0) return NOTHING_FAILED;
    const kept = keepOptions(ids, by);
    stops.push(kept.stop);
    return kept.added;
  }

  function opened(plugins: Readonly<Record<PluginId, Plugin>>): void {
    for (const [id, plugin] of Object.entries(plugins)) {
      const def = registeredClass(plugin.class);
      if (def?.options !== undefined) added.set(id, keep(def.options, `the plugin class "${def.id}"`));
    }
    if (deps.alone !== true && inRole(plugins, registeredClass, "frontend").length === 0) {
      basic = keep(BASIC_FRONTEND.options, "core's basic frontend");
    }
  }

  const offStore = store.subscribe((change, board) => {
    if ("opened" in change) opened(board.plugins);
  });
  onAbort(replaced, () => {
    offStore();
    for (const stop of stops.splice(0)) stop();
  });

  // The plugins that run, as running() last gave them, with what it gave them
  // from.
  let memo:
    | {
        readonly plugins: Readonly<Record<PluginId, Plugin>>;
        readonly classes: Readonly<Record<PluginClassId, PluginClass>>;
        readonly running: Readonly<Record<PluginId, Plugin>>;
      }
    | undefined;

  function running(): Readonly<Record<PluginId, Plugin>> {
    if (!store.hasBoard()) return NONE;
    const { plugins } = store.board();
    const classes = store.plugins.registered();
    if (memo?.plugins !== plugins || memo.classes !== classes) {
      const kept = Object.entries(plugins).filter(([, plugin]) => Object.hasOwn(classes, plugin.class));
      memo = {
        plugins,
        classes,
        running: kept.length === Object.keys(plugins).length ? plugins : Object.freeze(Object.fromEntries(kept)),
      };
    }
    return memo.running;
  }

  function pluginClass(plugin: PluginId): PluginClass {
    const found = own(running(), plugin);
    const def = found === undefined ? undefined : registeredClass(found.class);
    if (def === undefined) throw new Error(`The open board runs no plugin "${plugin}".`);
    return def;
  }

  // Core's own storage plugin's, made once something asks for it, since it
  // reaches for the page's document.
  let ownStorage: Storage | undefined;
  function storage(): Storage {
    const [id] = inRole(running(), registeredClass, "storage");
    const given = id === undefined ? undefined : pluginClass(id).storage;
    if (given !== undefined) return given;
    ownStorage ??= coreStorage();
    return ownStorage;
  }

  function frontend(): Frontend {
    const [id] = inRole(running(), registeredClass, "frontend");
    const module = id === undefined ? undefined : pluginClass(id).frontend;
    if (id === undefined || module === undefined) {
      return { module: BASIC_FRONTEND.frontend, plugin: undefined, added: basic };
    }
    return { module, plugin: id, added: added.get(id) ?? NOTHING_FAILED };
  }

  let started = false;
  async function start(): Promise<void> {
    if (started) return;
    started = true;
    const starting = Object.entries(running()).flatMap(([id, plugin]) => {
      const def = registeredClass(plugin.class);
      return def?.start === undefined ? [] : [{ id, def }];
    });
    const failures = await Promise.all(starting.map(({ id }) => added.get(id) ?? NOTHING_FAILED));
    starting.forEach(({ id, def }, i) => {
      if (replaced.aborted) return;
      if ((failures[i] ?? []).length > 0) {
        report(
          new Error(
            `The plugin "${id}" does not start, since board options that its class "${def.id}" requires did not go onto the board.`,
          ),
        );
        return;
      }
      launch(id, def);
    });
  }

  // Calls the plugin's `start`, and as the next board opens aborts its signal
  // and runs the cleanup that `start` returned.
  function launch(id: PluginId, def: PluginClass): void {
    const controller = new AbortController();
    let cleanup: (() => void) | undefined;
    onAbort(replaced, () => {
      controller.abort();
      if (cleanup !== undefined) attempt(cleanup);
    });
    let returned: unknown;
    try {
      returned = def.start?.({ core: deps.core(), plugin: id, signal: controller.signal });
    } catch (error) {
      report(new Error(`The start of the plugin class "${def.id}" failed for the plugin "${id}": ${errorMessage(error)}`, { cause: error }));
      return;
    }
    if (typeof returned !== "function") return;
    const given = returned as () => void;
    if (controller.signal.aborted) attempt(given);
    else cleanup = given;
  }

  function mountPlugin(el: HTMLElement, plugin: PluginId): () => void {
    if (replaced.aborted) throw new Error(REPLACED);
    const def = pluginClass(plugin);
    const { ui } = def;
    if (ui === undefined) {
      throw new Error(`The class "${def.id}" of the plugin "${plugin}" has no ui to mount.`);
    }
    const pkg = packageFromId(ui);

    // The mount ends as its signal aborts, which may happen whenever the
    // loading below awaits.
    const controller = new AbortController();
    const { signal } = controller;
    const ended = (): boolean => signal.aborted;
    let cleanup: (() => void) | undefined;
    // Stops keeping the class's board options on the board, once the module
    // has loaded.
    let stopKeeping: (() => void) | undefined;

    const end = (): void => {
      if (ended()) return;
      offEnd();
      stopKeeping?.();
      controller.abort();
      // The cleanup runs where render() has returned it, and runs at once
      // where render() resolves with it later.
      if (cleanup !== undefined) attempt(cleanup);
      el.replaceChildren();
    };
    const offEnd = onAbort(replaced, end);

    function fail(error: unknown): void {
      if (ended()) return;
      showError(el, errorMessage(error));
      report(error);
    }

    void (async () => {
      let mod: PluginModule;
      try {
        mod = mountableModule<PluginArgs>(await loader.module(ui), ui, pkg);
      } catch (error) {
        fail(error);
        return;
      }
      if (ended()) return;

      // Where a board option that the class names does not go onto the board,
      // the element shows why in place of the module, which core has
      // reported. A module that has drawn ends then.
      const kept = keepOptions(def.options ?? [], `the plugin class "${def.id}"`, (failures) => {
        if (ended()) return;
        end();
        showError(el, failures.join("\n"));
      });
      stopKeeping = kept.stop;
      const failures = await kept.added;
      if (ended()) return;
      if (failures.length > 0) {
        kept.stop();
        showError(el, failures.join("\n"));
        return;
      }

      try {
        const rendered = await mod.render({ el, core: deps.core(), plugin, signal });
        if (typeof rendered !== "function") return;
        if (ended()) attempt(rendered);
        else cleanup = rendered;
      } catch (error) {
        fail(new ModuleError(ui, pkg, `failed in render(): ${errorMessage(error)}`, { cause: error }));
      }
    })();

    return end;
  }

  return { running, pluginClass, storage, mountPlugin, frontend, start };
}
