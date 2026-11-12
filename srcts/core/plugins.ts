// Running the app's plugins on the open board, as Running a plugin has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#running-a-plugin).
// The app's plugins are those that R names in its reply to `hello`, each under
// the id of its class, and they run on every board that the app opens, as far
// as a package registers their classes:
// 1. As the store opens the board, core adds to the board the board options
//    that the class of each plugin that runs names in `options`, each with its
//    default where the board lacks it. Until the next board opens, it adds
//    again each that a delta removes, so that the board holds the value of
//    every board option that a plugin requires, as it holds those of a
//    mounted module (../mount/block.ts). For a block that serve() serves
//    alone, the app runs no plugin, so core adds none of them.
// 2. Core draws the board with the app's frontend, the plugin that runs whose
//    class has `frontend`, once the board options that its class names are on
//    the board (./start.ts).
// 3. Once the frontend's render() has returned, core calls the `start` of each
//    plugin that runs whose class has one, in the order of the app's plugins,
//    with CoreApi, the id of the plugin's class and a signal, once the board
//    options that the class names are on the board.
// 4. As the next board opens, core aborts each plugin's signal and runs the
//    cleanup that its `start` returned.
// Where a board option that a class names does not go onto the board, as where
// its default fails, nothing that requires it draws or runs: the element of the
// frontend or of the plugin's UI shows why, and core calls no `start` of the
// plugin, which it reports.
// A frontend places each plugin whose class has a `ui` with mountPlugin(),
// which calls the `ui` with the element that the frontend gives, as
// mountBlock() mounts a block's UI.
// No change adds a plugin to the open board or removes one, so what core runs
// for the plugins ends only as the next board opens.

import { attempt, onAbort, REPLACED, showError } from "../mount/index.ts";
import type { Mounts } from "../mount/index.ts";
import type { PluginEntry } from "../protocol/index.ts";
import { inRole } from "../store/index.ts";
import type { PluginClass, Storage, Store } from "../store/index.ts";
import type { OptionId, PluginClassId } from "../types/board.ts";
import type { CoreApi, FrontendModule, Plugin, PluginModule } from "../types/core-api.ts";
import { errorMessage, own, report } from "../util/index.ts";

// What the render() of the `ui` of a plugin's class gets.
export type PluginArgs = Parameters<PluginModule["render"]>[0];

// The frontend that draws the open board.
export interface Frontend {
  // What draws the page, its class's `frontend`.
  readonly frontend: FrontendModule;
  // The id of its class, which names the plugin that it is.
  readonly plugin: PluginClassId;
  // Resolves with why board options that it requires did not go onto the
  // board, once those that do are on it.
  readonly added: Promise<readonly string[]>;
}

// What core runs for the app's plugins on the open board: the members of
// CoreApi that reach them, and what starts them, which core calls as the board
// opens.
export interface RunningPlugins {
  appPlugins(): Readonly<Record<PluginClassId, Plugin>>;
  pluginClass(plugin: PluginClassId): PluginClass;
  storage(): Storage | undefined;
  mountPlugin(el: HTMLElement, plugin: PluginClassId): () => void;
  // The frontend that draws the open board, or undefined where no plugin that
  // runs is one, as for a block that serve() serves alone.
  frontend(): Frontend | undefined;
  // Calls the `start` of each plugin whose class has one, as step 3 above has
  // it, and resolves once it has called each, once.
  start(): Promise<void>;
}

export interface RunningDeps {
  readonly store: Store;
  readonly keepOptions: Mounts["keepOptions"];
  // The CoreApi that the plugins get, which core makes with what runPlugins()
  // returns.
  readonly core: () => CoreApi;
  // The app's plugins, from R's reply to `hello`, by the ids of their classes.
  readonly plugins: Readonly<Record<PluginClassId, PluginEntry>>;
  // Aborts as the next board opens, which ends what core runs for the plugins.
  readonly signal?: AbortSignal;
}

const NONE: Readonly<Record<PluginClassId, Plugin>> = Object.freeze({});
const NOTHING_FAILED: Promise<readonly string[]> = Promise.resolve([]);

export function runPlugins(deps: RunningDeps): RunningPlugins {
  const { store, keepOptions } = deps;
  const replaced = deps.signal ?? new AbortController().signal;

  const registeredClass = (id: PluginClassId): PluginClass | undefined => own(store.plugins.registered(), id);

  // Why board options that the class of each plugin names did not go onto the
  // board, by the id of the plugin's class. Each resolves once the board
  // options that do go onto the board are on it.
  const added = new Map<PluginClassId, Promise<readonly string[]>>();
  // What stops keeping the board options on the board, as the next board
  // opens.
  const stops: (() => void)[] = [];

  function keep(ids: readonly OptionId[], by: string): Promise<readonly string[]> {
    if (ids.length === 0) return NOTHING_FAILED;
    const kept = keepOptions(ids, by);
    stops.push(kept.stop);
    return kept.added;
  }

  function opened(): void {
    for (const id of Object.keys(appPlugins())) {
      const def = registeredClass(id);
      if (def?.options !== undefined) added.set(id, keep(def.options, `the plugin class "${id}"`));
    }
  }

  const offStore = store.subscribe((change) => {
    if ("opened" in change) opened();
  });
  onAbort(replaced, () => {
    offStore();
    for (const stop of stops.splice(0)) stop();
  });

  // The plugins that run, as appPlugins() last gave them, with the classes
  // that it gave them from.
  let memo:
    | {
        readonly classes: Readonly<Record<PluginClassId, PluginClass>>;
        readonly plugins: Readonly<Record<PluginClassId, Plugin>>;
      }
    | undefined;

  // The app's plugins whose classes a package registers, each with its name
  // alone, which run once the store has opened a board, since every package
  // registers its classes before.
  function appPlugins(): Readonly<Record<PluginClassId, Plugin>> {
    if (!store.hasBoard()) return NONE;
    const classes = store.plugins.registered();
    if (memo?.classes !== classes) {
      const registered = Object.entries(deps.plugins).flatMap(([id, { name }]) =>
        Object.hasOwn(classes, id) ? [[id, Object.freeze({ name })] as const] : [],
      );
      memo = { classes, plugins: Object.freeze(Object.fromEntries(registered)) };
    }
    return memo.plugins;
  }

  function pluginClass(plugin: PluginClassId): PluginClass {
    const def = Object.hasOwn(appPlugins(), plugin) ? registeredClass(plugin) : undefined;
    if (def === undefined) throw new Error(`The app runs no plugin "${plugin}" on the open board.`);
    return def;
  }

  function storage(): Storage | undefined {
    const [id] = inRole(appPlugins(), registeredClass, "storage");
    return id === undefined ? undefined : pluginClass(id).storage;
  }

  function frontend(): Frontend | undefined {
    const [id] = inRole(appPlugins(), registeredClass, "frontend");
    const drawn = id === undefined ? undefined : pluginClass(id).frontend;
    if (id === undefined || drawn === undefined) return undefined;
    return { frontend: drawn, plugin: id, added: added.get(id) ?? NOTHING_FAILED };
  }

  let started = false;
  async function start(): Promise<void> {
    if (started) return;
    started = true;
    const starting = Object.keys(appPlugins()).flatMap((id) => {
      const def = registeredClass(id);
      return def?.start === undefined ? [] : [{ id, def }];
    });
    const failures = await Promise.all(starting.map(({ id }) => added.get(id) ?? NOTHING_FAILED));
    starting.forEach(({ id, def }, i) => {
      if (replaced.aborted) return;
      if ((failures[i] ?? []).length > 0) {
        report(
          new Error(
            `The plugin "${id}" does not start, since board options that its class requires did not go onto the board.`,
          ),
        );
        return;
      }
      launch(id, def);
    });
  }

  // Calls the plugin's `start`, and as the next board opens aborts its signal
  // and runs the cleanup that `start` returned.
  function launch(id: PluginClassId, def: PluginClass): void {
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
      report(new Error(`The start of the plugin class "${id}" failed: ${errorMessage(error)}`, { cause: error }));
      return;
    }
    if (typeof returned !== "function") return;
    const given = returned as () => void;
    if (controller.signal.aborted) attempt(given);
    else cleanup = given;
  }

  function mountPlugin(el: HTMLElement, plugin: PluginClassId): () => void {
    if (replaced.aborted) throw new Error(REPLACED);
    const def = pluginClass(plugin);
    const { ui } = def;
    if (ui === undefined) {
      throw new Error(`The class of the plugin "${plugin}" has no ui to mount.`);
    }

    // The mount ends as its signal aborts, which may happen whenever the
    // waiting below awaits.
    const controller = new AbortController();
    const { signal } = controller;
    const ended = (): boolean => signal.aborted;
    let cleanup: (() => void) | undefined;
    // Stops keeping the class's board options on the board.
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
      // Where a board option that the class names does not go onto the board,
      // the element shows why in place of the ui, which core has reported. A
      // ui that has drawn ends then.
      const kept = keepOptions(def.options ?? [], `the plugin class "${plugin}"`, (failures) => {
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
        const rendered = await ui.render({ el, core: deps.core(), plugin, signal });
        if (typeof rendered !== "function") return;
        if (ended()) attempt(rendered);
        else cleanup = rendered;
      } catch (error) {
        fail(new Error(`The ui of the plugin class "${plugin}" failed in render(): ${errorMessage(error)}`, { cause: error }));
      }
    })();

    return end;
  }

  return { appPlugins, pluginClass, storage, mountPlugin, frontend, start };
}
