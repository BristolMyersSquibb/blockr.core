// What the tests of plugins share: the module of a plugin class or of a board
// option, as the package that ships it writes it, the `ui` of a plugin class,
// which keeps what core mounts it with, and why core opens no board where such
// a module fails.

import type { Option, PluginClass } from "../store/index.ts";
import type { PluginClassId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import { packageFromId } from "../util/index.ts";
import { pluginUrl } from "./store.ts";

// The module of a plugin class, whose default export is the class given
// without its id.
export function classModule(def: Omit<PluginClass, "id"> = {}) {
  return { api: 1, default: def };
}

// The module of a board option, whose default export is the board option
// given without its id.
export function optionModule(def: Omit<Option, "id">) {
  return { api: 1, default: def };
}

// The module of each plugin class and board option given, by the id of each,
// as the CoreApi for tests takes them in `modules`.
export function listedModules(classes: readonly PluginClass[] = [], options: readonly Option[] = []) {
  return {
    ...Object.fromEntries(classes.map(({ id, ...def }) => [id, classModule(def)])),
    ...Object.fromEntries(options.map(({ id, ...def }) => [id, optionModule(def)])),
  };
}

// The `ui` of a plugin class, which writes the name of the plugin it draws, as
// the app runs it, and keeps what each render() gets.
export function pluginUi() {
  const renders: { el: HTMLElement; core: CoreApi; plugin: PluginClassId; signal: AbortSignal }[] = [];
  let cleanups = 0;
  return {
    renders,
    cleanups: () => cleanups,
    ui: {
      render(args: { el: HTMLElement; core: CoreApi; plugin: PluginClassId; signal: AbortSignal }) {
        renders.push(args);
        args.el.textContent = args.core.plugins.appPlugins()[args.plugin]?.name ?? "";
        return () => {
          cleanups++;
        };
      },
    },
  };
}

// Why core refuses the plugin class of the id given, for the reason given,
// naming the module that the CoreApi for tests serves for the class, with the
// class's package.
export function refusedClass(id: PluginClassId, why: string): string {
  return `The module ${pluginUrl(id)} of ${packageFromId(id) ?? "app"} defines a plugin class that core refuses: ${why}`;
}

// Why core opens no board where the modules of plugin classes or board options
// fail for the reasons given, one line for each, as the page shows it in place
// of the board.
export function noBoard(...failures: string[]): string {
  const why =
    failures.length === 1
      ? "Core opens no board without this module, since saving the board would lose what it holds for the module's plugin or board option."
      : "Core opens no board without these modules, since saving the board would lose what it holds for their plugins and board options.";
  return [...failures, why].join("\n");
}
