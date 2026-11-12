// What the tests of plugins share: the module of a plugin class or of a board
// option, as the package that ships it writes it, and the `ui` of a plugin
// class, which keeps what core mounts it with.

import type { Option, PluginClass } from "../store/index.ts";
import type { PluginClassId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";

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
