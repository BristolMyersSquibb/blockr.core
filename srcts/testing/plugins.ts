// What the tests of plugins share: a startup module that registers plugin
// classes and board options, and the UI module of a plugin class, which keeps
// what core mounts it with.

import type { Option, PluginClass } from "../store/index.ts";
import type { PluginClassId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";

// A startup module that registers the plugin classes and the board options
// given, as the package that ships them does.
export function registering(classes: readonly PluginClass[], options: readonly Option[] = []) {
  return {
    api: 1,
    default: {
      initialize({ core }: { core: CoreApi }) {
        for (const option of options) core.options.register(option);
        for (const def of classes) core.plugins.register(def);
      },
    },
  };
}

// The UI module of a plugin class, which writes the name of the plugin it
// draws, as the app runs it, and keeps what each render() gets.
export function pluginUi() {
  const renders: { el: HTMLElement; core: CoreApi; plugin: PluginClassId; signal: AbortSignal }[] = [];
  let cleanups = 0;
  return {
    renders,
    cleanups: () => cleanups,
    exports: {
      api: 1,
      default: {
        render(args: { el: HTMLElement; core: CoreApi; plugin: PluginClassId; signal: AbortSignal }) {
          renders.push(args);
          args.el.textContent = args.core.plugins.appPlugins()[args.plugin]?.name ?? "";
          return () => {
            cleanups++;
          };
        },
      },
    },
  };
}
