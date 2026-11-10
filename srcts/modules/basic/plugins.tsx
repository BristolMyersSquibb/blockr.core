// The plugins that core's basic frontend shows, as Plugins has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#plugin-classes):
// each plugin whose class has a `ui` gets a section of its own, in which core
// mounts the module with mountPlugin(), as the frontend shows every block. The
// frontend reads no member of a plugin class beyond `ui`, so that a plugin
// needs only its `ui` to show under it.
// The user collapses and expands each section, which starts open. A collapsed
// section hides the plugin's UI and leaves it mounted, so that the UI keeps
// what it holds outside the plugin's state. Which sections are collapsed is
// the page's, which nothing saves or undoes.

import { useCallback } from "react";
import type { ReactElement } from "react";
import type { Plugin, PluginId } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { useFrontEnd } from "./context.ts";
import { classes, Mounted } from "./ui.tsx";

// The plugins that the frontend shows, in the order of the board's plugins:
// those that run whose class has a `ui`.
export function placed(core: Pick<CoreApi, "plugins">): readonly (readonly [PluginId, Plugin])[] {
  return Object.entries(core.plugins.running()).filter(([id]) => core.plugins.pluginClass(id).ui !== undefined);
}

// The section of each plugin that the frontend shows, beside the board where
// the page is wide enough, in a column that takes a third of the page's width
// and no less than 28rem, staying in view as the board scrolls, and above it
// otherwise. It draws nothing where the frontend shows no plugin.
export function Plugins(): ReactElement | null {
  const { core } = useFrontEnd();
  const shown = placed(core);
  if (shown.length === 0) return null;
  return (
    <aside
      aria-label="Plugins"
      className="bc:grid bc:min-w-0 bc:content-start bc:gap-4 bc:md:sticky bc:md:top-4 bc:md:max-h-[calc(100dvh-2rem)] bc:md:w-1/3 bc:md:min-w-md bc:md:shrink-0 bc:md:overflow-auto"
    >
      {shown.map(([id, plugin]) => (
        <PluginSection key={id} id={id} plugin={plugin} />
      ))}
    </aside>
  );
}

function PluginSection({ id, plugin }: { id: PluginId; plugin: Plugin }): ReactElement {
  const { core } = useFrontEnd();
  const mount = useCallback((el: HTMLElement) => core.mountPlugin(el, id), [core, id]);
  return (
    <details open className={`${classes.card} bc:min-w-0 bc:rounded-lg bc:border bc:border-border-default bc:p-3`}>
      <summary className={`${classes.heading} bc:cursor-pointer`}>{plugin.name}</summary>
      <Mounted mount={mount} className="bc:mt-3 bc:min-w-0" />
    </details>
  );
}
