// The drawers in which core's basic frontend places the plugins of the board,
// as Plugins has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#plugin-classes):
// each plugin whose class has a `ui` gets a drawer of its own, which the
// toolbar opens, and in which core mounts the module with mountPlugin(). The
// frontend reads no member of a plugin class beyond `ui`, so that a plugin
// needs only its `ui` to show under it.
// One drawer shows at a time, beside the board, which the user keeps working
// on. Core unmounts the plugin's UI as its drawer closes.

import { useCallback } from "react";
import type { ReactElement } from "react";
import type { Plugin, PluginId } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { useFrontEnd } from "./context.ts";
import { classes, Mounted } from "./ui.tsx";

// The plugins that the frontend places, in the order of the board's plugins:
// those that run whose class has a `ui`.
export function placed(core: Pick<CoreApi, "plugins">): readonly (readonly [PluginId, Plugin])[] {
  return Object.entries(core.plugins.running()).filter(([id]) => core.plugins.classOf(id).ui !== undefined);
}

// The drawer of the plugin of the id given, whose element has the DOM id
// given, which the button that opens it names.
export function Drawer({
  id,
  plugin,
  domId,
  onClose,
}: {
  id: PluginId;
  plugin: Plugin;
  domId: string;
  onClose: () => void;
}): ReactElement {
  const { core } = useFrontEnd();
  const mount = useCallback((el: HTMLElement) => core.mountPlugin(el, id), [core, id]);
  return (
    <aside
      id={domId}
      aria-label={plugin.name}
      className={`${classes.card} bc:grid bc:min-w-0 bc:content-start bc:gap-3 bc:overflow-auto bc:rounded-lg bc:border bc:border-border-default bc:p-3 bc:md:sticky bc:md:top-4 bc:md:max-h-[calc(100dvh-2rem)] bc:md:w-md bc:md:shrink-0`}
    >
      <div className="bc:flex bc:items-center bc:justify-between bc:gap-2">
        <h2 className={classes.heading}>{plugin.name}</h2>
        <button type="button" className={classes.button} onClick={onClose}>
          Close
        </button>
      </div>
      <Mounted mount={mount} className="bc:min-w-0" />
    </aside>
  );
}
