// The storage controls that core's basic frontend draws at the end of its
// toolbar and binds to their shortcuts, as the storage of the app's storage
// plugin gives them
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#cores-basic-frontend).
// The frontend reads them afresh as it draws the toolbar and as the user
// presses a key, so that it draws and runs what controls() gives at that
// moment:
// 1. It calls controls() with its CoreApi.
// 2. Of the controls of one list that share an id, it keeps the first, since
//    it keys each control by its id.
// 3. It leaves out a control of a kind that it does not know.
// 4. It puts the controls that ask for the end of the bar after the others,
//    and ignores the hints of the controls inside a menu.
// Where controls() throws, or gives what the frontend cannot read, no control
// draws, and the toolbar shows a warning in their place. The dock's navbar
// does the same with ids that two controls share and with controls() that
// throws (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.5#notes-for-implementers).

import type { StorageControl } from "../../store/index.ts";
import type { PluginClassId } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import { errorMessage, quoted } from "../../util/index.ts";
import type { FrontEnd } from "./context.ts";

export type Button = Extract<StorageControl, { kind: "button" }>;

// The storage controls as the toolbar draws them.
export interface Drawn {
  // The controls, in the order in which the toolbar draws them.
  readonly controls: readonly StorageControl[];
  // Whether controls() failed, so that no control draws.
  readonly failed: boolean;
  // What the page hears of, where something went wrong: why controls()
  // failed, or which ids two controls of one list share.
  readonly problem?: string;
}

const KINDS: ReadonlySet<string> = new Set(["button", "menu", "status"]);

// The storage controls of the app's storage plugin as the toolbar draws them,
// or undefined where the app runs no storage plugin.
export function drawnControls(core: CoreApi): Drawn | undefined {
  const storage = core.plugins.storage();
  if (storage === undefined) return undefined;
  try {
    const twice = new Set<string>();
    const controls = distinct(storage.controls({ core }), twice);
    return {
      controls: [...controls.filter((c) => c.placement !== "end"), ...controls.filter((c) => c.placement === "end")],
      failed: false,
      ...(twice.size === 0
        ? {}
        : { problem: `Storage controls of one list share the ids ${quoted([...twice])}, so only the first of each draws.` }),
    };
  } catch (error) {
    return { controls: [], failed: true, problem: `The storage controls failed: ${errorMessage(error)}` };
  }
}

// The buttons among the controls, those inside menus too, in the order in which
// the toolbar draws them.
export function buttonsOf(controls: readonly StorageControl[]): Button[] {
  return controls.flatMap((control) => {
    if (control.kind === "button") return [control];
    if (control.kind === "menu") return buttonsOf(control.items);
    return [];
  });
}

// Runs the button. It calls run() before it awaits anything, so that run()
// still acts within the user's click or key press, as a browser's file picker
// needs. Where run() rejects, the frontend tells the user why through a
// notification, as Plugins has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#storage):
// an error of the code `storage_control_failed`, which concerns the app's
// storage plugin, whose button it is, so that its toast names that plugin.
export async function runButton(front: Pick<FrontEnd, "core" | "plugin">, button: Button): Promise<void> {
  try {
    await button.run();
  } catch (error) {
    const storage = storagePlugin(front.core);
    front.core.notifications.notify(front.plugin, {
      severity: "error",
      code: "storage_control_failed",
      message: errorMessage(error),
      ...(storage === undefined ? {} : { about: { plugin: storage } }),
    });
  }
}

// The id of the class of the app's storage plugin, whose storage
// `core.plugins.storage()` gives, or undefined where the app runs none.
function storagePlugin(core: Pick<CoreApi, "plugins">): PluginClassId | undefined {
  const storage = core.plugins.storage();
  if (storage === undefined) return undefined;
  return Object.keys(core.plugins.appPlugins()).find((id) => core.plugins.pluginClass(id).storage === storage);
}

// The controls of a list that the toolbar draws: the first of those that share
// an id, of the kinds that it knows, and each menu with the same of its items.
// It adds to `twice` each id that it finds again.
function distinct(list: readonly StorageControl[], twice: Set<string>): StorageControl[] {
  const seen = new Set<string>();
  const kept: StorageControl[] = [];
  for (const control of list) {
    if (!KINDS.has(control.kind)) continue;
    if (seen.has(control.id)) {
      twice.add(control.id);
      continue;
    }
    seen.add(control.id);
    kept.push(control.kind === "menu" ? { ...control, items: distinct(control.items, twice) } : control);
  }
  return kept;
}
