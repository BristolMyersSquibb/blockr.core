// Core's own notifications, which it raises where it fails at something that
// the user asked for or that the board needs, as Notifications has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#notifications).
// Each has the origin "core", and a class that starts with its own, followed by
// those of an error or a warning as R's classes run.

import type { BlockId, OptionId, PluginClassId } from "../types/board.ts";
import type { Notification } from "../types/core-api.ts";

// Raises a notification of the open board, and returns what withdraws it.
export type Raise = (notification: Omit<Notification, "id">) => () => void;

// Raises nothing, for code that runs outside an opening, as in a test.
export const raiseNothing: Raise = () => () => undefined;

// What one of core's notifications concerns, none of these for the board.
export interface Concerns {
  readonly block?: BlockId;
  readonly option?: OptionId;
  readonly plugin?: PluginClassId;
  // The package whose code failed, or null where an id names none, as for a
  // block class defined outside a package.
  readonly package?: string | null;
}

const ERROR = ["blockr_error", "error", "condition"] as const;
const WARNING = ["blockr_warning", "warning", "condition"] as const;

// One of core's notifications, of the class `code`.
export function coreNotification(
  code: string,
  severity: "warning" | "error",
  message: string,
  concerns: Concerns = {},
): Omit<Notification, "id"> {
  return {
    severity,
    class: [code, ...(severity === "error" ? ERROR : WARNING)],
    message,
    origin: "core",
    ...(concerns.block === undefined ? {} : { block: concerns.block }),
    ...(concerns.option === undefined ? {} : { option: concerns.option }),
    ...(concerns.plugin === undefined ? {} : { plugin: concerns.plugin }),
    ...(concerns.package === undefined || concerns.package === null ? {} : { package: concerns.package }),
  };
}
