// The command registry, through which menus, shortcuts, extensions and the
// assistant's tools run every edit, with core's commands, and the schemas
// that core gives what every block, link and stack shares.

export { CommandError, createCommands } from "./commands.ts";
export type { Command, Commands } from "./commands.ts";
export { blockName, freeInputs, pluginName } from "./core.ts";
export * as schemas from "./schemas.ts";
export { stateSchema } from "./schemas.ts";
