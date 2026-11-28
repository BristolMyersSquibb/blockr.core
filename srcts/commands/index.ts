// The command registry, through which menus, shortcuts, plugins and the
// assistant's tools run every edit, with core's commands, and the schemas
// that core gives what every block, link and stack shares.

export { CommandError, createCommands, isLogged } from "./commands.ts";
export type { Command, CommandCall, CommandContext, Commands } from "./commands.ts";
export { CORE, freeInputs, linkFeeding, suggestedName, takesLink } from "./core.ts";
export * as schemas from "./schemas.ts";
