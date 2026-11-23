// Board actions, what a user triggers from a menu, a button or a gesture, with
// core's own, which run commands of any package, each batch as one undo step
// of the user, their quick actions, the logic that runs every menu of board
// actions by the rules of a quick action, the module that draws those menus,
// which the blockr option board_action_menu names, their advanced actions,
// core's dialog, which shows an advanced action where neither the caller nor
// the frontend shows it, and the roles, which tie the board action that plays
// each to the commands that it runs.

export { boardActionsApi, createBoardActions } from "./actions.ts";
export type { BoardAction, BoardActions, BoardActionsParts, CommandCall, MenuPlace, ShowMenu, Target } from "./actions.ts";
export type { AdvancedModule, Place, PlaceArgs, ShowDialog } from "./advanced.ts";
export { showDialog } from "./dialog.tsx";
export { CORE_MENU, drawWith, MENU_OPTION, menuModule } from "./module.ts";
export type { MenuModule } from "./module.ts";
export { openMenu } from "./quick.ts";
export type { DrivenMenu, MenuParts, MenuStart, OpenMenu, QuickLevel, QuickRow, ShownLevel, ShownRow } from "./quick.ts";
export type { Role } from "./roles.ts";
