// Board actions, what a user triggers from a menu, a button or a gesture, with
// core's own, which run commands of any package, each batch as one undo step
// of the user, their quick actions, the logic that runs every menu of board
// actions by the rules of a quick action, and core's menu, which draws them.

export { boardActionsApi, createBoardActions } from "./actions.ts";
export type { BoardAction, BoardActions, BoardActionsParts, CommandCall, MenuPlace, ShowMenu, Target } from "./actions.ts";
export { showMenu } from "./menu.tsx";
export type { OpenMenu, QuickLevel, QuickRow, ShownLevel, ShownRow } from "./quick.ts";
