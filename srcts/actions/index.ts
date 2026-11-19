// Board actions, what a user triggers from a menu, a button or a gesture, with
// core's own, which run commands of any package, each batch as one undo step
// of the user.

export { boardActionsApi, createBoardActions } from "./actions.ts";
export type { BoardAction, BoardActions, BoardActionsParts, CommandCall, Target } from "./actions.ts";
