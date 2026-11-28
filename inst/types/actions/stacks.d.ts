import type { BoardAction } from "./actions.ts";
export declare function createStackAction(): Omit<BoardAction, "package">;
export declare function editStackAction(): Omit<BoardAction, "package">;
export declare function addToStackAction(): Omit<BoardAction, "package">;
