import type { BoardAction } from "./actions.ts";
import type { CoreParts } from "./core.ts";
export declare function createLinkAction(parts: CoreParts): Omit<BoardAction, "package">;
export declare function editLinkAction(parts: CoreParts): Omit<BoardAction, "package">;
