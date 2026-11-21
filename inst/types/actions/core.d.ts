import type { Commands } from "../commands/index.ts";
import type { BlockRegistry } from "../types/core-api.ts";
import type { Raise } from "../util/index.ts";
import type { BoardAction } from "./actions.ts";
export interface CoreParts {
    readonly raise: Raise;
    readonly preflight: Commands["preflight"];
    readonly blockRegistry: () => BlockRegistry;
}
export declare function coreBoardActions(parts: CoreParts): BoardAction[];
