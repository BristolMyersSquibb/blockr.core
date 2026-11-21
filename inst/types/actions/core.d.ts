import type { BlockRegistry } from "../types/core-api.ts";
import type { Raise } from "../util/index.ts";
import type { BoardAction } from "./actions.ts";
export declare function coreBoardActions(raise: Raise, registry: () => BlockRegistry): BoardAction[];
