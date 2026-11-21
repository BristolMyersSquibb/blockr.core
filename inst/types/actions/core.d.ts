import type { RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import type { Raise } from "../util/index.ts";
import type { BoardAction } from "./actions.ts";
export declare function coreBoardActions(raise: Raise, registry: () => Readonly<Record<RegistryId, RegistryEntry>>): BoardAction[];
