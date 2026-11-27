import type { RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import type { BoardAction } from "./actions.ts";
type Registry = () => ReadonlyMap<RegistryId, RegistryEntry>;
export declare function addingBoardActions(registry: Registry): Omit<BoardAction, "package">[];
export {};
