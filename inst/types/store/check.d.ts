import type { Board, RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import type { Changed } from "./commit.ts";
import type { Built, Registrations } from "./registrations.ts";
export declare function check(board: Board, registry: Readonly<Record<RegistryId, RegistryEntry>>, registrations: Registrations, built: Built, changed?: Changed): string[];
export declare function isClass(x: unknown): x is readonly string[];
