import type { Board, RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import type { Changed } from "./commit.ts";
import type { Registrations } from "./registrations.ts";
export declare const HEX_COLOR = "^#[0-9a-fA-F]{6}$";
export declare function stateFailures(entry: RegistryEntry, state: unknown): readonly string[];
export declare function check(board: Board, registry: Readonly<Record<RegistryId, RegistryEntry>>, registrations: Registrations, changed?: Changed): string[];
