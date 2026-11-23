import type { BlockRegistry } from "../types/core-api.ts";
import type { Board, PluginClassId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { BoardAction, CommandCall, Target } from "./actions.ts";
export interface Role {
    readonly id: string;
    readonly on: BoardAction["on"];
    readonly default: string;
    readonly advanced?: true;
    readonly commands: readonly RoleCommand[];
}
export interface RoleCommand {
    readonly id: string;
    readonly each?: "block" | "input" | "link" | "stack";
    readonly optional?: true;
    readonly repeats?: true;
    fixes?(board: Board, target: Target, args: Json): boolean;
}
export declare const CORE_ROLES: readonly Role[];
export declare function checkedPlayer(role: Role, action: BoardAction | undefined): {
    readonly player: BoardAction;
} | {
    readonly why: string;
};
export declare function batchFailure(role: Role, board: Board, registry: BlockRegistry, target: Target, batch: readonly CommandCall[]): string | undefined;
export declare function described(role: Role): string;
export declare function checkedRoles(plugin: PluginClassId, given: unknown, taken: ReadonlyMap<string, string>, actions: readonly BoardAction[]): readonly Role[];
