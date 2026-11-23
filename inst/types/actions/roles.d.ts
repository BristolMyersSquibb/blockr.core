import type { PluginClassId } from "../types/board.ts";
import type { BoardAction } from "./actions.ts";
export interface Role {
    readonly id: string;
    readonly on: BoardAction["on"];
    readonly default: string;
    readonly advanced?: true;
}
export declare const CORE_ROLES: readonly Role[];
export declare function checkedPlayer(role: Role, action: BoardAction | undefined): {
    readonly player: BoardAction;
} | {
    readonly why: string;
};
export declare function checkedRoles(plugin: PluginClassId, given: unknown, taken: ReadonlyMap<string, string>, actions: readonly BoardAction[]): readonly Role[];
