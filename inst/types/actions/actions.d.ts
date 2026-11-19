import type { Store } from "../store/index.ts";
import type { Board, PluginClassId } from "../types/board.ts";
import type { BoardObject, CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import type { Lock } from "../protocol/index.ts";
import type { Raise, Refused } from "../util/index.ts";
export type Target = {
    readonly board: true;
} | BoardObject | {
    readonly selection: readonly BoardObject[];
};
export interface CommandCall {
    readonly id: string;
    readonly args?: Json;
}
export interface BoardAction {
    readonly id: string;
    readonly label: string;
    readonly icon?: string;
    detail?(board: Board, target: Target): string | undefined;
    readonly on: readonly ("board" | "block" | "link" | "stack" | "option" | "plugin" | "selection")[];
    readonly category: string;
    offers?(board: Board, target: Target, lock: Lock): boolean;
    run?(board: Board, target: Target, core: CoreApi): readonly CommandCall[] | undefined | Promise<readonly CommandCall[] | undefined>;
}
export interface BoardActions {
    register(plugin: PluginClassId, actions: readonly BoardAction[] | undefined): () => void;
    registered(): readonly BoardAction[];
}
export declare function createBoardActions(raise: Raise): BoardActions;
export interface BoardActionsParts {
    readonly store: Pick<Store, "board" | "hasBoard">;
    readonly lock: Lock;
    readonly core: () => CoreApi;
    readonly refused: Refused;
}
export declare function boardActionsApi(actions: BoardActions, parts: BoardActionsParts): CoreApi["boardActions"];
