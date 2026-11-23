import type { Store } from "../store/index.ts";
import type { Board, PluginClassId } from "../types/board.ts";
import type { BoardObject, CoreApi, Notification, Notifier } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import type { Lock } from "../protocol/index.ts";
import type { Refused } from "../util/index.ts";
import type { AdvancedModule, Place, ShowDialog } from "./advanced.ts";
import type { CoreParts } from "./core.ts";
import type { OpenMenu, QuickLevel } from "./quick.ts";
import type { Role } from "./roles.ts";
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
    quick?(board: Board, target: Target, core: CoreApi): QuickLevel | undefined | Promise<QuickLevel | undefined>;
    readonly advanced?: AdvancedModule;
    readonly role?: string;
}
export interface BoardActions {
    register(plugin: PluginClassId, actions: readonly BoardAction[] | undefined, roles?: readonly Role[]): () => void;
    registered(): readonly BoardAction[];
    classOf(id: string): PluginClassId | undefined;
    role(id: string): Role | undefined;
    choosePlayers(picks: Json | undefined): void;
    player(role: string): BoardAction | undefined;
    offered(): readonly BoardAction[];
}
export declare function createBoardActions(parts: CoreParts): BoardActions;
export interface MenuPlace {
    readonly el: HTMLElement;
    readonly anchor: DOMRect;
}
export type ShowMenu = (menu: OpenMenu, at: MenuPlace) => () => void;
export interface BoardActionsParts {
    readonly store: Pick<Store, "board" | "hasBoard" | "subscribe">;
    readonly lock: Lock;
    readonly core: () => CoreApi;
    readonly refused: Refused;
    readonly raiseAgain: (notification: Omit<Notification, "id">, notifier?: Notifier) => void;
    readonly show: ShowMenu;
    readonly frontend?: () => Place | undefined;
    readonly dialog: ShowDialog;
    readonly signal?: AbortSignal;
}
export declare function boardActionsApi(actions: BoardActions, parts: BoardActionsParts): CoreApi["boardActions"];
