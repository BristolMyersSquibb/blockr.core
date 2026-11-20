import type { Board } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { BoardAction, CommandCall, Target } from "./actions.ts";
export interface QuickLevel {
    readonly title?: string;
    readonly filter?: boolean;
    readonly rows: readonly QuickRow[];
}
export type QuickRow = {
    readonly kind: "item";
    readonly label: string;
    readonly heading?: string;
    readonly icon?: string;
    readonly detail?: string;
    readonly category?: BoardAction["category"];
    pick(): readonly CommandCall[] | QuickLevel | Promise<readonly CommandCall[] | QuickLevel>;
} | {
    readonly kind: "multi";
    readonly label: string;
    readonly choices: readonly {
        readonly id: string;
        readonly label: string;
        readonly picked: boolean;
    }[];
    apply(picked: readonly string[]): readonly CommandCall[];
} | {
    readonly kind: "text";
    readonly label: string;
    readonly value?: string;
    check?(value: string): string | undefined;
    submit(value: string): readonly CommandCall[];
};
export interface ShownLevel {
    readonly id: number;
    readonly label: string;
    readonly title?: string;
    readonly filter?: string;
    readonly back: boolean;
    readonly loading: boolean;
    readonly error?: string;
    readonly rows: readonly ShownRow[];
}
export type ShownRow = Omit<Extract<QuickRow, {
    kind: "item";
}>, "pick"> | Omit<Extract<QuickRow, {
    kind: "multi";
}>, "apply"> | {
    readonly kind: "text";
    readonly label: string;
    readonly value: string;
    readonly refusal?: string;
};
export interface OpenMenu {
    readonly get: () => ShownLevel;
    readonly subscribe: (fn: () => void) => () => void;
    readonly closed: () => boolean;
    readonly pick: (row?: number) => Promise<void>;
    readonly tick: (row: number, choice: string) => void;
    readonly type: (value: string, row?: number) => void;
    readonly escape: () => void;
    readonly close: () => Promise<void>;
    readonly drop: () => void;
}
export type MenuStart = {
    readonly actions: readonly BoardAction[];
} | {
    readonly action: BoardAction;
};
export interface MenuParts {
    readonly board: () => Board;
    readonly target: Target;
    readonly core: () => CoreApi;
    readonly applies: (action: BoardAction) => boolean;
    readonly run: (batch: readonly CommandCall[], action: BoardAction) => Promise<void>;
    readonly trigger: (action: BoardAction) => Promise<void>;
    readonly failed: (action: BoardAction, failure: Error) => void;
    readonly closed: (ran: Promise<void> | undefined) => void;
}
export declare function inMenuOrder(actions: readonly BoardAction[]): BoardAction[];
export declare function openMenu(start: MenuStart, parts: MenuParts): OpenMenu;
export declare function isBatch(value: unknown): value is readonly CommandCall[];
