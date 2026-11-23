import type { Board } from "../types/board.ts";
import type { Delta } from "./delta.ts";
import type { Registrations } from "./registrations.ts";
export declare const ROUNDS = 10;
export declare function expand(delta: Delta, board: Board, registrations: Pick<Registrations, "plugin">): Delta | {
    readonly failures: readonly string[];
};
export declare function afterStep(board: Board, before: Board, registrations: Pick<Registrations, "plugin">): Delta | undefined | {
    readonly failures: readonly string[];
};
