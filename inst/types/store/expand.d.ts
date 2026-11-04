import type { Board } from "../types/board.ts";
import type { Delta } from "./delta.ts";
import type { Registrations } from "./registrations.ts";
export declare const ROUNDS = 10;
export declare function expand(delta: Delta, board: Board, registrations: Registrations): Delta | {
    readonly failures: readonly string[];
};
