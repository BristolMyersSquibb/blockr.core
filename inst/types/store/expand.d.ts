import type { Board } from "../types/board.ts";
import type { Delta } from "./delta.ts";
import type { Built } from "./registrations.ts";
export declare const ROUNDS = 10;
export declare function expand(delta: Delta, board: Board, built: Built): Delta | {
    readonly failures: readonly string[];
};
