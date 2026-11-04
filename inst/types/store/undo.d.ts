import type { Board } from "../types/board.ts";
import type { Delta } from "./delta.ts";
import type { Built } from "./registrations.ts";
export declare function fit(delta: Delta, board: Board, built: Built): Delta;
export declare function isEmpty(delta: Delta): boolean;
