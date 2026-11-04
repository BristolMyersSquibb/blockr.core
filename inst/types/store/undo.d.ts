import type { Board } from "../types/board.ts";
import type { Delta } from "./delta.ts";
import type { Registrations } from "./registrations.ts";
export declare function fit(delta: Delta, board: Board, registrations: Registrations): Delta;
export declare function isEmpty(delta: Delta): boolean;
