import type { BlockId, Board } from "../types/board.ts";
import type { Target } from "./actions.ts";
export declare function blockOf(target: Target): BlockId | undefined;
export declare function heldBlock(board: Board, target: Target): BlockId | undefined;
