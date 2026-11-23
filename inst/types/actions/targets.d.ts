import type { BlockId, Board } from "../types/board.ts";
import type { BoardAction, Target } from "./actions.ts";
export declare const KINDS: Readonly<Record<BoardAction["on"][number], true>>;
export declare function kindsFailure(on: unknown, empty: string): string | undefined;
export declare function idOf(target: Target, kind: "block" | "link" | "stack"): string | undefined;
export declare function blockOf(target: Target): BlockId | undefined;
export declare function heldBlock(board: Board, target: Target): BlockId | undefined;
export declare function held(board: Board, target: Target, kind: "block" | "link" | "stack"): string[];
export declare function heldBlocks(board: Board, target: Target): BlockId[];
