import type { BlockId, Board } from "../types/board.ts";
import type { Delta } from "./delta.ts";
export declare function fixes(board: Board | undefined, block: BlockId, boardLock: boolean): boolean;
export declare function lockFailures(before: Board, inverse: Delta): string[];
