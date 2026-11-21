import type { BlockId, Board, Stack } from "../types/board.ts";
import type { AdvancedModule } from "./advanced.ts";
import type { Target } from "./actions.ts";
export declare const PALETTE: readonly ["#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3"];
export declare function suggestedColor(stacks: readonly Stack[]): string;
export declare function suggestedName(stacks: readonly Stack[]): string;
export declare const createStackForm: AdvancedModule;
export declare const editStackForm: AdvancedModule;
export declare function stackedBlocks(board: Board, target: Target): BlockId[];
