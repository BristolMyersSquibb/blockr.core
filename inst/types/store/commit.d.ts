import type { BlockId, Board, LinkId, OptionId, PluginId, StackId } from "../types/board.ts";
import type { Delta } from "./delta.ts";
export type Places = Readonly<Record<string, Readonly<Record<string, string | null>>>>;
export interface Inverse {
    readonly delta: Delta;
    readonly places: Places;
}
export interface Changed {
    readonly blocks: ReadonlySet<BlockId>;
    readonly links: ReadonlySet<LinkId>;
    readonly stacks: ReadonlySet<StackId>;
    readonly options: ReadonlySet<OptionId>;
    readonly plugins: ReadonlySet<PluginId>;
    readonly targets: ReadonlySet<string>;
}
export interface Committed {
    readonly board: Board;
    readonly inverse: Inverse;
    readonly changed: Changed;
}
export type Target = {
    readonly block: BlockId;
} | {
    readonly plugin: PluginId;
};
export declare function target(of: Target): string;
export declare function commit(board: Board, delta: Delta, places?: Places): Committed | {
    readonly failures: readonly string[];
};
