import type { BlockId, Board, LinkId, OptionId, StackId } from "../types/board.ts";
import type { Delta } from "./delta.ts";
import type { Built } from "./registrations.ts";
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
    readonly properties: ReadonlySet<string>;
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
    readonly property: string;
};
export declare function target(of: Target): string;
export declare function commit(board: Board, delta: Delta, built: Built, places?: Places): Committed | {
    readonly failures: readonly string[];
};
