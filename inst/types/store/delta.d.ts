import type { Block, BlockId, Link, LinkId, OptionId, Stack, StackId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
export type Origin = string;
export interface Delta {
    readonly blocks?: {
        readonly add?: Readonly<Record<BlockId, Block>>;
        readonly mod?: Readonly<Record<BlockId, BlockMod>>;
        readonly rm?: readonly BlockId[];
    };
    readonly links?: {
        readonly add?: Readonly<Record<LinkId, Link>>;
        readonly mod?: Readonly<Record<LinkId, Partial<Link>>>;
        readonly rm?: readonly LinkId[];
        readonly before?: Readonly<Record<LinkId, LinkId>>;
    };
    readonly stacks?: {
        readonly add?: Readonly<Record<StackId, Stack>>;
        readonly mod?: Readonly<Record<StackId, Partial<Stack>>>;
        readonly rm?: readonly StackId[];
    };
    readonly options?: Values<OptionId>;
    readonly name?: string;
    readonly patches?: readonly Patch[];
}
export declare function isDeltas(delta: Delta | readonly Delta[]): delta is readonly Delta[];
export interface Values<K extends string> {
    readonly set?: Readonly<Record<K, Json>>;
    readonly unset?: readonly K[];
}
export interface BlockMod {
    readonly edit?: number;
    readonly state?: Readonly<Record<string, Json>>;
    readonly unset?: readonly string[];
    readonly name?: string;
    readonly locked?: boolean | null;
}
export interface Patch {
    readonly op: "add" | "remove" | "replace";
    readonly path: string;
    readonly value?: Json;
    readonly edit?: number;
}
