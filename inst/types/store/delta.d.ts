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
    readonly front_end?: {
        readonly state?: readonly KeyChange[];
        readonly attrs?: Values<string>;
    };
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
    readonly attrs?: Values<string>;
}
export interface KeyChange {
    readonly path: readonly string[];
    readonly change: KeyDelta;
}
export type Entry = Readonly<Record<string, Json>>;
export type KeyDelta = CollectionDelta | ValueDelta;
export interface CollectionDelta {
    readonly add?: Readonly<Record<string, Entry>>;
    readonly mod?: Readonly<Record<string, {
        readonly edit?: number;
        readonly set?: Entry;
        readonly unset?: readonly string[];
    }>>;
    readonly rm?: readonly string[];
}
export declare function isValueDelta(change: KeyDelta): change is ValueDelta;
export type ValueDelta = {
    readonly edit?: number;
    readonly set: Json;
    readonly unset?: never;
} | {
    readonly edit?: number;
    readonly unset: true;
    readonly set?: never;
};
