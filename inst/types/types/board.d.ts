import type { Json } from "./json.ts";
export type BlockId = string;
export type LinkId = string;
export type StackId = string;
export type OptionId = string;
export type RegistryId = string;
export interface FrontEndState {
}
export interface Board {
    readonly id: string;
    readonly front_end: FrontEnd;
    readonly name: string;
    readonly blocks: Readonly<Record<BlockId, Block>>;
    readonly links: Readonly<Record<LinkId, Link>>;
    readonly stacks: Readonly<Record<StackId, Stack>>;
    readonly options: Readonly<Record<OptionId, Json>>;
}
export interface FrontEnd {
    readonly id: string;
    readonly state: Readonly<Partial<FrontEndState> & Record<string, unknown>>;
    readonly attrs: Readonly<Record<string, Json>>;
}
export interface Block {
    readonly registry_id: RegistryId;
    readonly state: Readonly<Record<string, Json>>;
    readonly name: string;
    readonly attrs: Readonly<Record<string, Json>>;
}
export interface Link {
    readonly from: BlockId;
    readonly to: BlockId;
    readonly input: string;
}
export interface Stack {
    readonly name: string;
    readonly blocks: readonly BlockId[];
}
export interface SavedBoard {
    readonly format: number;
    readonly packages: Readonly<Record<string, string>>;
    readonly board: Board;
}
export interface OpeningBoard {
    readonly id?: string;
    readonly front_end: FrontEnd;
    readonly name?: string;
    readonly blocks: readonly (Block & {
        readonly id?: BlockId;
    })[];
    readonly links: readonly (Link & {
        readonly id?: LinkId;
    })[];
    readonly stacks: readonly (Stack & {
        readonly id?: StackId;
    })[];
    readonly options: Readonly<Record<OptionId, Json>>;
}
