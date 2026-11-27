import type { Json } from "./json.ts";
export type BlockId = string;
export type LinkId = string;
export type StackId = string;
export type OptionId = string;
declare const interned: unique symbol;
export interface Id<P extends string | null = string> {
    readonly package: P;
    readonly id: string;
    readonly [interned]: true;
}
export type PluginClassId = Id;
export type RegistryId = Id<string | null>;
export type RendererId = Id<string | null>;
export type ModuleId = Id;
export type CommandId = Id;
export type BoardActionId = Id;
export interface Board {
    readonly id: string;
    readonly name: string;
    readonly blocks: Readonly<Record<BlockId, Block>>;
    readonly links: Readonly<Record<LinkId, Link>>;
    readonly stacks: Readonly<Record<StackId, Stack>>;
    readonly options: Readonly<Record<OptionId, Json>>;
    readonly plugins: Readonly<Record<string, Readonly<Record<string, Json>>>>;
}
export interface Block {
    readonly registry_id: RegistryId;
    readonly state: Readonly<Record<string, Json>>;
    readonly name: string;
    readonly locked: boolean | null;
}
export interface Link {
    readonly from: BlockId;
    readonly to: BlockId;
    readonly input: string;
}
export interface Stack {
    readonly name: string;
    readonly blocks: readonly BlockId[];
    /**
     * @pattern ^#[0-9a-fA-F]{6}$
     */
    readonly color: string | null;
}
export interface JsonBoard {
    readonly format: number;
    readonly packages: Readonly<Record<string, string>>;
    readonly board: Omit<Board, "blocks" | "options" | "plugins"> & {
        readonly blocks: Readonly<Record<BlockId, JsonBlock>>;
        readonly options: Readonly<Record<OptionId, JsonOption>>;
        readonly plugins: Readonly<Record<string, Readonly<Record<string, JsonPlugin>>>>;
    };
}
export interface JsonBlock extends Omit<Block, "registry_id"> {
    readonly registry_id: {
        readonly package: string | null;
        readonly id: string;
    };
    readonly version: number;
}
export interface JsonOption {
    readonly value: Json;
    readonly version: number;
    readonly package: string | null;
}
export interface JsonPlugin {
    readonly state: Json;
    readonly version: number;
}
export {};
