import type { Json } from "./json.ts";
export type BlockId = string;
export type LinkId = string;
export type StackId = string;
export type OptionId = string;
export type PluginId = string;
export type PluginClassId = string;
export type RegistryId = string;
export interface Board {
    readonly id: string;
    readonly name: string;
    readonly blocks: Readonly<Record<BlockId, Block>>;
    readonly links: Readonly<Record<LinkId, Link>>;
    readonly stacks: Readonly<Record<StackId, Stack>>;
    readonly options: Readonly<Record<OptionId, Json>>;
    readonly plugins: Readonly<Record<PluginId, Plugin>>;
}
export interface Block {
    readonly registry_id: RegistryId;
    readonly state: Readonly<Record<string, Json>>;
    readonly name: string;
}
export type Plugin = {
    readonly class: PluginClassId;
    readonly name: string;
    readonly state: Json;
};
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
    readonly board: Omit<Board, "blocks" | "options"> & {
        readonly versions: Readonly<Record<PluginClassId, number>>;
        readonly blocks: Readonly<Record<BlockId, JsonBlock>>;
        readonly options: Readonly<Record<OptionId, JsonOption>>;
    };
}
export interface JsonBlock extends Block {
    readonly version: number;
}
export interface JsonOption {
    readonly value: Json;
    readonly version: number;
}
