import type { Commands } from "../commands/index.ts";
import type { Lock } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Options, PluginClass, Store } from "../store/index.ts";
import type { BlockId, OptionId, PluginId, RegistryId } from "./board.ts";
import type { Json } from "./json.ts";
import type { RegistryEntry } from "./registry.ts";
export interface CoreApi {
    readonly store: Store;
    readonly commands: Commands;
    readonly scheduler: Pick<Scheduler, "demand" | "evaluate" | "invalidate" | "status" | "subscribe">;
    blockRegistry(): BlockRegistry;
    readonly options: Options;
    readonly plugins: {
        register(def: PluginClass): () => void;
    };
    readonly lock: Lock | null;
    readonly outputEvents: OutputEvents;
    request<T = Json>(plugin: PluginId, name: string, args?: Readonly<Record<string, Json>>, opts?: RequestOpts): Promise<T | Blob>;
    module<T = unknown>(id: string): Promise<T>;
    blockrOption(name: string): Promise<Json | undefined>;
    mountBlock(el: HTMLElement, block: BlockId): () => void;
    mountOutput(el: HTMLElement, block: BlockId): () => void;
    mountOption(el: HTMLElement, option: OptionId): () => void;
}
export type BlockRegistry = Readonly<Record<RegistryId, RegistryEntry>>;
export interface RequestOpts {
    reads?: BlockId[];
    signal?: AbortSignal;
    file?: File;
    progress?(note: {
        value?: number;
        message?: string;
    }): void;
}
export interface OutputEvents {
    subscribe(fn: (event: {
        block: BlockId;
        name: string;
        payload: Json;
    }) => void): () => void;
}
