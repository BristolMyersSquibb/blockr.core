import type { Commands } from "../commands/index.ts";
import type { Lock } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Options, PluginClass, Storage, Store } from "../store/index.ts";
import type { BlockId, OptionId, Plugin, PluginId, RegistryId } from "./board.ts";
import type { Rendered } from "./hosts.ts";
import type { Json } from "./json.ts";
import type { RegistryEntry } from "./registry.ts";
export interface CoreApi {
    readonly store: Store;
    readonly commands: Commands;
    readonly scheduler: Pick<Scheduler, "demand" | "evaluate" | "invalidate" | "status" | "subscribe">;
    blockRegistry(): BlockRegistry;
    readonly options: Options & {
        value(option: OptionId): Json | undefined;
    };
    readonly plugins: {
        register(def: PluginClass): () => void;
        running(): Readonly<Record<PluginId, Plugin>>;
        classOf(plugin: PluginId): PluginClass;
        storage(): Storage;
    };
    readonly lock: Lock;
    readonly outputEvents: OutputEvents;
    readonly notifications: {
        notify(note: Note): void;
        subscribe(fn: (note: Note) => void): () => void;
    };
    readonly focus: {
        get(): BlockId | null;
        set(block: BlockId | null): void;
        subscribe(fn: (block: BlockId | null) => void): () => void;
    };
    request<T = Json>(plugin: PluginId, name: string, args?: Readonly<Record<string, Json>>, opts?: RequestOpts): Promise<T | Blob>;
    module<T = unknown>(id: string): Promise<T>;
    blockrOption<T extends Json>(name: string, fallback: T): Promise<T>;
    mountBlock(el: HTMLElement, block: BlockId): () => void;
    mountOutput(el: HTMLElement, block: BlockId): () => void;
    mountOption(el: HTMLElement, option: OptionId): () => void;
    mountPlugin(el: HTMLElement, plugin: PluginId): () => void;
}
export interface Note {
    readonly message: string;
    readonly severity: "message" | "warning" | "error";
}
export interface PluginModule {
    render(args: {
        el: HTMLElement;
        core: CoreApi;
        plugin: PluginId;
        signal: AbortSignal;
    }): Rendered;
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
