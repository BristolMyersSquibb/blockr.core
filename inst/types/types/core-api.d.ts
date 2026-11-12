import type { Commands } from "../commands/index.ts";
import type { Lock, Phase } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Options, Origin, PluginClass, Storage, Store } from "../store/index.ts";
import type { BlockId, OptionId, PluginClassId, RegistryId } from "./board.ts";
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
        appPlugins(): Readonly<Record<PluginClassId, Plugin>>;
        pluginClass(plugin: PluginClassId): PluginClass;
        storage(): Storage | undefined;
    };
    readonly lock: Lock;
    readonly outputEvents: OutputEvents;
    readonly notifications: {
        notify(notification: Omit<Notification, "id">): () => void;
        get(): readonly Notification[];
        subscribe(fn: (all: readonly Notification[]) => void): () => void;
    };
    readonly focus: {
        get(): BlockId | null;
        set(block: BlockId | null): void;
        subscribe(fn: (block: BlockId | null) => void): () => void;
    };
    request<T = Json>(plugin: PluginClassId, name: string, args?: Readonly<Record<string, Json>>, opts?: RequestOpts): Promise<T | Blob>;
    module<T = unknown>(id: string): Promise<T>;
    blockrOption(name: string): Promise<Json | undefined>;
    mountBlock(el: HTMLElement, block: BlockId): () => void;
    mountOutput(el: HTMLElement, block: BlockId): () => void;
    mountOption(el: HTMLElement, option: OptionId): () => void;
    mountPlugin(el: HTMLElement, plugin: PluginClassId): () => void;
}
export interface Plugin {
    readonly name: string;
}
export interface Notification {
    readonly id: string;
    readonly severity: "message" | "warning" | "error";
    readonly class: readonly string[];
    readonly message: string;
    readonly origin: Origin;
    readonly phase?: Phase;
    readonly block?: BlockId;
    readonly option?: OptionId;
    readonly plugin?: PluginClassId;
    readonly package?: string;
}
export interface PluginModule {
    render(args: {
        el: HTMLElement;
        core: CoreApi;
        plugin: PluginClassId;
        signal: AbortSignal;
    }): Rendered;
}
export interface FrontendModule {
    render(args: {
        el: HTMLElement;
        core: CoreApi;
        plugin: PluginClassId;
        signal: AbortSignal;
        args: Readonly<Record<string, Json>>;
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
