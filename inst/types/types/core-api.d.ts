import type { Commands } from "../commands/index.ts";
import type { Lock, Phase } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Options, PluginClass, Storage, Store } from "../store/index.ts";
import type { BlockId, LinkId, OptionId, PluginClassId, RegistryId, StackId } from "./board.ts";
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
        notify(plugin: PluginClassId, notification: Raised): () => void;
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
    readonly severity: Severity;
    readonly message: string;
    readonly source: NotificationSource;
    readonly about?: BoardObject;
}
export type Severity = "message" | "warning" | "error";
export type BoardObject = {
    readonly block: BlockId;
} | {
    readonly link: LinkId;
} | {
    readonly stack: StackId;
} | {
    readonly option: OptionId;
} | {
    readonly plugin: PluginClassId;
};
export type NotificationSource = RSource | CoreSource | ModuleSource;
export interface RSource {
    readonly kind: "r";
    readonly class: readonly string[];
    readonly request: RRequest;
}
export type RRequest = {
    readonly type: "evaluate";
    readonly phase?: EvaluatePhase;
} | {
    readonly type: "output";
} | {
    readonly type: "block_class" | "output_class" | "option" | "plugin";
    readonly name: string;
} | {
    readonly type: "renderer";
    readonly renderer: string;
    readonly name: string;
};
export type EvaluatePhase = Extract<Phase, "expr" | "inputs" | "block_eval" | "result_summary">;
export interface CoreSource {
    readonly kind: "core";
    readonly code: CoreCode;
    readonly package?: string;
    readonly renderer?: string;
}
export type CoreCode = "module_failed" | "board_option_failed" | "on_change_failed" | "plugin_start_failed" | "board_open_failed" | "edit_refused" | "board_objects_dropped" | "page_reload_needed" | "evaluation_unsent" | "output_unanswered";
export interface ModuleSource {
    readonly kind: "module";
    readonly object: {
        readonly plugin: PluginClassId;
    } | {
        readonly block: BlockId;
    } | {
        readonly option: OptionId;
    };
    readonly code: string;
    readonly package?: string;
    readonly renderer?: string;
}
export interface Raised {
    readonly severity: Severity;
    readonly message: string;
    readonly code: string;
    readonly about?: BoardObject | null;
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
