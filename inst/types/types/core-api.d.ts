import type { BoardAction, CommandCall, Target } from "../actions/index.ts";
import type { Commands } from "../commands/index.ts";
import type { Lock, Phase } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Delta, Options, PluginClass, Storage, Store } from "../store/index.ts";
import type { BlockId, BoardActionId, Id, LinkId, OptionId, PluginClassId, RegistryId, RendererId, StackId } from "./board.ts";
import type { Rendered } from "./hosts.ts";
import type { Json } from "./json.ts";
import type { Log } from "./log.ts";
import type { RegistryEntry } from "./registry.ts";
export interface CoreApi {
    readonly store: Store;
    readonly commands: Commands;
    readonly boardActions: {
        on(target: Target): readonly BoardAction[];
        menu(target: Target, opts: {
            el: HTMLElement;
            anchor: DOMRect;
            extend?(deltas: readonly Delta[]): readonly CommandCall[];
            notifier?(notifications: readonly Notification[], action: BoardAction): void;
            advanced?: FrontendModule["advanced"];
        }): void;
        trigger(id: {
            readonly package: string;
            readonly id: string;
        } | string, target: Target, opts: {
            el: HTMLElement;
            anchor?: DOMRect;
            extend?(deltas: readonly Delta[]): readonly CommandCall[];
            notifier?(notifications: readonly Notification[], action: BoardAction): void;
            advanced?: FrontendModule["advanced"];
        }): Promise<void>;
        mountAdvanced(el: HTMLElement, id: {
            readonly package: string;
            readonly id: string;
        } | string, target: Target, picked?: Json): () => void;
    };
    readonly scheduler: Pick<Scheduler, "demand" | "evaluate" | "invalidate" | "status" | "subscribe">;
    blockRegistry(): BlockRegistry;
    readonly options: Options & {
        value(option: OptionId): Json | undefined;
    };
    readonly plugins: {
        appPlugins(): ReadonlyMap<PluginClassId, Plugin>;
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
    log(pkg: string): Log;
    readonly focus: {
        get(): BlockId | null;
        set(block: BlockId | null): void;
        subscribe(fn: (block: BlockId | null) => void): () => void;
    };
    readonly selection: {
        get(): readonly BoardObject[];
        set(objects: readonly BoardObject[]): void;
        subscribe(fn: (objects: readonly BoardObject[]) => void): () => void;
    };
    readonly onScreen: OnScreen;
    show(object: {
        readonly block: BlockId;
    } | {
        readonly plugin: PluginClassId;
    }): Promise<void>;
    request<T = Json>(plugin: PluginClassId, name: string, args?: Readonly<Record<string, Json>>, opts?: RequestOpts): Promise<T | Blob>;
    id<P extends string | null>(pkg: P, id: string): Id<P>;
    module<T = unknown>(id: {
        readonly package: string;
        readonly id: string;
    }): Promise<T>;
    blockrOption(name: string): Promise<Json | undefined>;
    mintId(taken: (id: string) => boolean): string;
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
export interface SourceModule {
    readonly object: {
        readonly plugin: PluginClassId;
    } | {
        readonly block: BlockId;
    } | {
        readonly option: OptionId;
    };
    readonly renderer?: RendererId;
    readonly package?: string;
}
export interface RSource {
    readonly kind: "r";
    readonly class: readonly string[];
    readonly request: RRequest;
    readonly module?: SourceModule;
}
export type RRequest = {
    readonly type: "evaluate";
    readonly phase?: EvaluatePhase;
} | {
    readonly type: "output";
} | {
    readonly type: "block_class" | "output_class" | "renderer" | "option" | "plugin";
    readonly name: string;
};
export type EvaluatePhase = Extract<Phase, "expr" | "inputs" | "block_eval" | "result_summary">;
export interface CoreSource {
    readonly kind: "core";
    readonly code: CoreCode;
    readonly module?: SourceModule;
}
export type CoreCode = "module_failed" | "board_option_failed" | "on_change_failed" | "plugin_start_failed" | "board_open_failed" | "board_edit_failed" | "board_action_failed" | "board_objects_dropped" | "page_reload_needed" | "evaluation_unsent" | "output_unanswered" | "copy_failed";
export interface CoreFailure {
    readonly kind: "core";
    readonly code: Extract<CoreCode, "output_unanswered" | "board_option_failed">;
    readonly message: string;
}
export interface ModuleSource {
    readonly kind: "module";
    readonly module: SourceModule;
    readonly code: string;
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
    show?(args: {
        object: {
            readonly block: BlockId;
        } | {
            readonly plugin: PluginClassId;
        };
        el: HTMLElement;
        core: CoreApi;
    }): void | Promise<void>;
    advanced?(args: {
        readonly id: BoardActionId;
        readonly title: string;
        readonly target: Target;
        readonly mount: (el: HTMLElement, target?: Target) => () => void;
        readonly close: () => void;
    }): (deltas?: readonly Delta[]) => void;
}
export interface OnScreen {
    get(): {
        readonly blocks: readonly BlockId[];
        readonly plugins: readonly PluginClassId[];
    } | null;
    set(shown: {
        readonly blocks: readonly BlockId[];
        readonly plugins: readonly PluginClassId[];
    }): void;
    subscribe(fn: (shown: {
        readonly blocks: readonly BlockId[];
        readonly plugins: readonly PluginClassId[];
    } | null) => void): () => void;
}
export type BlockRegistry = ReadonlyMap<RegistryId, RegistryEntry>;
export interface RequestOpts {
    reads?: BlockId[];
    signal?: AbortSignal;
    file?: File;
    progress?(note: {
        value?: number;
        message?: string;
    }): void;
    notifier?(notifications: readonly Notification[]): void;
}
export type Notifier = (notifications: readonly Notification[]) => void;
export interface OutputEvents {
    subscribe(fn: (event: {
        block: BlockId;
        name: string;
        payload: Json;
    }) => void): () => void;
}
