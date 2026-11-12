import type { Loader, Mounts } from "../mount/index.ts";
import type { PluginEntry } from "../protocol/index.ts";
import type { PluginClass, Storage, Store } from "../store/index.ts";
import type { PluginClassId } from "../types/board.ts";
import type { CoreApi, Plugin, PluginModule } from "../types/core-api.ts";
export declare function corePluginClasses(): readonly PluginClass[];
export type PluginArgs = Parameters<PluginModule["render"]>[0];
export interface Frontend {
    readonly module: string;
    readonly plugin: PluginClassId;
    readonly added: Promise<readonly string[]>;
}
export interface RunningPlugins {
    appPlugins(): Readonly<Record<PluginClassId, Plugin>>;
    pluginClass(plugin: PluginClassId): PluginClass;
    storage(): Storage | undefined;
    mountPlugin(el: HTMLElement, plugin: PluginClassId): () => void;
    frontend(): Frontend | undefined;
    start(): Promise<void>;
}
export interface RunningDeps {
    readonly store: Store;
    readonly loader: Loader;
    readonly keepOptions: Mounts["keepOptions"];
    readonly core: () => CoreApi;
    readonly plugins: Readonly<Record<PluginClassId, PluginEntry>>;
    readonly signal?: AbortSignal;
}
export declare function runPlugins(deps: RunningDeps): RunningPlugins;
