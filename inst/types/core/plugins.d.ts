import type { Mounts } from "../mount/index.ts";
import type { PluginEntry } from "../protocol/index.ts";
import type { PluginClass, Storage, Store } from "../store/index.ts";
import type { PluginClassId } from "../types/board.ts";
import type { CoreApi, FrontendModule, Plugin, PluginModule } from "../types/core-api.ts";
import type { Raise } from "../util/index.ts";
export type PluginArgs = Parameters<PluginModule["render"]>[0];
export interface Frontend {
    readonly frontend: FrontendModule;
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
    readonly keepOptions: Mounts["keepOptions"];
    readonly core: () => CoreApi;
    readonly plugins: Readonly<Record<PluginClassId, PluginEntry>>;
    readonly raise: Raise;
    readonly signal?: AbortSignal;
}
export declare function runPlugins(deps: RunningDeps): RunningPlugins;
