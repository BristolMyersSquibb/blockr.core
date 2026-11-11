import type { Loader, Mounts } from "../mount/index.ts";
import type { PluginClass, Storage, Store } from "../store/index.ts";
import type { Plugin, PluginId } from "../types/board.ts";
import type { CoreApi, PluginModule } from "../types/core-api.ts";
export declare const BASIC_FRONTEND: {
    readonly frontend: "blockr.core-basic";
    readonly options: readonly ["show_conditions", "dark_mode"];
};
type PluginArgs = Parameters<PluginModule["render"]>[0];
export type FrontendArgs = Omit<PluginArgs, "plugin"> & {
    plugin?: PluginId;
};
export interface Frontend {
    readonly module: string;
    readonly plugin: PluginId | undefined;
    readonly added: Promise<readonly string[]>;
}
export interface RunningPlugins {
    running(): Readonly<Record<PluginId, Plugin>>;
    pluginClass(plugin: PluginId): PluginClass;
    storage(): Storage;
    mountPlugin(el: HTMLElement, plugin: PluginId): () => void;
    frontend(): Frontend;
    start(): Promise<void>;
}
export interface RunningDeps {
    readonly store: Store;
    readonly loader: Loader;
    readonly keepOptions: Mounts["keepOptions"];
    readonly core: () => CoreApi;
    readonly signal?: AbortSignal;
    readonly alone?: boolean;
}
export declare function runPlugins(deps: RunningDeps): RunningPlugins;
export {};
