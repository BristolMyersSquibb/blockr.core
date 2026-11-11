import type { Commands } from "../commands/index.ts";
import type { Loader, Measure } from "../mount/index.ts";
import type { Lock, Plugin, Renderer } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { PluginClassId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { BlockrOptions } from "./blockr-options.ts";
import type { RunningPlugins } from "./plugins.ts";
export interface CoreParts {
    readonly store: Store;
    readonly commands: Commands;
    readonly scheduler: Scheduler;
    readonly loader: Loader;
    readonly blockrOptions: BlockrOptions;
    readonly lock: Lock;
    readonly renderers: () => Readonly<Record<string, Renderer>>;
    readonly plugins: Readonly<Record<PluginClassId, Plugin>>;
    readonly measure?: Measure;
    readonly signal?: AbortSignal;
}
export interface Core {
    readonly api: CoreApi;
    readonly plugins: Pick<RunningPlugins, "frontend" | "start">;
}
export declare function createCore(parts: CoreParts): Core;
