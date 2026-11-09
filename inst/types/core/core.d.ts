import type { Commands } from "../commands/index.ts";
import type { Loader, Measure } from "../mount/index.ts";
import type { Lock, Renderer } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { OptionId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { BlockrOptions } from "./blockr-options.ts";
export interface CoreParts {
    readonly store: Store;
    readonly commands: Commands;
    readonly scheduler: Scheduler;
    readonly loader: Loader;
    readonly blockrOptions: BlockrOptions;
    readonly lock: Lock | null;
    readonly renderers: () => Readonly<Record<string, Renderer>>;
    readonly measure?: Measure;
    readonly optionPackage?: (id: OptionId) => string | null;
    readonly signal?: AbortSignal;
}
export declare function createCore(parts: CoreParts): CoreApi;
