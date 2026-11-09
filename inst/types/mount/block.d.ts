import type { Commands } from "../commands/index.ts";
import type { Renderer } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { BlockId, OptionId } from "../types/board.ts";
import type { Defaults } from "./defaults.ts";
import type { Loader } from "./load.ts";
import type { Measure } from "./measure.ts";
import type { OutputEvent } from "./output.ts";
export interface Mounts {
    mountBlock(el: HTMLElement, block: BlockId): () => void;
    mountOutput(el: HTMLElement, block: BlockId): () => void;
    mountOption(el: HTMLElement, option: OptionId): () => void;
    readonly defaults: Defaults;
}
export interface MountsOptions {
    readonly store: Store;
    readonly commands: Pick<Commands, "run">;
    readonly scheduler: Pick<Scheduler, "evaluate" | "inputs" | "request" | "status" | "subscribe">;
    readonly loader: Loader;
    readonly renderers?: () => Readonly<Record<string, Renderer>>;
    readonly emit?: (event: OutputEvent) => void;
    readonly measure?: Measure;
    readonly signal?: AbortSignal;
}
export declare function createMounts(options: MountsOptions): Mounts;
