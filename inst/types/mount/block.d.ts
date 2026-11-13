import type { Commands } from "../commands/index.ts";
import type { Renderer } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { BlockId, OptionId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Notify, Raise, Replied } from "../util/index.ts";
import type { Loader } from "./load.ts";
import type { Measure } from "./measure.ts";
import type { OutputEvent } from "./output.ts";
export interface Mounts {
    mountBlock(el: HTMLElement, block: BlockId): () => void;
    mountOutput(el: HTMLElement, block: BlockId): () => void;
    mountOption(el: HTMLElement, option: OptionId): () => void;
    addOptions(ids: readonly OptionId[], by: string): Promise<readonly string[]>;
    keepOptions(ids: readonly OptionId[], by: string, failed?: (failures: readonly string[]) => void): {
        readonly added: Promise<readonly string[]>;
        readonly stop: () => void;
    };
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
    readonly core: () => CoreApi;
    readonly raise?: Raise;
    readonly replied?: Replied;
    readonly notify?: Notify;
    readonly optionPackage?: (option: OptionId) => string | undefined;
}
export declare function createMounts(options: MountsOptions): Mounts;
