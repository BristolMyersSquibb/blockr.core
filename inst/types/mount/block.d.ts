import type { Commands } from "../commands/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { BlockId, OptionId } from "../types/board.ts";
import type { Loader } from "./load.ts";
export interface Mounts {
    mountBlock(el: HTMLElement, block: BlockId): () => void;
    addOptions(ids: readonly OptionId[], by: string): Promise<void>;
}
export interface MountsOptions {
    readonly store: Store;
    readonly commands: Pick<Commands, "run">;
    readonly scheduler: Pick<Scheduler, "evaluate" | "inputs" | "request" | "status" | "subscribe">;
    readonly loader: Loader;
}
export declare function createMounts(options: MountsOptions): Mounts;
