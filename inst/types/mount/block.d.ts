import type { Commands } from "../commands/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { BlockId, OptionId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import type { Notify, Raise, Refused, Replied } from "../util/index.ts";
import type { Loader } from "./load.ts";
import type { Measure } from "./measure.ts";
import type { OnScreenOf, OutputEvent } from "./output.ts";
import type { UserPicks } from "./user-picks.ts";
export interface Mounts {
    mountBlock(el: HTMLElement, block: BlockId): () => void;
    mountOutput(el: HTMLElement, block: BlockId): () => void;
    mountOption(el: HTMLElement, option: OptionId): () => void;
    addOptions(ids: readonly OptionId[], by: string, user?: boolean): Promise<readonly string[]>;
    keepOptions(ids: readonly OptionId[], by: string, failed?: (failures: readonly string[]) => void, user?: boolean): {
        readonly added: Promise<readonly string[]>;
        readonly stop: () => void;
    };
    value(id: OptionId): Json | undefined;
    pick(id: OptionId, value: Json): void;
    onPick(fn: (id: OptionId, value: Json) => void): () => void;
}
export interface MountsOptions {
    readonly store: Store;
    readonly commands: Pick<Commands, "run">;
    readonly scheduler: Pick<Scheduler, "evaluate" | "inputs" | "request" | "status" | "subscribe">;
    readonly loader: Loader;
    readonly emit?: (event: OutputEvent) => void;
    readonly measure?: Measure;
    readonly onScreen?: OnScreenOf;
    readonly signal?: AbortSignal;
    readonly core: () => CoreApi;
    readonly raise?: Raise;
    readonly replied?: Replied;
    readonly notify?: Notify;
    readonly refused?: Refused;
    readonly optionPackage?: (option: OptionId) => string | undefined;
    readonly userPicks?: UserPicks;
}
export declare function createMounts(options: MountsOptions): Mounts;
