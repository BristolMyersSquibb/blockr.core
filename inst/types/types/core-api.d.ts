import type { Commands } from "../commands/commands.ts";
import type { Lock } from "../protocol/messages.ts";
import type { Scheduler } from "../scheduler/scheduler.ts";
import type { Options } from "../store/registrations.ts";
import type { Store } from "../store/store.ts";
import type { BlockId, OptionId, RegistryId } from "./board.ts";
import type { Json } from "./json.ts";
import type { RegistryEntry } from "./registry.ts";
export interface CoreApi {
    readonly store: Store;
    readonly commands: Commands;
    readonly scheduler: Pick<Scheduler, "demand" | "evaluate" | "invalidate" | "status" | "subscribe">;
    blockRegistry(): BlockRegistry;
    readonly options: Options;
    readonly lock: Lock | null;
    readonly outputEvents: OutputEvents;
    request<T = Json>(extension: string, name: string, args?: Json, opts?: RequestOpts): Promise<T | Blob>;
    module<T = unknown>(id: string): Promise<T>;
    blockrOption<T extends Json>(name: string, fallback: T): Promise<T>;
    mountBlock(el: HTMLElement, block: BlockId): () => void;
    mountOutput(el: HTMLElement, block: BlockId): () => void;
    mountOption(el: HTMLElement, option: OptionId): () => void;
}
export type BlockRegistry = Readonly<Record<RegistryId, RegistryEntry>>;
export interface RequestOpts {
    reads?: BlockId[];
    signal?: AbortSignal;
    progress?(note: {
        value?: number;
        message?: string;
    }): void;
}
export interface OutputEvents {
    subscribe(fn: (event: {
        block: BlockId;
        name: string;
        payload: Json;
    }) => void): () => void;
}
