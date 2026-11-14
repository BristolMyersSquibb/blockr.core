import type { HelloReply } from "../protocol/index.ts";
import type { BlockId, Board, JsonBoard, PluginClassId, RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import type { Delta, Origin } from "./delta.ts";
import type { Dropped } from "./open.ts";
import type { Migration, Options, Plugins } from "./registrations.ts";
export type Change = {
    readonly delta: Delta;
    readonly origin: Origin;
} | {
    readonly opened: Board;
    readonly dropped: readonly Dropped[];
};
export interface Store {
    board(): Board;
    hasBoard(): boolean;
    locked(block: BlockId): boolean;
    subscribe(fn: (change: Change, board: Board) => void): () => void;
    apply(delta: Delta | readonly Delta[], origin?: Origin, opts?: {
        readonly undo?: false;
    }): void;
    dryRun(delta: Delta | readonly Delta[]): Board;
    open(given: string | HelloReply): Promise<Dropped[]>;
    check(text: string): Promise<Dropped[]>;
    jsonBoard(): JsonBoard;
    save(): string;
    undo(origin?: Origin): void;
    redo(origin?: Origin): void;
    canUndo(origin?: Origin): boolean;
    canRedo(origin?: Origin): boolean;
    readonly plugins: Plugins;
    readonly options: Options;
    blockRegistry(): Readonly<Record<RegistryId, RegistryEntry>>;
    packages(): Readonly<Record<string, string>>;
    editId(of: {
        readonly block: BlockId;
    } | {
        readonly plugin: PluginClassId;
    }): number | undefined;
}
export interface StoreOptions {
    readonly hello?: HelloReply;
    readonly signal?: AbortSignal;
    readonly migration?: (registryId: RegistryId) => Migration | undefined;
}
export declare function createStore(options?: StoreOptions): Store;
