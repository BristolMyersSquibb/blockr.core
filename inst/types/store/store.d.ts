import type { Client } from "../protocol/client.ts";
import type { HelloReply, OpenReply } from "../protocol/messages.ts";
import type { BlockId, Board, RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import type { Delta, Origin } from "./delta.ts";
import type { Dropped } from "./open.ts";
import type { Attribute, Migration, Options, StateKey } from "./registrations.ts";
export type Change = {
    readonly delta: Delta;
    readonly origin: Origin;
} | {
    readonly opened: Board;
    readonly dropped: readonly Dropped[];
};
export interface Store {
    board(): Board;
    subscribe(fn: (change: Change, board: Board) => void): () => void;
    apply(delta: Delta | readonly Delta[], origin?: Origin): void;
    open(saved: string | HelloReply): Promise<Dropped[]>;
    undo(origin?: Origin): void;
    redo(origin?: Origin): void;
    canUndo(origin?: Origin): boolean;
    canRedo(origin?: Origin): boolean;
    register(def: StateKey | Attribute): () => void;
    registered(): readonly (StateKey | Attribute)[];
    registerMigration(pkg: string, fn: Migration): () => void;
    readonly options: Options;
    blockRegistry(): Readonly<Record<RegistryId, RegistryEntry>>;
    packages(): Readonly<Record<string, string>>;
    editId(of: {
        readonly block: BlockId;
    } | {
        readonly path: readonly string[];
        readonly entry?: string;
    }): number | undefined;
}
export interface StoreOptions {
    readonly client: Pick<Client, "request">;
    readonly loaded?: (reply: OpenReply, frontEnd: string) => void | Promise<void>;
}
export declare function createStore(options: StoreOptions): Store;
