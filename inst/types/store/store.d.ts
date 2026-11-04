import type { HelloReply } from "../protocol/index.ts";
import type { BlockId, Board, JsonBoard, RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import type { Delta, Origin } from "./delta.ts";
import type { Dropped } from "./open.ts";
import type { Attribute, BoardClass, Migration, Options } from "./registrations.ts";
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
    subscribe(fn: (change: Change, board: Board) => void): () => void;
    apply(delta: Delta | readonly Delta[], origin?: Origin): void;
    open(given: string | HelloReply): Promise<Dropped[]>;
    jsonBoard(): JsonBoard;
    undo(origin?: Origin): void;
    redo(origin?: Origin): void;
    canUndo(origin?: Origin): boolean;
    canRedo(origin?: Origin): boolean;
    register(def: BoardClass | Attribute): () => void;
    registered(): readonly (BoardClass | Attribute)[];
    boardClass(classes: readonly string[]): BoardClass;
    frontEnd(classes: readonly string[]): string;
    registerMigration(pkg: string, fn: Migration): () => void;
    readonly options: Options;
    blockRegistry(): Readonly<Record<RegistryId, RegistryEntry>>;
    packages(): Readonly<Record<string, string>>;
    editId(of: {
        readonly block: BlockId;
    } | {
        readonly property: string;
    }): number | undefined;
}
export interface StoreOptions {
    readonly loaded?: (reply: HelloReply, classes: readonly string[]) => void | Promise<void>;
}
export declare function createStore(options?: StoreOptions): Store;
