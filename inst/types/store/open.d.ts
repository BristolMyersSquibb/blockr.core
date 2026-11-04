import type { OpenReply } from "../protocol/index.ts";
import type { Board, OpeningBoard, OptionId, SavedBoard } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { Built, Registrations } from "./registrations.ts";
export declare const FORMAT = 1;
export type Dropped = {
    readonly block: string;
    readonly registry_id: string;
    readonly package: string | null;
} | {
    readonly class: string;
    readonly skipped: readonly string[];
};
export declare function parse(text: string): unknown;
export declare function checkFile(file: unknown): SavedBoard;
export declare function classOf(board: Board): readonly string[];
export declare function keyed(opening: OpeningBoard): Board;
export declare function openFile(file: SavedBoard, reply: OpenReply, registrations: Registrations): {
    board: Board;
    dropped: Dropped[];
    built: Built;
};
export declare function required(board: Board, built: Built, registrations: Registrations): Promise<Readonly<Record<OptionId, Json>>>;
