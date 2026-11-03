import type { OpenReply } from "../protocol/messages.ts";
import type { Board, OpeningBoard, SavedBoard } from "../types/board.ts";
import type { Registrations } from "./registrations.ts";
export declare const FORMAT = 1;
export declare const BASIC_FRONT_END = "blockr.core-basic";
export type Dropped = {
    readonly block: string;
    readonly registry_id: string;
    readonly package: string | null;
} | {
    readonly front_end: string;
    readonly package: string | null;
};
export declare function parse(text: string): unknown;
export declare function checkFile(file: unknown): SavedBoard;
export declare function keyed(opening: OpeningBoard): Board;
export declare function openFile(file: SavedBoard, reply: OpenReply, registrations: Registrations): {
    board: Board;
    dropped: Dropped[];
};
export declare function frontEndOf(board: Board, frontEnds: Readonly<Record<string, unknown>>): string;
