import type { HelloReply } from "../protocol/index.ts";
import type { Board, JsonBoard, OptionId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { Built, Registrations } from "./registrations.ts";
export declare const FORMAT = 1;
export type Dropped = {
    readonly package: string;
    readonly version: string;
} | {
    readonly block: string;
    readonly registry_id: string;
    readonly package: string | null;
};
export declare function parse(text: string): unknown;
export declare function checkJsonBoard(json: unknown): JsonBoard;
export declare function classOf(board: Board): readonly string[];
export declare function openJsonBoard(json: JsonBoard, reply: Pick<HelloReply, "packages" | "block_registry" | "board_classes">, registrations: Registrations): {
    board: Board;
    dropped: Dropped[];
    built: Built;
};
export declare function required(board: Board, built: Built, registrations: Registrations): Promise<Readonly<Record<OptionId, Json>>>;
