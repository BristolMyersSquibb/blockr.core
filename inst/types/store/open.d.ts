import type { HelloReply } from "../protocol/index.ts";
import type { Board, JsonBoard, OptionId, PluginId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { Registrations } from "./registrations.ts";
export declare const FORMAT = 1;
export type Dropped = {
    readonly package: string;
    readonly version: string;
} | {
    readonly block: string;
    readonly registry_id: string;
    readonly package: string | null;
} | {
    readonly plugin: PluginId;
    readonly class: string;
    readonly package: string | null;
};
export declare function parse(text: string): unknown;
export declare function checkJsonBoard(json: unknown): JsonBoard;
export declare function openJsonBoard(json: JsonBoard, reply: Pick<HelloReply, "packages" | "block_registry" | "plugin_versions" | "option_versions" | "board">, registrations: Registrations): {
    board: Board;
    dropped: Dropped[];
};
export declare function required(board: Board, registrations: Registrations): Promise<Readonly<Record<OptionId, Json>>>;
