import type { HelloReply } from "../protocol/index.ts";
import type { Board, JsonBoard, OptionId, PluginClassId } from "../types/board.ts";
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
    readonly plugin: PluginClassId;
    readonly package: string | null;
} | {
    readonly option: OptionId;
};
export declare function parse(text: string): unknown;
export declare function checkJsonBoard(json: unknown): JsonBoard;
export declare function openJsonBoard(json: JsonBoard, reply: Pick<HelloReply, "packages" | "block_registry" | "plugins" | "options">, registrations: Registrations): {
    board: Board;
    dropped: Dropped[];
};
