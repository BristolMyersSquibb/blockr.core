import type { HelloReply } from "../protocol/index.ts";
import type { BlockId, Board, JsonBoard, OptionId, PluginClassId, RegistryId } from "../types/board.ts";
import type { Registrations } from "./registrations.ts";
export declare const FORMAT = 1;
export interface Dropped {
    readonly about: {
        readonly package: string;
        readonly version: string;
    } | {
        readonly block: BlockId;
        readonly name: string;
        readonly registry_id: RegistryId;
    } | {
        readonly plugin: PluginClassId;
    } | {
        readonly option: OptionId;
    };
    readonly package: string | null;
}
export declare function parse(text: string): unknown;
export declare function checkJsonBoard(json: unknown): JsonBoard;
export declare function openJsonBoard(json: JsonBoard, reply: Pick<HelloReply, "packages" | "block_registry" | "plugins" | "options">, registrations: Registrations): {
    board: Board;
    dropped: Dropped[];
};
