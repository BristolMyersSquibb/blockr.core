import type { PluginClassId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
export declare function refused(by: {
    readonly core: Pick<CoreApi, "notifications">;
    readonly plugin: PluginClassId;
}, error: unknown): void;
export declare function runCommand(by: {
    readonly core: Pick<CoreApi, "commands" | "notifications">;
    readonly plugin: PluginClassId;
}, id: string, args?: Json): Promise<boolean>;
