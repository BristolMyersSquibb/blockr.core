import type { CommandCall } from "../actions/index.ts";
import type { CoreApi, Notification } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
export type Done = (batch: readonly CommandCall[], opts: {
    notifier(): void;
}) => Promise<void>;
export declare function runCommand(core: Pick<CoreApi, "commands">, id: CommandName, args?: Json): Promise<boolean>;
export declare function runCommand(core: Pick<CoreApi, "commands">, batch: readonly CommandCall[]): Promise<boolean>;
export interface Edits {
    readonly refusal: Refused | null;
    run(title: string, id: CommandName, args?: Json): Promise<boolean>;
    run(title: string, batch: readonly CommandCall[], done?: Done): Promise<boolean>;
    clear(): void;
}
export interface Refused {
    readonly title: string;
    readonly reasons: readonly string[];
}
export declare function useEdits(core: Pick<CoreApi, "commands">): Edits;
export declare function notifiedReasons(notifications: readonly Pick<Notification, "message">[]): readonly string[];
interface CommandName {
    readonly package: string;
    readonly id: string;
}
export {};
