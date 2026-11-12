import type { BlockId, OptionId, PluginClassId } from "../types/board.ts";
import type { Notification } from "../types/core-api.ts";
export type Raise = (notification: Omit<Notification, "id">) => () => void;
export declare const raiseNothing: Raise;
export interface Concerns {
    readonly block?: BlockId;
    readonly option?: OptionId;
    readonly plugin?: PluginClassId;
    readonly package?: string | null;
}
export declare function coreNotification(code: string, severity: "warning" | "error", message: string, concerns?: Concerns): Omit<Notification, "id">;
