import { ToastQueue } from "@blockr/ui";
import type { Board, PluginClassId } from "../../types/board.ts";
import type { BoardObject, CoreApi, Notification } from "../../types/core-api.ts";
export declare const SHOWN_FOR = 5000;
export interface Toasts {
    readonly queue: ToastQueue<Notification>;
    readonly stop: () => void;
}
export declare function createToasts(notifications: Pick<CoreApi["notifications"], "get" | "subscribe">, sectioned: (plugin: PluginClassId) => boolean): Toasts;
export declare function concerned(about: BoardObject | undefined, core: {
    readonly options: Pick<CoreApi["options"], "registered">;
    readonly plugins: Pick<CoreApi["plugins"], "appPlugins">;
}, board: Board): string;
export declare function sourcePackage(notification: Notification): string | undefined;
export declare function isReload(notification: Notification): boolean;
