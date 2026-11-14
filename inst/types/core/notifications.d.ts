import type { Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { PluginClassId } from "../types/board.ts";
import type { CoreApi, Notification } from "../types/core-api.ts";
import type { Notify, Raise, Replied } from "../util/index.ts";
export interface Holder {
    readonly api: CoreApi["notifications"];
    readonly raise: Raise;
    readonly raiseAgain: (notification: Omit<Notification, "id">) => void;
    readonly replied: Replied;
    readonly notify: Notify;
    follow(scheduler: Pick<Scheduler, "status" | "subscribe">): void;
}
export interface HolderOptions {
    readonly signal?: AbortSignal;
    readonly plugins?: Readonly<Record<PluginClassId, unknown>>;
}
export declare function createNotifications(store: Pick<Store, "board" | "hasBoard" | "options" | "subscribe">, options?: HolderOptions): Holder;
