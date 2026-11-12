import type { Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Raise } from "../util/index.ts";
export interface Holder {
    readonly api: CoreApi["notifications"];
    readonly raise: Raise;
    follow(scheduler: Pick<Scheduler, "status" | "subscribe">): void;
}
export declare function createNotifications(store: Pick<Store, "board" | "hasBoard" | "subscribe">, signal?: AbortSignal): Holder;
