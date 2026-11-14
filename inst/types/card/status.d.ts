import type { BlockStatus, Scheduler } from "../scheduler/index.ts";
import type { BlockId } from "../types/board.ts";
import type { CoreApi, Notification } from "../types/core-api.ts";
export declare function useStatus(core: {
    readonly scheduler: Pick<Scheduler, "status" | "subscribe">;
}, block: BlockId): BlockStatus | undefined;
export declare function useNotified(core: {
    readonly notifications: Pick<CoreApi["notifications"], "get" | "subscribe">;
}, block: BlockId | null): readonly Notification[];
export declare function useSized(status: BlockStatus | undefined): boolean;
