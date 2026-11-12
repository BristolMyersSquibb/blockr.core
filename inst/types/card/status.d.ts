import type { BlockStatus, Scheduler } from "../scheduler/index.ts";
import type { BlockId } from "../types/board.ts";
export declare function useStatus(core: {
    readonly scheduler: Pick<Scheduler, "status" | "subscribe">;
}, block: BlockId): BlockStatus | undefined;
export declare function useSized(status: BlockStatus | undefined): boolean;
