import type { Client, Reply, RequestOptions } from "../protocol/index.ts";
import type { Store } from "../store/index.ts";
import type { BlockId, Board } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { Inputs } from "./recipes.ts";
import type { BlockStatus } from "./status.ts";
export interface Scheduler {
    demand(owner: string, blocks: Iterable<BlockId> | ((board: Board) => Iterable<BlockId>), options?: {
        priority?: number;
    }): Demand;
    evaluate(blocks: Iterable<BlockId>, params?: Record<string, Json>, options?: {
        priority?: number;
        signal?: AbortSignal;
    }): Promise<Record<BlockId, BlockStatus>>;
    invalidate(id: BlockId): void;
    status(id: BlockId): BlockStatus | undefined;
    subscribe(fn: (changed: readonly BlockId[]) => void): () => void;
    request(request: Extract<Parameters<Client["request"]>[0], {
        kind: "request" | "output";
    }>, options?: RequestOptions): Promise<{
        reply: Reply;
        body: Blob | null;
    }>;
    inputs(id: BlockId): Inputs | undefined;
    start(): void;
    hold(): void;
    session(): void;
}
export interface Demand {
    set(blocks: Iterable<BlockId> | ((board: Board) => Iterable<BlockId>), options?: {
        priority?: number;
    }): void;
    release(): void;
}
export interface SchedulerOptions {
    readonly store: Pick<Store, "board" | "hasBoard" | "subscribe" | "blockRegistry">;
    readonly client: Pick<Client, "request" | "forget" | "capabilities">;
    readonly evaluationQueue?: number;
    readonly backgroundEvaluationDelay?: number;
    readonly gateVisibility?: boolean;
    readonly bound?: readonly string[];
}
export declare function createScheduler(options: SchedulerOptions): Scheduler;
