import type { BlockClassRequest, ProgressNote, Reply } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
export type ClassRequest = Omit<BlockClassRequest, "id" | "body">;
export interface Ask {
    readonly request: ClassRequest;
    readonly body?: Blob;
    readonly share: "keep" | "merge" | "once";
}
export interface Answer {
    readonly reply: Reply;
    readonly body: Blob | null;
}
export interface Answers {
    ask(ask: Ask, options?: {
        signal?: AbortSignal;
        progress?: (note: ProgressNote) => void;
    }): Promise<Answer>;
    clear(): void;
    size(): number;
}
export declare function createAnswers(scheduler: Pick<Scheduler, "request" | "status" | "subscribe">): Answers;
