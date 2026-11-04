import type { Condition, ReplyStatus } from "../protocol/index.ts";
import type { BlockStatus, Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { BlockId } from "../types/board.ts";
import type { HostRequestOpts } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { RegistryEntry } from "../types/registry.ts";
import type { Answers } from "./answers.ts";
export declare class RequestError extends Error {
    readonly status: Exclude<ReplyStatus, "ok"> | null;
    readonly conditions: readonly Condition[];
    readonly inputs: readonly {
        readonly input: string;
        readonly status: BlockStatus;
    }[];
    constructor(message: string, fields?: {
        status?: Exclude<ReplyStatus, "ok">;
        conditions?: readonly Condition[];
        inputs?: readonly {
            readonly input: string;
            readonly status: BlockStatus;
        }[];
    });
}
export interface RequestDeps {
    readonly store: Pick<Store, "board">;
    readonly scheduler: Pick<Scheduler, "evaluate" | "inputs">;
    readonly answers: Answers;
    readonly bound: () => boolean;
    readonly unboundError: () => Error;
}
export declare function blockRequest(deps: RequestDeps, block: BlockId, entry: RegistryEntry, name: string, given: Readonly<Record<string, Json | undefined>>, opts: HostRequestOpts): Promise<Json | Blob>;
