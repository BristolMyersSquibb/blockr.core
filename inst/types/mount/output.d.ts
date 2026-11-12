import type { Store } from "../store/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { OutputInfo, OutputReply, Reply } from "../protocol/index.ts";
import type { BlockId, OptionId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { Raise } from "../util/index.ts";
import type { Answers } from "./answers.ts";
import type { Loader } from "./load.ts";
import type { Measure } from "./measure.ts";
export interface OutputEvent {
    readonly block: BlockId;
    readonly name: string;
    readonly payload: Json;
}
interface Asked {
    readonly revision: number;
    readonly options: Readonly<Record<OptionId, Json>>;
    readonly info: OutputInfo;
    readonly params: Readonly<Record<string, Json>>;
}
interface Made {
    readonly asked: Asked;
    readonly output: OutputReply["output"];
    readonly body: Blob | null;
}
interface Flight {
    readonly done: Promise<{
        reply: Reply;
        body: Blob | null;
    }>;
    readonly controller: AbortController;
    callers: number;
}
interface Shelf {
    params: Readonly<Record<string, Json>>;
    last: Made | undefined;
    failure: (() => void) | undefined;
    readonly mounts: Set<() => void>;
    readonly flights: Map<string, Flight>;
}
export interface Shelves {
    of(block: BlockId): Shelf;
    drop(block: BlockId): void;
}
export declare function createShelves(): Shelves;
export interface OutputDeps {
    readonly store: Pick<Store, "board" | "blockRegistry" | "options" | "subscribe">;
    readonly scheduler: Pick<Scheduler, "request" | "status" | "subscribe">;
    readonly loader: Loader;
    readonly answers: Answers;
    readonly shelves: Shelves;
    readonly measure: Measure;
    readonly replaced: AbortSignal;
    readonly addOptions: (ids: readonly OptionId[], by: string) => Promise<readonly string[]>;
    readonly keepOptions: (ids: readonly OptionId[], by: string, failed: (failures: readonly string[]) => void) => {
        readonly stop: () => void;
    };
    readonly resolveDefaults: (ids: readonly OptionId[], by: string) => Promise<Readonly<Record<OptionId, Json>>>;
    readonly emit: (event: OutputEvent) => void;
    readonly raise: Raise;
}
export declare function mountOutput(deps: OutputDeps, el: HTMLElement, block: BlockId): () => void;
export {};
