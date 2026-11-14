import type { BlockStatus } from "../scheduler/index.ts";
import type { BlockId, OptionId, RegistryId } from "../types/board.ts";
import type { Raised } from "../types/core-api.ts";
import type { BlockContext, BlockHost, State } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { LogLine } from "../types/log.ts";
export interface BlockFixture {
    readonly states: readonly State[];
    readonly requests: readonly {
        readonly name: string;
        readonly args: Readonly<Record<string, Json>>;
        readonly options: Readonly<Record<OptionId, Json>>;
        readonly answer: Json;
    }[];
}
export interface StubBlockHostOptions<S extends State = State> {
    readonly state?: S;
    readonly block?: BlockId;
    readonly class?: RegistryId;
    readonly context?: Partial<BlockContext>;
    readonly evaluate?: (params: Record<string, Json>) => BlockStatus | Promise<BlockStatus>;
    readonly modules?: Readonly<Record<string, unknown>>;
}
export interface StubBlockHost<S extends State = State> extends BlockHost<S> {
    readonly submitted: readonly S[];
    readonly requested: readonly {
        readonly name: string;
        readonly args: Readonly<Record<string, Json>>;
        readonly file?: File;
    }[];
    readonly evaluated: readonly Record<string, Json>[];
    readonly notified: readonly Raised[];
    readonly logged: readonly LogLine[];
    setState(state: S): void;
    setContext(context: Partial<BlockContext>): void;
}
export declare function stubBlockHost<S extends State = State>(fixture: BlockFixture, options?: StubBlockHostOptions<S>): StubBlockHost<S>;
