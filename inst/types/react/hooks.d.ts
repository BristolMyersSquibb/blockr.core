import type { Condition } from "../protocol/index.ts";
import type { BlockStatus, Scheduler } from "../scheduler/index.ts";
import type { BlockId, Board } from "../types/board.ts";
import type { CoreApi, CoreFailure, Notification } from "../types/core-api.ts";
import type { BlockHost, OptionHost, OutputHost, State } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
export interface OutputSnapshot {
    readonly output: Json;
    readonly body: Blob | null;
    readonly params: Json;
    readonly loading: boolean;
    readonly error: Condition | CoreFailure | null;
}
export declare function useOutput(host: OutputHost): OutputSnapshot;
export interface Bound {
    readonly value: Json | undefined;
    readonly error: string | null;
    get(): Json | undefined;
    commit(value: Json): Promise<boolean>;
    unset(): Promise<boolean>;
    type(value: Json | undefined | ((held: Json | undefined) => Json | undefined)): void;
    flush(): void;
}
export declare function useValue<S extends State>(host: BlockHost<S>, key: Extract<keyof S, string>): Bound;
export declare function useValue<V extends Json>(host: OptionHost<V>): Bound;
export interface Answered {
    readonly answer: Json | Blob | undefined;
    readonly error: string | null;
    readonly loading: boolean;
}
export interface RequestOptions {
    readonly args?: Readonly<Record<string, Json>>;
    readonly reads?: Json;
}
export declare function useRequest<S extends State>(host: BlockHost<S>, name: string, options?: RequestOptions): Answered;
export declare function useRequest<V extends Json>(host: OptionHost<V>, name: string, options?: RequestOptions): Answered;
export declare function useBoard(core: Pick<CoreApi, "store">): Board;
export declare function useStatus(core: {
    readonly scheduler: Pick<Scheduler, "status" | "subscribe">;
}, block: BlockId): BlockStatus | undefined;
export declare function useNotified(core: {
    readonly notifications: Pick<CoreApi["notifications"], "get" | "subscribe">;
}, block: BlockId | null): readonly Notification[];
