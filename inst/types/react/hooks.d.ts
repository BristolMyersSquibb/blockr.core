import type { Condition } from "../protocol/index.ts";
import type { OptionId } from "../types/board.ts";
import type { BlockHost, OptionHost, OutputHost, State } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
export interface OutputSnapshot {
    readonly output: Json;
    readonly body: Blob | null;
    readonly params: Json;
    readonly options: Readonly<Record<OptionId, Json>>;
    readonly loading: boolean;
    readonly error: Condition | null;
}
export declare function useOutput(host: OutputHost): OutputSnapshot;
export interface Bound {
    readonly value: Json | undefined;
    readonly error: string | null;
    get(): Json | undefined;
    commit(value: Json): Promise<boolean>;
    type(value: Json | undefined, pause?: boolean): void;
    flush(): void;
}
export declare function useValue<S extends State>(host: BlockHost<S>, key: Extract<keyof S, string>): Bound;
export declare function useValue<V extends Json>(host: OptionHost<V>): Bound;
export interface Answered {
    readonly answer: Json | Blob | undefined;
    readonly error: string | null;
}
export interface RequestOptions {
    readonly args?: Readonly<Record<string, Json>>;
    readonly reads?: Json;
}
export declare function useRequest<S extends State>(host: BlockHost<S>, name: string, options?: RequestOptions): Answered;
export declare function useRequest<V extends Json>(host: OptionHost<V>, name: string, options?: RequestOptions): Answered;
