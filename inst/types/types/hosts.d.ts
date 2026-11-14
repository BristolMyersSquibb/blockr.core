import type { Condition } from "../protocol/index.ts";
import type { BlockStatus } from "../scheduler/index.ts";
import type { BlockId, OptionId, RegistryId } from "./board.ts";
import type { Raised, RequestOpts } from "./core-api.ts";
import type { Json } from "./json.ts";
import type { Log } from "./log.ts";
export type State = Readonly<Record<string, Json>>;
export interface BlockModule<S extends State = State> {
    readonly options?: readonly OptionId[];
    render(args: {
        el: HTMLElement;
        host: BlockHost<S>;
        signal: AbortSignal;
    }): Rendered;
}
export interface OptionModule<V extends Json = Json> {
    render(args: {
        el: HTMLElement;
        host: OptionHost<V>;
        signal: AbortSignal;
    }): Rendered;
}
export interface OutputModule {
    readonly options?: readonly OptionId[];
    render(args: {
        el: HTMLElement;
        host: OutputHost;
        signal: AbortSignal;
    }): Rendered;
}
export type Rendered = (() => void) | undefined | Promise<(() => void) | undefined>;
export interface BlockHost<S extends State = State> {
    readonly block: BlockId;
    readonly class: RegistryId;
    state(): S;
    context(): BlockContext;
    on(event: "state" | "context", fn: () => void): () => void;
    submit(change: Partial<S>): Promise<void>;
    request<T = Json>(name: string, args?: Partial<S> & Record<string, Json>, opts?: HostRequestOpts): Promise<T | Blob>;
    evaluate(params: Record<string, Json>): Promise<BlockStatus>;
    module<T = unknown>(id: string): Promise<T>;
    notify(notification: Raised): () => void;
    readonly log: Log;
}
export interface BlockContext {
    readonly name: string;
    readonly status: BlockStatus;
    readonly inputs: readonly {
        readonly input: string;
        readonly revision: number;
    }[];
    readonly options: Readonly<Record<OptionId, Json>>;
}
export type HostRequestOpts = Omit<RequestOpts, "reads">;
export interface OutputRequestOpts extends HostRequestOpts {
    scope?: "class" | "renderer";
}
export interface OptionHost<V extends Json = Json> {
    readonly option: OptionId;
    value(): V;
    on(event: "value", fn: () => void): () => void;
    submit(value: V): Promise<void>;
    request<T = Json>(name: string, args?: Record<string, Json>, opts?: HostRequestOpts): Promise<T | Blob>;
    notify(notification: Raised): () => void;
    readonly log: Log;
}
export interface OutputHost {
    readonly block: BlockId;
    output(): Json;
    body(): Blob | null;
    params(): Json;
    options(): Readonly<Record<OptionId, Json>>;
    loading(): boolean;
    error(): Condition | null;
    on(event: "output", fn: () => void): () => void;
    setParams(params: Json): void;
    request<T = Json>(name: string, args?: Record<string, Json>, opts?: OutputRequestOpts): Promise<T | Blob>;
    emit(name: string, payload: Json): void;
    notify(notification: Raised): () => void;
    readonly log: Log;
}
