import type { Attributes, Context, Link } from "@opentelemetry/api";
import type { BlockId } from "../types/board.ts";
import type { Capability, CancelRequest, ClientMessage, ForgetRequest, HelloRequest, Progress, Reply, Request } from "./messages.ts";
type DistributiveOmit<T, K extends PropertyKey> = T extends unknown ? Omit<T, K> : never;
export type ProgressNote = Progress["progress"];
export interface RequestOptions {
    body?: Blob;
    progress?: (note: ProgressNote) => void;
    signal?: AbortSignal;
    span?: RequestSpan;
}
export interface RequestSpan {
    readonly parent?: Context;
    readonly links?: readonly Link[];
    readonly attributes?: Attributes;
}
export type Asked = DistributiveOmit<Exclude<Request, HelloRequest | CancelRequest | ForgetRequest>, "id" | "body" | "traceparent">;
export interface Client {
    request(request: Asked, options?: RequestOptions): Promise<{
        reply: Reply;
        body: Blob | null;
    }>;
    forget(blocks: BlockId[]): void;
    capabilities(): ReadonlySet<Capability>;
    hello(): void;
}
export interface ClientOptions {
    hello(): Omit<HelloRequest, "id" | "kind">;
    session(reply: Reply): void;
}
export interface Transport {
    send(message: ClientMessage, bodies: ReadonlyMap<string, Blob>): Promise<void>;
    listen(listener: TransportListener): void;
    sessionId?(): string | undefined;
}
export interface TransportListener {
    started(): void;
    ended(): void;
    received(item: Reply | Progress, body: Promise<Blob> | null): void;
}
export declare class SessionEndedError extends Error {
    constructor(id: string);
}
export declare function sameMajor(a: string, b: string): boolean;
export declare function createClient(transport: Transport, options: ClientOptions): Client;
export {};
