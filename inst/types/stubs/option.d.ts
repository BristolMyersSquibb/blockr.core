import type { OptionId } from "../types/board.ts";
import type { Raised } from "../types/core-api.ts";
import type { OptionHost } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { LogLine } from "../types/log.ts";
export interface Answered {
    readonly name: string;
    readonly args?: Readonly<Record<string, Json>>;
    readonly answer: Json;
}
export interface StubOptionHostOptions<V extends Json = Json> {
    readonly option: OptionId;
    readonly value: V;
    readonly requests?: readonly Answered[];
}
export interface StubOptionHost<V extends Json = Json> extends OptionHost<V> {
    readonly submitted: readonly V[];
    readonly requested: readonly {
        readonly name: string;
        readonly args: Readonly<Record<string, Json>>;
    }[];
    readonly notified: readonly Raised[];
    readonly logged: readonly LogLine[];
    setValue(value: V): void;
}
export declare function stubOptionHost<V extends Json = Json>(options: StubOptionHostOptions<V>): StubOptionHost<V>;
export declare function answer<T>(answers: readonly Answered[], name: string, args: Readonly<Record<string, Json>>, of: string): Promise<T>;
