import type { Condition } from "../protocol/messages.ts";
import type { BlockId, OptionId } from "../types/board.ts";
import type { OutputHost } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { OutputFixtures } from "../types/outputs.ts";
import type { Answered } from "./option.ts";
export interface StubOutputHostOptions {
    readonly output?: Json;
    readonly body?: Blob | null;
    readonly params?: Json;
    readonly block?: BlockId;
    readonly fixtures?: OutputFixtures;
    readonly options?: Readonly<Record<OptionId, Json>>;
    readonly requests?: readonly Answered[];
}
export interface Drawn {
    readonly key: string;
    readonly el: HTMLElement;
    readonly outputs: readonly Json[];
    readonly removed: boolean;
}
export interface StubOutputHost extends OutputHost {
    readonly emitted: readonly {
        readonly name: string;
        readonly payload: Json;
    }[];
    readonly asked: readonly Json[];
    readonly requested: readonly {
        readonly name: string;
        readonly args: Readonly<Record<string, Json>>;
    }[];
    readonly drawn: readonly Drawn[];
    setOutput(output: Json, body?: Blob | null, params?: Json): void;
    setLoading(loading: boolean): void;
    setError(error: Condition | null): void;
}
export declare function stubOutputHost(options?: StubOutputHostOptions): StubOutputHost;
