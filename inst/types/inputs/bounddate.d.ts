import type { Bound } from "../react/index.ts";
import type { Json } from "../types/json.ts";
export interface BoundDate<V> {
    readonly value: V | null;
    readonly missing: string | null;
    readonly heard: (key: string) => void;
    change(value: V | null): void;
    pick(value: V | null): void;
    flush(): void;
}
export interface BoundDateOptions<V> {
    readonly read: (value: Json) => V | undefined;
    readonly write: (value: V) => Json;
    readonly fits: (value: V) => boolean;
    readonly required: boolean;
}
export declare function useBoundDate<V>(bound: Bound, { read, write, fits, required }: BoundDateOptions<V>): BoundDate<V>;
