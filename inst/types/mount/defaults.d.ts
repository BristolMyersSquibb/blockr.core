import type { Store } from "../store/index.ts";
import type { Board, OptionId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
export interface Defaults {
    value(id: OptionId, held?: Board["options"]): Json | undefined;
    values(ids: readonly OptionId[], held?: Board["options"], before?: Readonly<Record<OptionId, Json>>): Readonly<Record<OptionId, Json>>;
    settled(ids: readonly OptionId[]): boolean;
    resolve(ids: readonly OptionId[], by: string): Promise<readonly string[]>;
    require(ids: readonly OptionId[], by: string): () => void;
    inUse(): readonly OptionId[];
    subscribe(fn: () => void): () => void;
}
export interface DefaultsOptions {
    readonly store: Pick<Store, "board" | "hasBoard" | "options" | "subscribe">;
    readonly signal?: AbortSignal;
}
export declare function createDefaults(options: DefaultsOptions): Defaults;
