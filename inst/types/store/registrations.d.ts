import type { Block, BlockId, Board, OptionId, RegistryId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { OptionModule } from "../types/hosts.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import type { Validate } from "../util/index.ts";
import type { Delta, KeyDelta } from "./delta.ts";
export interface StateKey {
    readonly path: readonly string[];
    readonly kind: "collection" | "value";
    readonly schema: JsonSchema;
    readonly initial?: Json;
    expand?(delta: Delta, board: Board): KeyDelta | undefined;
    check?(value: Json, board: Board): string[];
}
export interface Attribute {
    readonly name: string;
    readonly on: "block" | "front_end";
    readonly schema: JsonSchema;
    readonly initial?: Json;
}
export interface Option {
    readonly id: OptionId;
    readonly label: string;
    readonly category: string;
    readonly schema: JsonSchema;
    readonly default: Json | (() => Promise<Json>);
    readonly ui: OptionModule;
    onChange?(value: Json, core: CoreApi): void;
}
export interface Options {
    register(option: Option): () => void;
    registered(): Readonly<Record<OptionId, Option>>;
}
export interface Saved {
    readonly front_end: {
        readonly state: Json;
    };
    readonly blocks: Readonly<Record<BlockId, {
        readonly registry_id: RegistryId;
        readonly state: Block["state"];
    }>>;
}
export type Migration = (saved: Saved, version: string | null) => Saved;
export interface Checked<T> {
    readonly def: T;
    readonly validate: Validate;
}
export interface Registrations {
    register(def: StateKey | Attribute): () => void;
    registered(): readonly (StateKey | Attribute)[];
    readonly options: Options;
    registerMigration(pkg: string, fn: Migration): () => void;
    keys(): readonly Checked<StateKey>[];
    key(path: readonly string[]): Checked<StateKey> | undefined;
    attributes(): readonly Checked<Attribute>[];
    attribute(on: Attribute["on"], name: string): Checked<Attribute> | undefined;
    option(id: OptionId): Checked<Option> | undefined;
    migrations(): ReadonlyMap<string, Migration>;
}
export declare function createRegistrations(): Registrations;
