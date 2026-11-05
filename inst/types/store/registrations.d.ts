import type { Block, Board, OptionId, RegistryId } from "../types/board.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import type { Validate } from "../util/index.ts";
import type { Delta, Patch } from "./delta.ts";
export interface BoardClass {
    readonly class: string;
    readonly parent?: string;
    readonly options?: readonly OptionId[];
    readonly properties: Readonly<Record<string, Property>>;
    migrate?(properties: Readonly<Record<string, Json>>, version: number): Readonly<Record<string, Json>>;
    update?(delta: Delta, board: Board): readonly Patch[];
}
export interface Property {
    readonly schema: JsonSchema;
    readonly default: Json;
}
export interface Option {
    readonly id: OptionId;
    readonly label: string;
    readonly category: string;
    readonly schema: JsonSchema;
    readonly default: Json | (() => Promise<Json>);
}
export interface Options {
    register(option: Option): () => void;
    registered(): Readonly<Record<OptionId, Option>>;
}
export type Migration = (state: Block["state"], version: number) => Block["state"];
export interface Checked<T> {
    readonly def: T;
    readonly validate: Validate;
}
export interface CheckedClass {
    readonly def: BoardClass;
    readonly validate: Readonly<Record<string, Validate>>;
}
export interface Built {
    readonly def: BoardClass;
    readonly chain: readonly CheckedClass[];
    readonly properties: ReadonlyMap<string, {
        readonly owner: CheckedClass;
        readonly validate: Validate;
    }>;
}
export interface Registrations {
    register(def: BoardClass): () => void;
    registered(): readonly BoardClass[];
    readonly options: Options;
    registerMigration(registryId: RegistryId, fn: Migration): () => void;
    builtAs(classes: readonly string[]): Built;
    option(id: OptionId): Checked<Option> | undefined;
    migration(registryId: RegistryId): Migration | undefined;
}
export declare const CORE_BOARD: BoardClass;
export declare const CORE_PROPERTIES: readonly string[];
export declare function createRegistrations(): Registrations;
export declare function recordOf(board: Board): Readonly<Record<string, unknown>>;
export declare function propertiesOf(board: Board): Readonly<Record<string, unknown>>;
export declare function defaultOf(built: Built, name: string): Json | undefined;
