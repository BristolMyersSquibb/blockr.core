import type { Block, Board, OptionId, PluginClassId, PluginId, RegistryId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { OptionModule } from "../types/hosts.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import type { Validate } from "../util/index.ts";
import type { Delta, Patch } from "./delta.ts";
export interface PluginClass {
    readonly id: PluginClassId;
    readonly options?: readonly OptionId[];
    readonly state?: {
        readonly schema: JsonSchema;
        readonly default: Json;
    };
    update?(delta: Delta, board: Board, plugin: PluginId): readonly Patch[];
    migrate?(state: Json, version: number): Json;
    readonly ui?: string;
    readonly frontend?: string;
    readonly storage?: Storage;
    start?(args: {
        readonly core: CoreApi;
        readonly plugin: PluginId;
        readonly signal: AbortSignal;
    }): (() => void) | undefined;
}
export interface Storage {
    save(board: string): Promise<void>;
    open(): Promise<string | null>;
}
export interface Plugins {
    register(def: PluginClass): () => void;
    registered(): Readonly<Record<PluginClassId, PluginClass>>;
}
export interface Option {
    readonly id: OptionId;
    readonly label: string;
    readonly category: string;
    readonly schema: JsonSchema;
    readonly default: Json | (() => Promise<Json>);
    readonly ui: OptionModule;
    onChange?(value: Json, core: CoreApi): void;
    migrate?(value: Json, version: number): Json;
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
export interface Registrations {
    readonly plugins: Plugins;
    readonly options: Options;
    registerMigration(registryId: RegistryId, fn: Migration): () => void;
    plugin(id: PluginClassId): Checked<PluginClass> | undefined;
    option(id: OptionId): Checked<Option> | undefined;
    migration(registryId: RegistryId): Migration | undefined;
}
export declare const BASIC_OPTIONS: readonly OptionId[];
export declare function createRegistrations(): Registrations;
export declare function isPluginClassId(id: string): boolean;
