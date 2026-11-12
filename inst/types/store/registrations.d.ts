import type { Command } from "../commands/index.ts";
import type { Block, Board, OptionId, PluginClassId, RegistryId } from "../types/board.ts";
import type { CoreApi, FrontendModule, PluginModule } from "../types/core-api.ts";
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
    update?(delta: Delta, board: Board, plugin: PluginClassId): readonly Patch[];
    migrate?(state: Json, version: number): Json;
    readonly ui?: PluginModule;
    readonly frontend?: FrontendModule;
    readonly storage?: Storage;
    readonly commands?: readonly Command<Json, unknown>[];
    start?(args: {
        readonly core: CoreApi;
        readonly plugin: PluginClassId;
        readonly signal: AbortSignal;
    }): (() => void) | undefined;
}
export interface Storage {
    controls(args: {
        readonly core: CoreApi;
    }): readonly StorageControl[];
    subscribe?(listener: () => void): () => void;
    load?(args: {
        readonly core: CoreApi;
        readonly plugin: PluginClassId;
        readonly signal: AbortSignal;
    }): Promise<string | undefined>;
}
export type StorageControl = Hints & ({
    readonly kind: "button";
    readonly id: string;
    readonly label: string;
    readonly tooltip?: string;
    readonly shortcut?: string;
    readonly disabled?: boolean;
    run(): Promise<void>;
} | {
    readonly kind: "menu";
    readonly id: string;
    readonly label: string;
    readonly items: readonly StorageControl[];
} | {
    readonly kind: "status";
    readonly id: string;
    readonly text: string;
    readonly tooltip?: string;
});
export interface Hints {
    readonly placement?: "end";
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
    readonly default: Json | ((core: CoreApi) => Promise<Json>);
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
    plugin(id: PluginClassId): Checked<PluginClass> | undefined;
    option(id: OptionId): Checked<Option> | undefined;
    migration(registryId: RegistryId): Migration | undefined;
}
export declare function createRegistrations(migration?: (registryId: RegistryId) => Migration | undefined): Registrations;
export declare function isPluginClassId(id: string): boolean;
