import type { Loader } from "../mount/index.ts";
import type { Migration } from "../store/index.ts";
import type { RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
export interface Migrations {
    get(registryId: RegistryId): Migration | undefined;
    prepare(board: unknown, registry: Readonly<Record<RegistryId, RegistryEntry>>): Promise<void>;
}
export declare function createMigrations(loader: Loader): Migrations;
