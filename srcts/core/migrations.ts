// The migrations of block classes' states, which each class's UI module
// exports as `migrate`, as Board updates has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.2#plugins):
// 1. Before the store opens or checks a board, core reads the version of
//    each block that the board holds, and of the block's class, from the block
//    registry of R's reply to `hello`.
// 2. For each class of which the board holds a block below the class's
//    version, core imports the class's UI module, without its stylesheet,
//    which loads as core mounts a block of the class.
// 3. It keeps the module's `migrate` for the page, since a module loads once
//    per page, and the store calls it as it opens the board.
// A module that fails to load, that declares another major of core's JS API,
// or whose `migrate` is no function gives no migration, which core logs,
// so the store keeps the state that the board holds, as for a class without
// one.

import { ModuleError, moduleExports } from "../mount/index.ts";
import type { Loader } from "../mount/index.ts";
import type { Migration } from "../store/index.ts";
import type { RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { log } from "../log/index.ts";
import { asError, isObject, own, packageFromId } from "../util/index.ts";

export interface Migrations {
  // The migration of the class with the registry id, which the store reads as
  // it opens a board, or undefined for none.
  get(registryId: RegistryId): Migration | undefined;
  // Imports the UI module of each class of which the board, a JSON board or
  // its text, holds a block below the class's version in the block registry
  // given, and resolves once each has loaded or failed. A text that holds no
  // JSON board needs none, since the store refuses it.
  prepare(board: unknown, registry: Readonly<Record<RegistryId, RegistryEntry>>): Promise<void>;
}

export function createMigrations(loader: Loader): Migrations {
  // The migration that the UI module at each URL exports, or undefined for
  // none, by the class's registry id, as the module loaded.
  const known = new Map<RegistryId, { readonly module: string; readonly migration: Migration | undefined }>();

  async function load(registryId: RegistryId, module: string): Promise<void> {
    const pkg = packageFromId(registryId);
    let migration: Migration | undefined;
    try {
      migration = migrationOf(await loader.load({ module }, pkg), module, pkg);
    } catch (error) {
      log.error(asError(error));
    }
    known.set(registryId, { module, migration });
  }

  return {
    get: (registryId) => known.get(registryId)?.migration,
    async prepare(board, registry) {
      const needed = new Map<RegistryId, string>();
      for (const block of olderBlocks(board, registry)) {
        const entry = own(registry, block);
        if (entry === undefined || known.get(block)?.module === entry.module) continue;
        needed.set(block, entry.module);
      }
      await Promise.all([...needed].map(([registryId, module]) => load(registryId, module)));
    },
  };
}

// The registry ids of the classes of which the board holds a block whose
// version is below its class's.
function olderBlocks(given: unknown, registry: Readonly<Record<RegistryId, RegistryEntry>>): Set<RegistryId> {
  let json = given;
  if (typeof given === "string") {
    try {
      json = JSON.parse(given);
    } catch {
      return new Set();
    }
  }
  const blocks = isObject(json) && isObject(json.board) ? json.board.blocks : undefined;
  const older = new Set<RegistryId>();
  if (!isObject(blocks)) return older;
  for (const block of Object.values(blocks)) {
    if (!isObject(block) || typeof block.registry_id !== "string" || typeof block.version !== "number") continue;
    const version = own(registry, block.registry_id)?.version;
    if (version !== undefined && block.version < version) older.add(block.registry_id);
  }
  return older;
}

// The `migrate` that a block class's UI module exports, or undefined where it
// exports none.
function migrationOf(exports: unknown, module: string, pkg: string | null): Migration | undefined {
  const { migrate } = moduleExports(exports, module, pkg);
  if (migrate === undefined) return undefined;
  if (typeof migrate !== "function") {
    throw new ModuleError(module, pkg, "exports a migrate that is no function.");
  }
  return migrate as Migration;
}
