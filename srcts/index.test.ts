// @vitest-environment node

// What the modules of packages import from `@blockr/core`: each kind of id by a
// type of its own, each one of the ids that CoreApi's id() gives, so that a
// package names what it passes core's functions and keeps what they give.
// The type check runs these, and vitest runs nothing of them.

import { expectTypeOf, test } from "vitest";
import type { BoardActionId, CommandCall, CommandId, CoreApi, Id, ModuleId, PluginClassId, RegistryId, RendererId } from "./index.ts";

test("exports the type of each id that core's functions take and give", () => {
  expectTypeOf<ReturnType<CoreApi["id"]>>().toEqualTypeOf<Id<string | null>>();
  expectTypeOf<PluginClassId>().toEqualTypeOf<Id>();
  expectTypeOf<CommandId>().toEqualTypeOf<Id>();
  expectTypeOf<BoardActionId>().toEqualTypeOf<Id>();
  expectTypeOf<ModuleId>().toEqualTypeOf<Id>();
  expectTypeOf<RegistryId>().toEqualTypeOf<Id<string | null>>();
  expectTypeOf<RendererId>().toEqualTypeOf<Id<string | null>>();
  expectTypeOf<CommandCall["id"]>().toEqualTypeOf<CommandId>();
});
