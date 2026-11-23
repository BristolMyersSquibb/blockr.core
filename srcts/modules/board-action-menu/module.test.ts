// The tests that core ships for a package's module that draws the menus of
// board actions (../../stubs/menu-module.ts), run against core's own menu,
// with React's act() around what each test does, as core's tests of React set
// it up.

import { describe, test } from "vitest";
import { menuModuleTests } from "../../stubs/index.ts";
import "../../testing/react.ts";
import * as coreMenu from "./index.tsx";

describe("core's menu, under the tests of a module that draws the menus of board actions", () => {
  for (const { name, run } of menuModuleTests(coreMenu)) test(name, run);
});
