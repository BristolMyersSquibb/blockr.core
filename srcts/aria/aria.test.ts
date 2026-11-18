// The parts of React Aria that core's code draws with: one copy of the
// package of hooks, react-aria, which core imports the portal provider from,
// and which React Aria's components use, so that they read the container
// that the provider gives.

import { createRequire } from "node:module";
import { expect, test } from "vitest";

test("React Aria's components use the copy of react-aria that core imports", () => {
  const require = createRequire(import.meta.url);
  const components = createRequire(require.resolve("react-aria-components/package.json"));
  expect(components.resolve("react-aria/package.json")).toBe(require.resolve("react-aria/package.json"));
});
