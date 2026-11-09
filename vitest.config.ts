import { fileURLToPath } from "node:url";
import { defineConfig } from "vitest/config";

// A test that loads a bundle that core builds, such as a block's module in
// inst/js, resolves the bundle's imports of core's React SDK, its binding and
// its controls to their sources, where the page resolves them through its
// import map, and its imports of React to the React in node_modules.
const source = (path: string): string => fileURLToPath(new URL(path, import.meta.url));

export default defineConfig({
  resolve: {
    alias: [
      { find: /^@blockr\/core\/react$/, replacement: source("./srcts/react/index.ts") },
      { find: /^@blockr\/core\/binding$/, replacement: source("./srcts/binding/index.ts") },
      { find: /^@blockr\/core\/controls$/, replacement: source("./srcts/controls/index.ts") },
    ],
  },
  test: {
    environment: "happy-dom",
    include: ["srcts/**/*.test.{ts,tsx}"],
  },
});
