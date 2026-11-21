import { fileURLToPath } from "node:url";
import { defineConfig } from "vitest/config";

// A test that loads a bundle that core builds, such as a block's module in
// inst/js, resolves the bundle's imports of core's React SDK, its binding, its
// controls and its OpenTelemetry module to their sources, where the page resolves them through its
// import map, and its imports of React to the React in node_modules.
const source = (path: string): string => fileURLToPath(new URL(path, import.meta.url));

export default defineConfig({
  resolve: {
    alias: [
      { find: /^@blockr\/core\/react$/, replacement: source("./srcts/react/index.ts") },
      { find: /^@blockr\/core\/binding$/, replacement: source("./srcts/binding/index.ts") },
      { find: /^@blockr\/core\/controls$/, replacement: source("./srcts/controls/index.ts") },
      { find: /^@blockr\/core\/otel$/, replacement: source("./srcts/otel/index.ts") },
    ],
  },
  test: {
    environment: "happy-dom",
    include: ["srcts/**/*.test.{ts,tsx}"],
    // A test that walks a UI through several of its selects, which React Aria
    // draws, takes a second or two alone, and longer while the other test files
    // run beside it.
    testTimeout: 15_000,
    // Fails a test that logs an error that it does not capture.
    setupFiles: ["./srcts/testing/setup.ts"],
    // Every run measures the coverage of the tests, `npm test` included.
    // The typescript job of blockr.ci uploads coverage/lcov.info to Codecov.
    // The lcov reporter writes that file, and an HTML report beside it in
    // coverage/lcov-report.
    // Istanbul counts the code of the sources in srcts alone.
    // V8 counts all the code that runs, TypeScript's compiler included.
    // That makes the tests that run the compiler several times slower.
    // With `include` set, a source in srcts that no test imports counts as
    // uncovered.
    // Vitest leaves out the test files and the setup file by itself.
    // The `exclude` option leaves out the other helpers of the tests, in
    // srcts/testing.
    coverage: {
      enabled: true,
      provider: "istanbul",
      reporter: ["text", "lcov"],
      include: ["srcts/**/*.{ts,tsx}"],
      exclude: ["srcts/testing/**"],
    },
  },
});
