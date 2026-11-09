// The global under which core's runtime puts what it shares with every module
// on the page, by the import that reaches each.
// It holds the one copy of React, by the entry point of each of React's
// packages, and core's React SDK, its binding and its controls, by their names
// in `@blockr/core`.
// The build preset maps each such import in a module to it, so that a module's
// bundle holds none of them.
// Hooks fail on a second copy of React
// (https://react.dev/warnings/invalid-hook-call-warning),
// and a module runs the SDK of the core it runs on,
// whose stylesheet holds the classes that the SDK's inputs use.

export const SHARED_GLOBAL = "blockr.core-shared";

export const REACT_ENTRIES = ["react", "react/jsx-runtime", "react-dom", "react-dom/client"] as const;

export const SHARED_ENTRIES = [
  ...REACT_ENTRIES,
  "@blockr/core/react",
  "@blockr/core/binding",
  "@blockr/core/controls",
] as const;
