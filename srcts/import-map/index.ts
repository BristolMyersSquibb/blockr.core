// The imports that the page R serves resolves through its import map, each to
// a file of its own in inst/js that blockr.core ships: the entry points of
// React, core's React SDK, its binding and its controls, and core's
// OpenTelemetry module, which core's runtime alone imports, and only where the
// page sends its log records to a collector.
// A module's build leaves these imports as they are, so that its bundle holds
// none of them, and every module on the page imports the one copy of each.
// Hooks fail on a second copy of React
// (https://react.dev/warnings/invalid-hook-call-warning),
// and a module runs the SDK of the core it runs on,
// whose stylesheet holds the classes that the SDK's inputs use.

export const REACT_ENTRIES = ["react", "react/jsx-runtime", "react-dom", "react-dom/client"] as const;

export const IMPORTS = [
  ...REACT_ENTRIES,
  "@blockr/core/react",
  "@blockr/core/binding",
  "@blockr/core/controls",
  "@blockr/core/otel",
] as const;

export type Import = (typeof IMPORTS)[number];

// The file in inst/js that an import resolves to, named after the import, as
// react-dom-client.js for `react-dom/client` and blockr-core-react.js for
// `@blockr/core/react`.
export function importFile(name: Import): string {
  return `${name.replace(/^@/, "").replaceAll("/", "-")}.js`;
}
