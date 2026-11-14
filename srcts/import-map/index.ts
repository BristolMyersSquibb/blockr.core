// The imports that the page R serves resolves through its import map, each to
// a file of its own in inst/js that blockr.core ships: the entry points of
// React, core's React SDK, its binding and its controls, OpenTelemetry's API,
// and core's OpenTelemetry module, which core's runtime alone imports, and
// only where the page sends log records or spans to a collector.
// A module's build leaves these imports as they are, so that its bundle holds
// none of them, and every module on the page imports the one copy of each.
// Hooks fail on a second copy of React
// (https://react.dev/warnings/invalid-hook-call-warning),
// and a module runs the SDK of the core it runs on,
// whose stylesheet holds the classes that the SDK's inputs use.
// A package records its own spans through the one copy of OpenTelemetry's API,
// so that they go into the traces of core's spans, as Tracing has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#tracing).

export const REACT_ENTRIES = ["react", "react/jsx-runtime", "react-dom", "react-dom/client"] as const;

// OpenTelemetry's API, which core's runtime, its OpenTelemetry module and the
// parts of OpenTelemetry's SDK that the module uses import alike.
export const OTEL_API = "@opentelemetry/api";

export const IMPORTS = [
  ...REACT_ENTRIES,
  "@blockr/core/react",
  "@blockr/core/binding",
  "@blockr/core/controls",
  "@blockr/core/otel",
  OTEL_API,
] as const;

export type Import = (typeof IMPORTS)[number];

// The file in inst/js that an import resolves to, named after the import, as
// react-dom-client.js for `react-dom/client`, blockr-core-react.js for
// `@blockr/core/react` and opentelemetry-api.js for `@opentelemetry/api`.
export function importFile(name: Import): string {
  return `${name.replace(/^@/, "").replaceAll("/", "-")}.js`;
}
