// The imports that the page R serves resolves through its import map, each to
// a file of its own in inst/js that blockr.core ships: core's React SDK, its
// binding and its controls, OpenTelemetry's API, and core's OpenTelemetry
// module, which core's runtime alone imports, and only where the page sends
// log records or spans to a collector.
// React's entry points, blockr.ui's components and React Aria's three
// packages resolve to files of blockr.ui's, which its own import map names.
// R puts the entries of both packages into the page's one import map
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#the-bundle).
// A module's build leaves these imports as they are, so that its bundle holds
// none of them, and every module on the page imports the one copy of each.
// A module runs the SDK of the core it runs on.
// A package records its own spans through the one copy of OpenTelemetry's API,
// so that they go into the traces of core's spans, as Tracing has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#tracing).

// OpenTelemetry's API, which core's runtime, its OpenTelemetry module and the
// parts of OpenTelemetry's SDK that the module uses import alike.
export const OTEL_API = "@opentelemetry/api";

export const IMPORTS = [
  "@blockr/core/react",
  "@blockr/core/binding",
  "@blockr/core/controls",
  "@blockr/core/otel",
  OTEL_API,
] as const;

export type Import = (typeof IMPORTS)[number];

// The file in inst/js that an import resolves to, named after the import, as
// blockr-core-react.js for `@blockr/core/react` and opentelemetry-api.js for
// `@opentelemetry/api`.
export function importFile(name: Import): string {
  return `${name.replace(/^@/, "").replaceAll("/", "-")}.js`;
}
