// srcts/build/preset.ts
import { preset as ui } from "@blockr/ui/build";

// srcts/import-map/index.ts
var OTEL_API = "@opentelemetry/api";
var IMPORTS = [
  "@blockr/core/react",
  "@blockr/core/binding",
  "@blockr/core/controls",
  "@blockr/core/otel",
  OTEL_API
];

// srcts/build/preset.ts
var CORE = new RegExp(`^(${IMPORTS.map((name) => name.replace(/[/.]/g, String.raw`\$&`)).join("|")})$`);
var imports = {
  name: "blockr.core-imports",
  setup(build) {
    build.onResolve({ filter: CORE }, (args) => ({
      path: args.path,
      external: true,
      ...args.path === OTEL_API ? { sideEffects: false } : {}
    }));
  }
};
var preset = {
  ...ui,
  plugins: [imports, ...ui.plugins]
};
export {
  preset
};
