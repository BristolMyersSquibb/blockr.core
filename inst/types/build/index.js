// srcts/import-map/index.ts
var REACT_ENTRIES = ["react", "react/jsx-runtime", "react-dom", "react-dom/client"];
var OTEL_API = "@opentelemetry/api";
var IMPORTS = [
  ...REACT_ENTRIES,
  "@blockr/core/react",
  "@blockr/core/binding",
  "@blockr/core/controls",
  "@blockr/core/otel",
  OTEL_API
];

// srcts/build/preset.ts
var imports = {
  name: "blockr.core-imports",
  setup(build) {
    build.onResolve({ filter: /^(react(-dom)?(\/.*)?|@blockr\/core\/(react|binding|controls|otel)|@opentelemetry\/api)$/ }, (args) => {
      if (!IMPORTS.includes(args.path)) {
        return {
          errors: [
            {
              text: `A module imports ${args.path}, which the page's import map does not name. It names ${IMPORTS.join(", ")}.`
            }
          ]
        };
      }
      return { path: args.path, external: true, ...args.path === OTEL_API ? { sideEffects: false } : {} };
    });
  }
};
var preset = {
  bundle: true,
  format: "esm",
  platform: "browser",
  target: "es2022",
  jsx: "automatic",
  // React and the libraries built on it read this to leave out what they
  // check only while an app is developed.
  define: { "process.env.NODE_ENV": JSON.stringify("production") },
  plugins: [imports]
};

// srcts/build/vendor-check.ts
var vendorCheck = {
  name: "blockr.core-vendor-check",
  setup(build) {
    build.initialOptions.metafile = true;
    build.onEnd((result) => ({
      errors: bundledPackages(result.metafile).map(({ name, file }) => ({
        text: `The build put code of ${name}, from node_modules, into ${file}. Bundle ${name} into the package's vendor file instead, and import it from there.`
      }))
    }));
  }
};
function bundledPackages(metafile) {
  return Object.entries(metafile?.outputs ?? {}).flatMap(([file, output]) => {
    const names = new Set(
      Object.entries(output.inputs).flatMap(([input, { bytesInOutput }]) => {
        const name = inputPackage(input);
        return name === void 0 || bytesInOutput === 0 ? [] : [name];
      })
    );
    return [...names].map((name) => ({ name, file }));
  });
}
function inputPackage(input) {
  const at = input.lastIndexOf("node_modules/");
  if (at === -1) return void 0;
  const [first = "", second = ""] = input.slice(at + "node_modules/".length).split("/");
  return first.startsWith("@") ? `${first}/${second}` : first;
}
export {
  preset,
  vendorCheck
};
