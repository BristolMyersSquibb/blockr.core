// srcts/import-map/index.ts
var REACT_ENTRIES = ["react", "react/jsx-runtime", "react-dom", "react-dom/client"];
var IMPORTS = [
  ...REACT_ENTRIES,
  "@blockr/core/react",
  "@blockr/core/binding",
  "@blockr/core/controls"
];

// srcts/build/preset.ts
var imports = {
  name: "blockr.core-imports",
  setup(build) {
    build.onResolve({ filter: /^(react(-dom)?(\/.*)?|@blockr\/core\/(react|binding|controls))$/ }, (args) => {
      if (!IMPORTS.includes(args.path)) {
        return {
          errors: [
            {
              text: `A module imports ${args.path}, which the page's import map does not name. It names ${IMPORTS.join(", ")}.`
            }
          ]
        };
      }
      return { path: args.path, external: true };
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
export {
  preset
};
