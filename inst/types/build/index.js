// srcts/shared-global/index.ts
var SHARED_GLOBAL = "blockr.core-shared";
var REACT_ENTRIES = ["react", "react/jsx-runtime", "react-dom", "react-dom/client"];
var SHARED_ENTRIES = [
  ...REACT_ENTRIES,
  "@blockr/core/react",
  "@blockr/core/binding",
  "@blockr/core/controls"
];

// srcts/build/preset.ts
var NAMESPACE = "blockr.core-shared";
var shared = {
  name: NAMESPACE,
  setup(build) {
    build.onResolve({ filter: /^(react(-dom)?(\/.*)?|@blockr\/core\/(react|binding|controls))$/ }, (args) => {
      if (!SHARED_ENTRIES.includes(args.path)) {
        return { errors: [{ text: `A module imports ${args.path}, which core does not share. It shares ${SHARED_ENTRIES.join(", ")}.` }] };
      }
      return { path: args.path, namespace: NAMESPACE };
    });
    build.onLoad({ filter: /.*/, namespace: NAMESPACE }, (args) => ({
      contents: [
        `const shared = globalThis[${JSON.stringify(SHARED_GLOBAL)}];`,
        `if (shared === undefined) throw new Error("Core's runtime is not on the page: it puts React and core's SDK there before any module loads.");`,
        `module.exports = shared[${JSON.stringify(args.path)}];`
      ].join("\n"),
      loader: "js"
    }));
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
  plugins: [shared]
};
export {
  preset,
  shared
};
