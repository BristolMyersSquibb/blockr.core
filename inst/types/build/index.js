// srcts/react-global/index.ts
var REACT_GLOBAL = "blockr.core-react";
var REACT_ENTRIES = ["react", "react/jsx-runtime", "react-dom", "react-dom/client"];

// srcts/build/preset.ts
var sharedReact = {
  name: "shared-react",
  setup(build) {
    build.onResolve({ filter: /^react(-dom)?(\/|$)/ }, (args) => {
      if (!REACT_ENTRIES.includes(args.path)) {
        return { errors: [{ text: `A module imports ${args.path}, which core does not share. It shares ${REACT_ENTRIES.join(", ")}.` }] };
      }
      return { path: args.path, namespace: "shared-react" };
    });
    build.onLoad({ filter: /.*/, namespace: "shared-react" }, (args) => ({
      contents: [
        `const shared = globalThis[${JSON.stringify(REACT_GLOBAL)}];`,
        `if (shared === undefined) throw new Error("React is not on the page: core's runtime puts it there before any module loads.");`,
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
  plugins: [sharedReact]
};
export {
  preset,
  sharedReact
};
