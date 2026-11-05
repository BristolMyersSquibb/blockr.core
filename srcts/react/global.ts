// The global under which core's runtime puts the one copy of React on the
// page, by the entry point of each of React's packages that a module imports.
// Core's build maps each import of those entry points in a module to it, so
// that a module's bundle holds no React of its own, as hooks fail on a second
// copy (https://react.dev/warnings/invalid-hook-call-warning).

export const REACT_GLOBAL = "blockr.core-react";

export const REACT_ENTRIES = ["react", "react/jsx-runtime", "react-dom", "react-dom/client"] as const;
