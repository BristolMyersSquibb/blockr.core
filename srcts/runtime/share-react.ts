// Puts the one copy of React on the page, under the global that the build
// maps the imports of React in core's runtime and in every module to. The
// build minifies it into the start of the runtime's bundle, so the copy is in
// place before core's own code runs, and core's runtime loads before any
// module.
// It leaves the global open, so that ./share-core.ts can put core's React SDK,
// its binding and its controls beside React and then freeze it.

import * as react from "react";
import * as reactDom from "react-dom";
import * as reactDomClient from "react-dom/client";
import * as jsxRuntime from "react/jsx-runtime";
import { SHARED_GLOBAL } from "../shared-global/index.ts";
import type { REACT_ENTRIES } from "../shared-global/index.ts";

const entries: Record<(typeof REACT_ENTRIES)[number], unknown> = {
  react,
  "react/jsx-runtime": jsxRuntime,
  "react-dom": reactDom,
  "react-dom/client": reactDomClient,
};

if (!Object.hasOwn(globalThis, SHARED_GLOBAL)) {
  Object.defineProperty(globalThis, SHARED_GLOBAL, { value: entries });
}
