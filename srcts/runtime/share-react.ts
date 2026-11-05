// Puts the one copy of React on the page, which core's runtime bundles, under
// the global that the build maps the modules' imports of React to. Core's
// runtime loads before any module, so the copy is in place before a module
// that imports React loads.

import * as react from "react";
import * as reactDom from "react-dom";
import * as reactDomClient from "react-dom/client";
import * as jsxRuntime from "react/jsx-runtime";
import { REACT_GLOBAL } from "../react/index.ts";
import type { REACT_ENTRIES } from "../react/index.ts";

const entries: Readonly<Record<(typeof REACT_ENTRIES)[number], unknown>> = {
  react,
  "react/jsx-runtime": jsxRuntime,
  "react-dom": reactDom,
  "react-dom/client": reactDomClient,
};

if (!Object.hasOwn(globalThis, REACT_GLOBAL)) {
  Object.defineProperty(globalThis, REACT_GLOBAL, { value: Object.freeze(entries) });
}
