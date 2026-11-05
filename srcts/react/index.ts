// The React SDK: what core offers modules written in React, the hooks through
// which they follow their host, and the global under which the one copy of
// React sits on the page.

export { REACT_ENTRIES, REACT_GLOBAL } from "./global.ts";
export { useOutput, useRequest, useValue } from "./hooks.ts";
export type { Answered, Bound, OutputSnapshot, RequestOptions } from "./hooks.ts";
