// A stub of each host that core gives the modules it mounts, which the build
// ships beside core's types, as `@blockr/core/stubs`, so that a module mounts
// in a test runner with a state, requests answered from fixtures and a record
// of what it submitted, needing neither R nor a browser
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#testing).

export { stubBlockHost } from "./block.ts";
export type { BlockFixture, StubBlockHost, StubBlockHostOptions } from "./block.ts";
export { stubOptionHost } from "./option.ts";
export type { Answered, StubOptionHost, StubOptionHostOptions } from "./option.ts";
export { stubOutputHost } from "./output.ts";
export type { Drawn, StubOutputHost, StubOutputHostOptions } from "./output.ts";
export { mount } from "./mount.ts";
export type { Mounted } from "./mount.ts";
export { READY } from "./util.ts";
