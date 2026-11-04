// The types that the modules of packages are written against, which the build
// ships inside blockr.core, in inst/types, where a package's build resolves
// them as `@blockr/core`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#packaging):
// the hosts that core gives the modules it mounts, `CoreApi`, and the types of
// the board, of its updates and of core's outputs. A front-end types what it
// keeps by adding its keys to the FrontEndState interface, as a module
// augmentation of `@blockr/core`. Nothing here runs: a module reaches core's
// runtime through its host, so that one copy of it runs on the page, and the
// stubs of the hosts, which tests run, come from `@blockr/core/stubs`.

export type { Json, JsonSchema } from "./types/json.ts";
export type {
  Block,
  BlockId,
  Board,
  FrontEnd,
  FrontEndState,
  Link,
  LinkId,
  OpeningBoard,
  OptionId,
  RegistryId,
  SavedBoard,
  Stack,
  StackId,
} from "./types/board.ts";
export type { ArgSpec, RegistryEntry } from "./types/registry.ts";
export type {
  BlockOutput,
  OutputFixture,
  OutputFixtures,
  PlotOutput,
  TableColumn,
  TableOutput,
  TableParams,
  TextOutput,
} from "./types/outputs.ts";
export type {
  BlockContext,
  BlockHost,
  BlockModule,
  HostRequestOpts,
  Nested,
  OptionHost,
  OptionModule,
  OutputHost,
  OutputModule,
  Rendered,
  State,
} from "./types/hosts.ts";
export type { BlockRegistry, CoreApi, OutputEvents, RequestOpts } from "./types/core-api.ts";
export type {
  Attribute,
  Change,
  Delta,
  Dropped,
  Entry,
  KeyDelta,
  Migration,
  Option,
  Options,
  Origin,
  Saved,
  StateKey,
  Store,
  StoreError,
} from "./store/index.ts";
export type { Command, CommandError, Commands } from "./commands/index.ts";
export type { BlockStatus, Demand, Scheduler, Status } from "./scheduler/index.ts";
export type { Condition, Lock, Omitted, OutputInfo, Phase } from "./protocol/index.ts";
export type { ModuleError, RequestError } from "./mount/index.ts";
