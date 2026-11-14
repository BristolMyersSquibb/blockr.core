// The types that the modules of packages are written against, which the build
// ships inside blockr.core, in inst/types, where a package's build resolves
// them as `@blockr/core`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#packaging):
// the hosts that core gives the modules it mounts, `CoreApi`, the module of a
// plugin, the log that each writes its lines through, and the types of the
// board, of its updates and of core's outputs.
// A plugin class defines itself as a `PluginClass`, and types the state of its
// plugins itself, with `type` rather than `interface`, as
// `type DockState = { views: ... }`, since only such a type is assignable to
// the `Json` that core holds a plugin's state as.
// Nothing here runs: a module reaches core's
// runtime through its host, so that one copy of it runs on the page, and the
// stubs of the hosts, which tests run, come from `@blockr/core/stubs`.

export type { Json, JsonSchema } from "./types/json.ts";
export type {
  Block,
  BlockId,
  Board,
  JsonBlock,
  JsonOption,
  JsonBoard,
  Link,
  LinkId,
  OptionId,
  PluginClassId,
  RegistryId,
  Stack,
  StackId,
} from "./types/board.ts";
export type { ArgSpec, RegistryEntry } from "./types/registry.ts";
export type { Log, LogLevel, LogLine } from "./types/log.ts";
export type {
  BlockOutput,
  OutputFixture,
  OutputFixtures,
  OutputRenderer,
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
  OptionHost,
  OptionModule,
  OutputHost,
  OutputModule,
  OutputRequestOpts,
  Rendered,
  State,
} from "./types/hosts.ts";
export type {
  BlockRegistry,
  BoardObject,
  CoreApi,
  CoreCode,
  CoreSource,
  EvaluatePhase,
  FrontendModule,
  ModuleSource,
  Notification,
  NotificationSource,
  OutputEvents,
  Plugin,
  PluginModule,
  Raised,
  RequestOpts,
  RRequest,
  RSource,
  Severity,
  SourceModule,
} from "./types/core-api.ts";
export type {
  Change,
  Delta,
  Dropped,
  Hints,
  Migration,
  Option,
  Options,
  Origin,
  Patch,
  PluginClass,
  Plugins,
  Storage,
  StorageControl,
  Store,
  StoreError,
} from "./store/index.ts";
export type { Command, CommandContext, CommandError, Commands } from "./commands/index.ts";
export type { BlockStatus, Demand, Scheduler, Status } from "./scheduler/index.ts";
export type { Condition, Lock, OutputInfo, Phase } from "./protocol/index.ts";
export type { ModuleError, RequestError } from "./mount/index.ts";
