// The log through which the client's code writes log lines, as Logging has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#logging):
// a plugin logs through CoreApi's log(), which names the package given, and a
// module that core mounts logs through its host's `log`, which names the
// package of the module's block class, renderer or board option already. Each
// line goes to the logger that the page writes lines out with: the browser's
// console, or an OpenTelemetry collector, as the blockr option `logger` picks.

import type { Json } from "./json.ts";

// The levels of a log line, from the most severe. They are R's but for fatal,
// which stops R and has nothing to stop in the client.
export type LogLevel = "error" | "warn" | "info" | "debug" | "trace";

export interface Log {
  // An error, which an Error gives with its stack.
  error(message: string | Error, attributes?: Readonly<Record<string, Json>>): void;
  warn(message: string, attributes?: Readonly<Record<string, Json>>): void;
  info(message: string, attributes?: Readonly<Record<string, Json>>): void;
  debug(message: string, attributes?: Readonly<Record<string, Json>>): void;
  trace(message: string, attributes?: Readonly<Record<string, Json>>): void;
}

// A log line as a logger writes it out, and as the stubs of the hosts hold
// what a module logs.
export interface LogLine {
  readonly level: LogLevel;
  readonly message: string;
  // The package that wrote it, or null for a module of a block class that is
  // defined outside a package.
  readonly package: string | null;
  // The Error that an error was logged with, whose stack goes with it.
  readonly error?: Error;
  // What the line's record holds beside its message, such as the id of a
  // block, as `blockr.block.id`.
  readonly attributes: Readonly<Record<string, Json>>;
  // When it was logged, in milliseconds since the epoch, since the page may
  // write it out later, once it knows its logger.
  readonly time: number;
}
