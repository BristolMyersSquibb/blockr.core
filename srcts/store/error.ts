// The failures of the store: the error that it throws where it refuses
// something, and the failure for what a function of a package threw as the
// store called it.

import { log } from "../log/index.ts";
import type { Json } from "../types/json.ts";
import { errorMessage } from "../util/index.ts";

// What the store throws where it refuses a delta, an undo or redo, or a board
// to open, with each failure in a sentence.
export class StoreError extends Error {
  readonly failures: readonly string[];

  constructor(message: string, failures: readonly string[]) {
    super([message, ...failures.map((failure) => `- ${failure}`)].join("\n"));
    this.name = "StoreError";
    this.failures = failures;
  }
}

// The failure for what a function of a package threw as the store called it:
// the `update` or the `afterStep` of a plugin class (./expand.ts), or a
// migration (./open.ts). The failure is `of` followed by the message of what
// was thrown.
// The store logs the failure at error, with what was thrown as the cause.
// Core logs every failure that it catches in this way
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#logging).
// The line's `attributes` name the plugin class, the block class or the board
// option.
// The refusal holds the message alone, so the stack reaches the log only here.
export function thrown(of: string, error: unknown, attributes: Readonly<Record<string, Json>>): string {
  const failure = `${of}: ${errorMessage(error)}`;
  log.error(new Error(failure, { cause: error }), attributes);
  return failure;
}
