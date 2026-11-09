// Reading blockr options, which the client asks R for by name, since it can
// read neither an R option nor an environment variable of the server, as
// Blockr options has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.9#blockr-options):
// - Core asks R for a blockr option the first time code reads it, with a
//   `blockr_options` request. The names that code reads at once, within one
//   task, go in one request.
// - Core keeps each value for the page, since a blockr option is fixed for the
//   app.
// - Code reads a blockr option with a fallback, which it takes where R has no
//   value. R sends a value as it is set, so that one an environment variable
//   sets arrives as a string, which core converts to the fallback's type, a
//   number or a logical. R writes Inf as null, which core reads as Infinity
//   where the fallback is a number.
// - Once core has stopped, as where R speaks another major of the protocol, a
//   read that R did not answer takes its fallback, and core reports nothing.

import { SessionEndedError } from "../protocol/index.ts";
import type { Client } from "../protocol/index.ts";
import type { Json } from "../types/json.ts";
import { messageOf, quoted, report } from "../util/index.ts";

export interface BlockrOptions {
  // The blockr option's value, asked of R once per page, or the fallback where
  // R has none.
  read<T extends Json>(name: string, fallback: T): Promise<T>;
}

// What R sent for a blockr option, or null where it sent nothing.
type Found = { readonly value: Json } | null;

// The signal aborts as core stops.
export function createBlockrOptions(client: Pick<Client, "request">, signal?: AbortSignal): BlockrOptions {
  const found = new Map<string, Promise<Found>>();
  // The names that code has read in this task, which go to R together once it
  // ends.
  let batch: Map<string, (value: Found) => void> | undefined;

  function ask(name: string): Promise<Found> {
    let asked = found.get(name);
    if (asked === undefined) {
      asked = new Promise<Found>((resolve) => {
        if (batch === undefined) {
          const sending = new Map<string, (value: Found) => void>();
          batch = sending;
          // A promise rather than a microtask, which core reports errors from.
          void Promise.resolve().then(() => {
            if (batch === sending) batch = undefined;
            void send(sending);
          });
        }
        batch.set(name, resolve);
      });
      found.set(name, asked);
    }
    return asked;
  }

  async function send(names: ReadonlyMap<string, (value: Found) => void>): Promise<void> {
    const asked = [...names.keys()];
    let values: Readonly<Record<string, Json>>;
    try {
      values = await valuesOf(client, asked);
    } catch (error) {
      if (signal?.aborted !== true) {
        report(new Error(`R sent no values of the blockr options ${quoted(asked)}, so core takes their fallbacks: ${messageOf(error)}`, { cause: error }));
      }
      // They are asked again the next time code reads them.
      for (const [name, settle] of names) {
        found.delete(name);
        settle(null);
      }
      return;
    }
    for (const [name, settle] of names) {
      settle(Object.hasOwn(values, name) ? { value: values[name] ?? null } : null);
    }
  }

  return {
    async read<T extends Json>(name: string, fallback: T): Promise<T> {
      if (typeof name !== "string" || name === "") {
        throw new TypeError("A blockr option is read by its name, a non-empty string.");
      }
      return converted(name, await ask(name), fallback);
    },
  };
}

// The values that R has of the blockr options, by name, asked again of the
// next session where the session ends before R replies, since reading them
// changes nothing.
async function valuesOf(client: Pick<Client, "request">, names: string[]): Promise<Readonly<Record<string, Json>>> {
  for (;;) {
    let reply;
    try {
      ({ reply } = await client.request({ kind: "blockr_options", names }));
    } catch (error) {
      if (error instanceof SessionEndedError) continue;
      throw error;
    }
    if (reply.status !== "ok" || !("values" in reply)) {
      throw new Error(`R replied ${reply.status}.`);
    }
    return reply.values;
  }
}

// The value as code reads it: the fallback where R sent none, a string or null
// converted where the fallback is a number or a logical, and otherwise the
// value as R sent it.
function converted<T extends Json>(name: string, found: Found, fallback: T): T {
  if (found === null) return fallback;
  const { value } = found;
  if (typeof fallback === "number") {
    if (value === null) return Infinity as T;
    const number = typeof value === "string" ? numberOf(value) : value;
    if (typeof number === "number" && !Number.isNaN(number)) return number as T;
  } else if (typeof fallback === "boolean") {
    const logical = typeof value === "string" ? logicalOf(value) : value;
    if (typeof logical === "boolean") return logical as T;
  } else if (typeof fallback === "string") {
    if (typeof value === "string") return value as T;
  } else {
    return value as T;
  }
  report(
    new Error(
      `The blockr option ${name} is ${JSON.stringify(value)}, which is no ${typeof fallback}, so core takes its fallback ${JSON.stringify(fallback)}.`,
    ),
  );
  return fallback;
}

// A number as R's as.numeric() reads one from text, or NaN.
function numberOf(text: string): number {
  const trimmed = text.trim();
  if (/^[+-]?inf(inity)?$/i.test(trimmed)) return trimmed.startsWith("-") ? -Infinity : Infinity;
  return trimmed === "" ? NaN : Number(trimmed);
}

// A logical as R's as.logical() reads one from text, or undefined.
function logicalOf(text: string): boolean | undefined {
  const trimmed = text.trim();
  if (["TRUE", "true", "True", "T"].includes(trimmed)) return true;
  if (["FALSE", "false", "False", "F"].includes(trimmed)) return false;
  return undefined;
}
