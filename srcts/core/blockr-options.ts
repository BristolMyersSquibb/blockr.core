// Reading blockr options, which the client asks R for by name, since it can
// read neither an R option nor an environment variable of the server, as
// Blockr options has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.9#blockr-options):
// - Core asks R for a blockr option the first time code reads it, with a
//   `blockr_options` request. The names that code reads at once, within one
//   task, go in one request.
// - Core keeps each value for the page, since a blockr option is fixed for the
//   app.
// - Code reads a blockr option with a fallback, which it takes only where R
//   has no value, since nothing sets the blockr option. R sends a value as it
//   is set, so that one an environment variable sets arrives as a string,
//   which core converts to the fallback's type, a number or a logical. R
//   writes Inf as null, which core reads as Infinity where the fallback is a
//   number.
// - A read never takes the fallback in place of a value that R cannot give or
//   that does not convert. It fails with R's message where R replies error, as
//   for a blockr option whose R option and environment variable disagree, and
//   where the value does not convert to the fallback's type.

import { SessionEndedError } from "../protocol/index.ts";
import type { Client } from "../protocol/index.ts";
import type { Json } from "../types/json.ts";
import { messageOf, quoted } from "../util/index.ts";

export interface BlockrOptions {
  // The blockr option's value, asked of R once per page, or the fallback where
  // nothing sets it. It rejects where R cannot give the value, and where the
  // value does not convert to the fallback's type.
  read<T extends Json>(name: string, fallback: T): Promise<T>;
}

// What R sent for a blockr option, or null where it sent nothing.
type Found = { readonly value: Json } | null;

// How a read in a batch is settled, once R has replied.
interface Settle {
  readonly resolve: (found: Found) => void;
  readonly reject: (error: Error) => void;
}

export function createBlockrOptions(client: Pick<Client, "request">): BlockrOptions {
  const found = new Map<string, Promise<Found>>();
  // The names that code has read in this task, which go to R together once it
  // ends.
  let batch: Map<string, Settle> | undefined;

  function ask(name: string): Promise<Found> {
    let asked = found.get(name);
    if (asked === undefined) {
      asked = new Promise<Found>((resolve, reject) => {
        if (batch === undefined) {
          const sending = new Map<string, Settle>();
          batch = sending;
          // A promise rather than a microtask, which core reports errors from.
          void Promise.resolve().then(() => {
            if (batch === sending) batch = undefined;
            void send(sending);
          });
        }
        batch.set(name, { resolve, reject });
      });
      found.set(name, asked);
    }
    return asked;
  }

  async function send(names: ReadonlyMap<string, Settle>): Promise<void> {
    const asked = [...names.keys()];
    let values: Readonly<Record<string, Json>>;
    try {
      values = await valuesOf(client, asked);
    } catch (error) {
      const failure = new Error(`R sent no values of the blockr options ${quoted(asked)}: ${messageOf(error)}`, { cause: error });
      // Each read fails, and the names are asked again the next time code
      // reads them.
      for (const [name, settle] of names) {
        found.delete(name);
        settle.reject(failure);
      }
      return;
    }
    for (const [name, settle] of names) {
      settle.resolve(Object.hasOwn(values, name) ? { value: values[name] ?? null } : null);
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
      const said = "conditions" in reply ? reply.conditions.map((c) => c.message).join(" ") : "";
      throw new Error(`R replied ${reply.status}. ${said}`.trim());
    }
    return reply.values;
  }
}

// The value as code reads it: the fallback where R sent none, a string or null
// converted where the fallback is a number or a logical, and otherwise the
// value as R sent it. A value that does not convert fails the read.
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
  throw new TypeError(`The blockr option ${name} is ${JSON.stringify(value)}, which is no ${typeof fallback}.`);
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
