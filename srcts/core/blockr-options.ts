// Reading blockr options, which the client asks R for by name, since it can
// read neither an R option nor an environment variable of the server, as
// Blockr options has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.9#blockr-options):
// - Core asks R for a blockr option the first time code reads it, with a
//   `blockr_options` request. The names that code reads at once, within one
//   task, go in one request.
// - Core keeps each value for the page, since a blockr option is fixed for the
//   app.
// - A read gives the value as R sent it, or undefined where nothing sets the
//   blockr option. The code that reads it converts and checks the value, and
//   takes a fallback of its own where nothing sets it. R sends a value as it is
//   set, so that one an environment variable sets arrives as a string, and
//   writes Inf, NA and NaN as null.
// - A read fails with R's message where R replies error, as for a blockr option
//   whose R option and environment variable disagree.

import { SessionEndedError } from "../protocol/index.ts";
import type { Client } from "../protocol/index.ts";
import type { Json } from "../types/json.ts";
import { errorMessage, quoted } from "../util/index.ts";

export interface BlockrOptions {
  // The blockr option's value as R sent it, asked of R once per page, or
  // undefined where nothing sets it. It rejects where R cannot give the value.
  read(name: string): Promise<Json | undefined>;
}

// How a read in a batch is settled, once R has replied.
interface Settle {
  readonly resolve: (value: Json | undefined) => void;
  readonly reject: (error: Error) => void;
}

export function createBlockrOptions(client: Pick<Client, "request">): BlockrOptions {
  const found = new Map<string, Promise<Json | undefined>>();
  // The names that code has read in this task, which go to R together once it
  // ends.
  let batch: Map<string, Settle> | undefined;

  function ask(name: string): Promise<Json | undefined> {
    let asked = found.get(name);
    if (asked === undefined) {
      asked = new Promise<Json | undefined>((resolve, reject) => {
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
      values = await requestBlockrOptions(client, asked);
    } catch (error) {
      const failure = new Error(`R sent no values of the blockr options ${quoted(asked)}: ${errorMessage(error)}`, { cause: error });
      // Each read fails, and the names are asked again the next time code
      // reads them.
      for (const [name, settle] of names) {
        found.delete(name);
        settle.reject(failure);
      }
      return;
    }
    for (const [name, settle] of names) {
      settle.resolve(Object.hasOwn(values, name) ? (values[name] ?? null) : undefined);
    }
  }

  return {
    async read(name: string): Promise<Json | undefined> {
      if (typeof name !== "string" || name === "") {
        throw new TypeError("A blockr option is read by its name, a non-empty string.");
      }
      return ask(name);
    },
  };
}

// The values that R has of the blockr options, by name, asked again of the
// next session where the session ends before R replies, since reading them
// changes nothing.
async function requestBlockrOptions(client: Pick<Client, "request">, names: string[]): Promise<Readonly<Record<string, Json>>> {
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
