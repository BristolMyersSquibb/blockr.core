// A stub of the host of an output's renderer, which holds an output, answers
// the renderer's change of parameters from its output class's fixtures, those
// that R's constructor writes, and keeps a record of what the renderer
// emitted, asked for and drew nested in its output.

import type { Condition } from "../protocol/messages.ts";
import { asError, jsonEqual } from "../store/util.ts";
import type { BlockId, OptionId } from "../types/board.ts";
import type { HostRequestOpts, Nested, OutputHost } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { OutputFixtures } from "../types/outputs.ts";
import { answer } from "./option.ts";
import type { Answered } from "./option.ts";
import { announcer } from "./util.ts";

export interface StubOutputHostOptions {
  // The output it holds, with its body and the parameters it was asked with,
  // the first of the fixtures with their parameters unless given.
  readonly output?: Json;
  readonly body?: Blob | null;
  readonly params?: Json;
  // The block's id, "block" unless given.
  readonly block?: BlockId;
  // The output class's fixtures, from which setParams() takes the output asked
  // with those parameters and the board option values given.
  readonly fixtures?: OutputFixtures;
  readonly options?: Readonly<Record<OptionId, Json>>;
  // What the output class's requests answer.
  readonly requests?: readonly Answered[];
}

// An output drawn nested in the one the host holds, with each output it was
// handed since, and whether it was removed.
export interface Drawn {
  readonly key: string;
  readonly el: HTMLElement;
  readonly outputs: readonly Json[];
  readonly removed: boolean;
}

export interface StubOutputHost extends OutputHost {
  // Each event the renderer emitted.
  readonly emitted: readonly { readonly name: string; readonly payload: Json }[];
  // The parameters of each setParams().
  readonly asked: readonly Json[];
  // Each request that the renderer made, with its args.
  readonly requested: readonly { readonly name: string; readonly args: Readonly<Record<string, Json>> }[];
  // Each output it drew nested in its own.
  readonly drawn: readonly Drawn[];
  // Hands the renderer another output, as core does once R sends one, and
  // announces it.
  setOutput(output: Json, body?: Blob | null, params?: Json): void;
  // Says whether core is asking R for a newer output, and why the latest ask
  // failed, and announces it.
  setLoading(loading: boolean): void;
  setError(error: Condition | null): void;
}

export function stubOutputHost(options: StubOutputHostOptions = {}): StubOutputHost {
  const first = options.fixtures?.outputs[0];
  let output: Json = options.output ?? (first?.output as unknown as Json | undefined) ?? null;
  let body = options.body ?? null;
  let params: Json = options.params ?? (options.output === undefined ? (first?.params ?? {}) : {});
  let loading = false;
  let error: Condition | null = null;
  const emitted: { name: string; payload: Json }[] = [];
  const asked: Json[] = [];
  const requested: { name: string; args: Readonly<Record<string, Json>> }[] = [];
  const drawn: { key: string; el: HTMLElement; outputs: Json[]; removed: boolean }[] = [];
  const on = announcer(["output"] as const);

  return {
    block: options.block ?? "block",
    output: () => output,
    body: () => body,
    params: () => params,
    loading: () => loading,
    error: () => error,
    on: on.listen,
    emitted,
    asked,
    requested,
    drawn,
    setParams(next) {
      asked.push(next);
      const found = options.fixtures?.outputs.find(
        (fixture) =>
          jsonEqual(fixture.params, next) &&
          Object.entries(fixture.options).every(([id, value]) => jsonEqual(options.options?.[id], value)),
      );
      if (found === undefined) {
        error = {
          severity: "error",
          class: ["blockr_error", "error", "condition"],
          message: `The fixtures hold no output asked with the parameters ${JSON.stringify(next)}.`,
        };
      } else {
        output = found.output as unknown as Json;
        params = next;
        error = null;
      }
      on.announce("output");
    },
    request<T = Json>(
      name: string,
      args: Readonly<Record<string, Json>> = {},
      opts: HostRequestOpts = {},
    ): Promise<T | Blob> {
      requested.push({ name, args });
      if (opts.signal?.aborted === true) return Promise.reject(asError(opts.signal.reason));
      return answer<T>(options.requests ?? [], name, args, "the output class");
    },
    emit(name, payload) {
      emitted.push({ name, payload });
    },
    draw(el, key, nested): Nested {
      const entry = { key, el, outputs: [nested], removed: false };
      drawn.push(entry);
      return {
        update(next) {
          entry.outputs.push(next);
        },
        remove() {
          entry.removed = true;
        },
      };
    },
    setOutput(next, nextBody = null, nextParams) {
      output = next;
      body = nextBody;
      if (nextParams !== undefined) params = nextParams;
      loading = false;
      error = null;
      on.announce("output");
    },
    setLoading(next) {
      loading = next;
      on.announce("output");
    },
    setError(next) {
      error = next;
      on.announce("output");
    },
  };
}
