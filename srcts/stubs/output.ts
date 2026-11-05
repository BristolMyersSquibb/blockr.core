// A stub of the host of an output's renderer, which holds an output, answers
// the renderer's change of parameters from its output class's fixtures, those
// that R's constructor writes, and keeps a record of what the renderer
// emitted, asked for and drew nested in its output.

import type { Condition } from "../protocol/index.ts";
import type { BlockId, OptionId } from "../types/board.ts";
import type { Nested, OutputHost, OutputRequestOpts } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { OutputFixtures } from "../types/outputs.ts";
import { asError, jsonEqual } from "../util/index.ts";
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
  // with those parameters and the board option values, which options() gives.
  // They are those given, or else those of the first of the fixtures where the
  // host holds it.
  readonly fixtures?: OutputFixtures;
  readonly options?: Readonly<Record<OptionId, Json>>;
  // What the output class's requests answer, and with the scope "renderer"
  // the renderer's own.
  readonly requests?: readonly (Answered & { readonly scope?: "class" | "renderer" })[];
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
  // Each request that the renderer made, with its args and its scope.
  readonly requested: readonly {
    readonly name: string;
    readonly args: Readonly<Record<string, Json>>;
    readonly scope: "class" | "renderer";
  }[];
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
  const values = options.options ?? (options.output === undefined ? (first?.options ?? {}) : {});
  let loading = false;
  let error: Condition | null = null;
  const emitted: { name: string; payload: Json }[] = [];
  const asked: Json[] = [];
  const requested: { name: string; args: Readonly<Record<string, Json>>; scope: "class" | "renderer" }[] = [];
  const drawn: { key: string; el: HTMLElement; outputs: Json[]; removed: boolean }[] = [];
  const on = announcer(["output"] as const);

  return {
    block: options.block ?? "block",
    output: () => output,
    body: () => body,
    params: () => params,
    options: () => values,
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
          Object.entries(fixture.options).every(([id, value]) => jsonEqual(values[id], value)),
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
      opts: OutputRequestOpts = {},
    ): Promise<T | Blob> {
      const scope = opts.scope ?? "class";
      requested.push({ name, args, scope });
      if (opts.signal?.aborted === true) return Promise.reject(asError(opts.signal.reason));
      const answers = (options.requests ?? []).filter((call) => (call.scope ?? "class") === scope);
      return answer<T>(answers, name, args, scope === "class" ? "the output class" : "the renderer");
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
