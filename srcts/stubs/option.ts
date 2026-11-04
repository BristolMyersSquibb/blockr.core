// A stub of the host of a board option's UI or renderer, which holds the
// board option's value, keeps a record of what the module submitted and
// answers the board option's requests from the answers a test gives.

import type { OptionId } from "../types/board.ts";
import type { HostRequestOpts, OptionHost } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import { asError, isObject, jsonEqual } from "../util/index.ts";
import { announcer } from "./util.ts";

// What a request answers, given its name and the args it is called with.
export interface Answered {
  readonly name: string;
  readonly args?: Readonly<Record<string, Json>>;
  readonly answer: Json;
}

export interface StubOptionHostOptions<V extends Json = Json> {
  readonly option: OptionId;
  readonly value: V;
  readonly requests?: readonly Answered[];
}

export interface StubOptionHost<V extends Json = Json> extends OptionHost<V> {
  // Each value that the module submitted.
  readonly submitted: readonly V[];
  // Each request that the module made, with its args.
  readonly requested: readonly { readonly name: string; readonly args: Readonly<Record<string, Json>> }[];
  // Sets the value from outside the module, as undo or another front-end's
  // control does, and announces it.
  setValue(value: V): void;
}

export function stubOptionHost<V extends Json = Json>(options: StubOptionHostOptions<V>): StubOptionHost<V> {
  let value = options.value;
  const submitted: V[] = [];
  const requested: { name: string; args: Readonly<Record<string, Json>> }[] = [];
  const on = announcer(["value"] as const);

  return {
    option: options.option,
    value: () => value,
    on: on.listen,
    submitted,
    requested,
    submit(next) {
      submitted.push(next);
      value = next;
      on.announce("value");
      return Promise.resolve();
    },
    request<T = Json>(
      name: string,
      args: Readonly<Record<string, Json>> = {},
      opts: HostRequestOpts = {},
    ): Promise<T | Blob> {
      requested.push({ name, args });
      if (opts.signal?.aborted === true) return Promise.reject(asError(opts.signal.reason));
      return answer<T>(options.requests ?? [], name, args, `the board option "${options.option}"`);
    },
    setValue(next) {
      value = next;
      on.announce("value");
    },
  };
}

// The answer that a test gives for the request with these args, or a
// rejection that names the request.
export function answer<T>(
  answers: readonly Answered[],
  name: string,
  args: Readonly<Record<string, Json>>,
  of: string,
): Promise<T> {
  if (!isObject(args)) {
    return Promise.reject(new TypeError(`The args of the request "${name}" are an object.`));
  }
  const found = answers.find((call) => call.name === name && jsonEqual(call.args ?? {}, args));
  return found === undefined
    ? Promise.reject(new Error(`No answer is given for the request "${name}" of ${of} with the args ${JSON.stringify(args)}.`))
    : Promise.resolve(found.answer as T);
}
