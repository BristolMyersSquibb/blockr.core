// A stub of the host of a block's UI module, which answers the module's
// requests from its class's fixture file, the one that the class's R tests
// write with expect_block_fixture(), and keeps a record of what the module
// submitted. It refuses a submit that would leave the block in a state the
// file does not list, so that the module tests check that the module submits
// only states that R has checked to run, the pattern of consumer-driven
// contract tests (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.5#testing).
//
// A file that the module sends, such as an upload,
// answers a request that R recorded with the file as its arg `file`.

import type { BlockStatus } from "../scheduler/index.ts";
import type { BlockId, OptionId, RegistryId } from "../types/board.ts";
import type { Raised } from "../types/core-api.ts";
import type { BlockContext, BlockHost, HostRequestOpts, State } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import { asError, isObject, jsonEqual, own } from "../util/index.ts";
import { announcer, notifier, stubBlockContext } from "./util.ts";

// A block class's fixture file: the states that a block of the class may
// hold, each as the block holds it, and what the class's requests answered in
// R, each with the args and the values of the board options it was called
// with.
export interface BlockFixture {
  readonly states: readonly State[];
  readonly requests: readonly {
    readonly name: string;
    readonly args: Readonly<Record<string, Json>>;
    readonly options: Readonly<Record<OptionId, Json>>;
    readonly answer: Json;
  }[];
}

export interface StubBlockHostOptions<S extends State = State> {
  // The block's state, the first that the fixture lists unless given.
  readonly state?: S;
  // The block's id and its class, "block" and "stub" unless given.
  readonly block?: BlockId;
  readonly class?: RegistryId;
  // What the context holds, a ready block with no linked inputs unless given.
  readonly context?: Partial<BlockContext>;
  // What evaluate() resolves with, the status that the context holds unless
  // given.
  readonly evaluate?: (params: Record<string, Json>) => BlockStatus | Promise<BlockStatus>;
  // The modules that other packages offer, by id.
  readonly modules?: Readonly<Record<string, unknown>>;
}

export interface StubBlockHost<S extends State = State> extends BlockHost<S> {
  // Each state that the module submitted, as the block would hold it once
  // the submit applied, whether the fixture lists it or not.
  readonly submitted: readonly S[];
  // Each request that the module made, with the args it passed.
  readonly requested: readonly {
    readonly name: string;
    readonly args: Readonly<Record<string, Json>>;
    readonly file?: File;
  }[];
  // The parameters of each one-time evaluation that the module asked for.
  readonly evaluated: readonly Record<string, Json>[];
  // Each notification that the module raised and that holds, in the order
  // they came.
  readonly notified: readonly Raised[];
  // Sets the block's state from outside the module, as undo, the assistant or
  // a plugin does, and announces it.
  setState(state: S): void;
  // Changes what the context holds, as a new status or a new revision of an
  // input does, and announces it.
  setContext(context: Partial<BlockContext>): void;
}

export function stubBlockHost<S extends State = State>(
  fixture: BlockFixture,
  options: StubBlockHostOptions<S> = {},
): StubBlockHost<S> {
  let state = (options.state ?? fixture.states[0] ?? {}) as S;
  let context = stubBlockContext(options.context);
  const submitted: S[] = [];
  const requested: { name: string; args: Readonly<Record<string, Json>>; file?: File }[] = [];
  const evaluated: Record<string, Json>[] = [];
  const on = announcer(["state", "context"] as const);
  const { notified, notify } = notifier();

  return {
    block: options.block ?? "block",
    class: options.class ?? "stub",
    state: () => state,
    context: () => context,
    on: on.listen,
    submitted,
    requested,
    evaluated,
    notified,
    notify,
    submit(change) {
      if (!isObject(change)) {
        return Promise.reject(new TypeError("A submit takes the state args it changes, as an object."));
      }
      const next = { ...state, ...defined(change) };
      submitted.push(next);
      if (!fixture.states.some((listed) => jsonEqual(listed, next))) {
        return Promise.reject(
          new Error(`The module submitted the state ${JSON.stringify(next)}, which the fixture does not list.`),
        );
      }
      state = next;
      on.announce("state");
      return Promise.resolve();
    },
    request<T = Json>(
      name: string,
      args: Readonly<Record<string, Json | undefined>> = {},
      opts: HostRequestOpts = {},
    ): Promise<T | Blob> {
      const given = defined(args);
      requested.push({ name, args: given, ...(opts.file === undefined ? {} : { file: opts.file }) });
      if (opts.signal?.aborted === true) return Promise.reject(asError(opts.signal.reason));
      const found = fixture.requests.find(
        (call) =>
          call.name === name &&
          Object.keys(given).every((arg) => Object.hasOwn(call.args, arg)) &&
          // The host fills in a state arg from the block's state
          // where the module passes none,
          // and R gives a file the module sends to the arg `file`.
          Object.entries(call.args).every(([arg, value]) =>
            arg === "file" && opts.file !== undefined && !Object.hasOwn(given, arg)
              ? sameFile(value, opts.file)
              : jsonEqual(Object.hasOwn(given, arg) ? given[arg] : own(state, arg), value),
          ) &&
          Object.entries(call.options).every(([id, value]) => jsonEqual(own(context.options, id), value)),
      );
      if (found === undefined) {
        return Promise.reject(
          new Error(
            `The fixture records no answer of the request "${name}" with the args ${JSON.stringify(given)}, on the state ${JSON.stringify(state)} and the board options ${JSON.stringify(context.options)}.`,
          ),
        );
      }
      return Promise.resolve(found.answer as T);
    },
    async evaluate(params) {
      if (!isObject(params)) {
        throw new TypeError("A host's evaluate() takes the parameters that the block's block_eval() method reads, as an object.");
      }
      evaluated.push(params);
      return options.evaluate === undefined ? context.status : await options.evaluate(params);
    },
    module<T = unknown>(id: string): Promise<T> {
      const modules = options.modules ?? {};
      return Object.hasOwn(modules, id)
        ? Promise.resolve(modules[id] as T)
        : Promise.reject(new Error(`No package offers a module "${id}".`));
    },
    setState(next) {
      state = next;
      on.announce("state");
    },
    setContext(changes) {
      context = stubBlockContext({ ...context, ...changes });
      on.announce("context");
    },
  };
}

// Whether a file that R recorded as an action's arg `file`,
// as Shiny's fileInput() gives a file, is the one the module sends,
// by its name, type and size, since its `datapath` is where R wrote it.
function sameFile(recorded: Json, file: File): boolean {
  return (
    isObject(recorded) &&
    recorded.name === file.name &&
    recorded.type === file.type &&
    recorded.size === file.size
  );
}

// The values of an object that are defined, as JSON keeps them.
function defined(x: Readonly<Record<string, unknown>>): Readonly<Record<string, Json>> {
  return Object.fromEntries(Object.entries(x).filter(([, value]) => value !== undefined)) as Readonly<
    Record<string, Json>
  >;
}
