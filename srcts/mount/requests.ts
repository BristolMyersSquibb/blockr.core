// A block class's request, as a block's host sends it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract):
// 1. The host has the inputs that the request reads evaluated, even where
//    nothing demands them.
// 2. Once each reads ready, the host sends the request through the scheduler,
//    at the revisions the inputs then have. With it go the state args its
//    function names, from `args` or else from the block's state, the
//    parameters `args` gives, and the values of the board options it names.
// 3. Where R replies `missing`, since a revision moved on before R ran the
//    request, the host has the inputs evaluated again and asks again.
// Once another board opens or the block leaves the board, the request stops,
// whichever step it is at, so that nothing of it reaches the next board.
// What core keeps of the answers is ./answers.ts's.

import type { Condition, ReplyStatus } from "../protocol/index.ts";
import type { BlockStatus, Inputs, Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { BlockId } from "../types/board.ts";
import type { HostRequestOpts } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { isObject, own, quoted } from "../util/index.ts";
import type { Answers, ClassRequest } from "./answers.ts";

// How many times a request asks R before it gives up on results that R keeps
// lacking, which only a fault in R causes once the scheduler has had the
// blocks it lacks evaluated again.
const ATTEMPTS = 3;

// Why a request of a block class rejects: R's reply, by its status and
// conditions, or, where the request did not go to R, the inputs it reads that
// could not be evaluated, each with the status of the block that feeds it.
export class RequestError extends Error {
  readonly status: Exclude<ReplyStatus, "ok"> | null;
  readonly conditions: readonly Condition[];
  readonly inputs: readonly { readonly input: string; readonly status: BlockStatus }[];

  constructor(
    message: string,
    fields: {
      status?: Exclude<ReplyStatus, "ok">;
      conditions?: readonly Condition[];
      inputs?: readonly { readonly input: string; readonly status: BlockStatus }[];
    } = {},
  ) {
    const conditions = fields.conditions ?? [];
    super([message, ...conditions.map((condition) => `- ${condition.message}`)].join("\n"));
    this.name = "RequestError";
    this.status = fields.status ?? null;
    this.conditions = conditions;
    this.inputs = fields.inputs ?? [];
  }
}

export interface RequestDeps {
  readonly store: Pick<Store, "board">;
  readonly scheduler: Pick<Scheduler, "evaluate" | "inputs">;
  readonly answers: Answers;
  // Whether the host is still bound to its board and its block, and why not
  // where it is not.
  readonly bound: () => boolean;
  readonly unboundError: () => Error;
}

// Sends the request `name` of the block's class, `entry`, and resolves with
// its answer as R writes it, or with the File of a download.
export async function blockRequest(
  deps: RequestDeps,
  block: BlockId,
  entry: RegistryEntry,
  name: string,
  given: Readonly<Record<string, Json | undefined>>,
  opts: HostRequestOpts,
): Promise<Json | Blob> {
  const spec = own(entry.requests, name);
  if (spec === undefined) {
    throw new Error(`The block class "${entry.id}" has no request "${name}".`);
  }
  if (!isObject(given)) {
    throw new TypeError(`The args of the request "${name}" are an object of state args and parameters.`);
  }
  const share = spec.action === true || opts.file !== undefined ? "once" : spec.volatile === true ? "merge" : "keep";
  const what = `The request "${name}" of block "${block}"`;

  for (let attempt = 1; ; attempt++) {
    const reads = await readsOf(deps, block, entry, name, spec.inputs, opts.signal);
    if (!deps.bound()) throw deps.unboundError();

    // The state and the board options, as the board holds them once the
    // inputs are evaluated.
    const board = deps.store.board();
    const state = own(board.blocks, block)?.state;
    if (state === undefined) throw new Error(`Block "${block}" left the board, so its request "${name}" did not go to R.`);

    const args: Record<string, Json> = {};
    for (const arg of spec.state_args) {
      const value = own(state, arg);
      if (value !== undefined && !Object.hasOwn(given, arg)) args[arg] = value;
    }
    for (const [arg, value] of Object.entries(given)) {
      if (value !== undefined) args[arg] = value;
    }
    const options: Record<string, Json> = {};
    for (const id of spec.options) {
      const value = own(board.options, id);
      if (value !== undefined) options[id] = value;
    }

    const request: ClassRequest = {
      kind: "request",
      type: "block_class",
      registry_id: entry.id,
      block,
      name,
      args,
      reads,
      ...(spec.options.length === 0 ? {} : { options }),
    };
    const { reply, body } = await deps.answers.ask(
      { request, share, ...(opts.file === undefined ? {} : { body: opts.file }) },
      {
        ...(opts.signal === undefined ? {} : { signal: opts.signal }),
        ...(opts.progress === undefined ? {} : { progress: opts.progress.bind(opts) }),
      },
    );
    if (!deps.bound()) throw deps.unboundError();

    switch (reply.status) {
      case "ok":
        if (body !== null) return body;
        return "value" in reply && reply.value !== undefined ? reply.value : null;
      case "missing":
        if (attempt < ATTEMPTS) continue;
        throw new RequestError(`${what} gave up, since R kept lacking the results it reads.`, {
          status: "missing",
        });
      case "cancelled":
        throw new RequestError(`R cancelled the request "${name}" of block "${block}".`, { status: "cancelled" });
      default:
        throw new RequestError(`R replied ${reply.status} to the request "${name}" of block "${block}".`, {
          status: reply.status,
          conditions: "conditions" in reply ? reply.conditions : [],
        });
    }
  }
}

// The results that the request reads, each by the input it feeds, at the
// revision its block has once it is evaluated, or a RequestError where an
// input it reads has no link or could not be evaluated. A request that names
// `...` reads each input of a variadic block that is none of its class's
// inputs of fixed names.
async function readsOf(
  deps: RequestDeps,
  block: BlockId,
  entry: RegistryEntry,
  name: string,
  names: readonly string[],
  signal: AbortSignal | undefined,
): Promise<ClassRequest["reads"]> {
  const what = `The request "${name}" of block "${block}"`;
  if (names.length === 0) return [];

  const inputs: Inputs | undefined = deps.scheduler.inputs(block);
  if (inputs === undefined) throw new Error(`Block "${block}" left the board, so its request "${name}" did not go to R.`);

  const unlinked = inputs.unlinked.filter((input) => names.includes(input));
  if (unlinked.length > 0) {
    throw new RequestError(`${what} did not go to R, since no link feeds its ${unlinked.length === 1 ? "input" : "inputs"} ${quoted(unlinked)}.`);
  }

  const read = inputs.recipe.filter(
    (input) => names.includes(input.input) || (names.includes("...") && !entry.inputs.includes(input.input)),
  );
  const upstream = [...new Set(read.flatMap((input) => input.block ?? []))];
  if (upstream.length === 0) return read.map((input) => ({ input: input.input }));

  const statuses = await deps.scheduler.evaluate(upstream, undefined, signal === undefined ? {} : { signal });
  const unready = read.flatMap((input) => {
    if (input.block === undefined) return [];
    const status = own(statuses, input.block);
    return status === undefined || status.status === "ready" ? [] : [{ input: input.input, status }];
  });
  if (unready.length > 0) {
    const listed = unready.map(({ input, status }) => `"${input}" reads ${status.status}`);
    throw new RequestError(`${what} did not go to R, since an input it reads is not ready: ${listed.join(", ")}.`, {
      inputs: unready,
    });
  }

  return read.map((input) =>
    input.block === undefined
      ? { input: input.input }
      : { input: input.input, block: input.block, revision: own(statuses, input.block)?.revision ?? 0 },
  );
}
