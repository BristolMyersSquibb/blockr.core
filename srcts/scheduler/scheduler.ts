// The scheduler: what the client tracks about evaluation, and the API through
// which front-ends, extensions and packages ask for it, as Evaluation in the
// client has it (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10).
// It keeps a revision for each block, which goes up whenever the block's
// recipe changes or something invalidates it, has R evaluate what is demanded,
// with everything upstream of it, by the evaluation priority each demand gives
// and in topological order within one, and keeps each block's status from R's
// replies. There is one per board, which core creates beside the store and
// hands to front-ends and extensions through `CoreApi`.

import { SessionEndedError } from "../protocol/client.ts";
import type { Client, RequestOptions } from "../protocol/client.ts";
import type { Condition, Recipe, Reply } from "../protocol/messages.ts";
import type { Delta } from "../store/delta.ts";
import type { Store } from "../store/store.ts";
import { jsonEqual, messageOf, own, quoted } from "../store/util.ts";
import type { BlockId, Board, RegistryId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import { graphOf, upstreamOf } from "./graph.ts";
import type { Graph, Input } from "./graph.ts";
import { counter } from "./revisions.ts";
import {
  outcomeOf,
  replied,
  sameStatus,
  unevaluated,
  unsent,
  waiting,
  waitingFor,
} from "./status.ts";
import type { BlockStatus, Outcome } from "./status.ts";

export interface Scheduler {
  // Keeps the blocks evaluated, with everything upstream of them, until the
  // demand it returns is released: a set of blocks, such as what a view shows,
  // or a function of the board, which the scheduler calls again after every
  // change to the board, at the evaluation priority given, 0 unless given. The
  // owner, such as "blockr.dock", says whose demand it is, so that the
  // scheduler can name it, as when a function of the board throws.
  demand(
    owner: string,
    blocks: Iterable<BlockId> | ((board: Board) => Iterable<BlockId>),
    options?: { priority?: number },
  ): Demand;
  // A one-time evaluation, which brings the blocks and their upstream up to
  // date without keeping them so, and resolves with each block's status by its
  // id once none of them reads stale or unevaluated, at whatever revision each
  // has by then. It leaves out a block that leaves the board meanwhile, rejects
  // once it is left with none or when another board opens, and stops once the
  // signal aborts. Given parameters, which R passes to the block_eval() method
  // of each block, it evaluates each block once, whether it is stale or not,
  // at the revision the block has once R holds what it reads, never cancels
  // that evaluation once sent, and resolves with the statuses R's replies
  // give. It rejects where a block reads waiting, which has no result in
  // sight.
  evaluate(
    blocks: Iterable<BlockId>,
    params?: Record<string, Json>,
    options?: { priority?: number; signal?: AbortSignal },
  ): Promise<Record<BlockId, BlockStatus>>;
  // Gives the block and every block downstream of it a new revision, so that
  // they evaluate again where something demands them, and turn stale where
  // nothing does. A block that the board lacks is passed over.
  invalidate(id: BlockId): void;
  // The block's status now, the same object until subscribe() announces that
  // it changed, or undefined for a block that the board lacks.
  status(id: BlockId): BlockStatus | undefined;
  // Calls `fn` after statuses change, with the blocks whose status changed.
  subscribe(fn: (changed: readonly BlockId[]) => void): () => void;

  // What core uses beside the above, which front-ends and extensions use.

  // Sends an output or a request to R through the client, as the client's own
  // request() does, so that the scheduler sees everything R is asked to run.
  // Where R replies `missing` for a result whose revision is still current and
  // reads ready, it has the block evaluated again and asks again. Where the
  // session ends before R replies, it asks the next session for an output, an
  // output class's request and a block class's request, and rejects with the
  // SessionEndedError for an action and for a board option's or an
  // extension's request, which may have run.
  request(
    request: Extract<Parameters<Client["request"]>[0], { kind: "request" | "output" }>,
    options?: RequestOptions,
  ): Promise<{ reply: Reply; body: Blob | null }>;
  // Lets the scheduler send evaluations, which it holds from its creation
  // until then, so that core starts it once the front-end's render() has
  // returned.
  start(): void;
  // Holds evaluations again until start(), as while the page switches to
  // another front-end.
  hold(): void;
  // Hears that R answered the hello of a new session, which holds no results.
  session(): void;
}

// A demand that demand() returns, which only its holder replaces or releases.
export interface Demand {
  // Replaces the blocks it demands, and its evaluation priority where given.
  set(
    blocks: Iterable<BlockId> | ((board: Board) => Iterable<BlockId>),
    options?: { priority?: number },
  ): void;
  release(): void;
}

export interface SchedulerOptions {
  // The store of the board, whose changes the scheduler follows.
  readonly store: Pick<Store, "board" | "subscribe" | "blockRegistry">;
  // The client of the wire protocol, which the store sends through too.
  readonly client: Pick<Client, "request" | "forget" | "capabilities">;
  // The blockr options that the scheduler reads, which core asks R for as soon
  // as R has answered its hello. The `evaluation_queue` blockr option: how many
  // evaluations the scheduler keeps waiting in R behind the one R runs, 2
  // unless given, where 0 sends the next evaluation only once the last reply
  // is back and Infinity sends every evaluation at once.
  readonly evaluationQueue?: number;
  // The `background_evaluation_delay` blockr option: how many seconds R has had
  // nothing else to run before the scheduler evaluates the stale and
  // unevaluated blocks that nothing demands, Infinity unless given, which
  // evaluates nothing in the background.
  readonly backgroundEvaluationDelay?: number;
  // The `gate_visibility` blockr option, true unless given, which has the
  // scheduler demand every block when false.
  readonly gateVisibility?: boolean;
  // For a block that `serve()` serves alone, the inputs that R binds itself to
  // the data it was given, from its reply to hello, which count as linked on
  // the board R sent until another board opens.
  readonly bound?: readonly string[];
}

// The client's request() of an output or a request.
type Asking = Parameters<Scheduler["request"]>[0];

// What the scheduler keeps for a block.
interface Entry {
  revision: number;
  // What the revision was taken for, which a new revision follows when it
  // changes: the class, the state and the inputs, while the revisions of the
  // blocks upstream are theirs.
  registryId: RegistryId;
  state: Readonly<Record<string, Json>>;
  inputs: readonly Input[];
  // What its latest reply said, from which its status comes.
  outcome: Outcome | undefined;
  // The revision whose result R holds for the block, as far as the client
  // knows: that of the latest evaluation R replied ok to, until R loses it.
  held: number | undefined;
}

// An evaluation sent to R and not yet answered.
interface Sent {
  // When it was sent, as one count orders everything the scheduler sends.
  readonly seq: number;
  readonly at: number;
  readonly block: BlockId;
  readonly revision: number;
  readonly controller: AbortController;
  // The one-time evaluation with parameters that it belongs to.
  readonly part: Part | undefined;
  // Whether it was sent as evaluation in the background.
  readonly background: boolean;
  cancelled: boolean;
  // Whether another board opened since, so that its reply counts for nothing.
  orphaned: boolean;
}

// An output or a request sent through request() and not yet answered.
interface Asked {
  readonly seq: number;
  readonly at: number;
}

// A demand as its owner keeps it.
interface Demanding {
  readonly owner: string;
  blocks: ReadonlySet<BlockId> | ((board: Board) => Iterable<BlockId>);
  priority: number;
  // What a function of the board demanded of the board it last read.
  last: { readonly board: Board; readonly blocks: ReadonlySet<BlockId> } | undefined;
}

// A one-time evaluation without parameters, waiting on its blocks.
interface OneTime {
  readonly blocks: Set<BlockId>;
  readonly priority: number;
  settle(result: { statuses: Record<BlockId, BlockStatus> } | { error: unknown }): void;
}

// A one-time evaluation with parameters, an evaluation of each block.
interface WithParams {
  readonly priority: number;
  readonly params: Record<string, Json>;
  readonly parts: Map<BlockId, Part>;
  done: boolean;
  settle(result: { statuses: Record<BlockId, BlockStatus> } | { error: unknown }): void;
}

interface Part {
  readonly of: WithParams;
  readonly block: BlockId;
  // The recipe it went to R with, at the revision the block had then, while R
  // has not replied.
  recipe: Recipe | undefined;
  sent: Sent | undefined;
  result: BlockStatus | undefined;
}

// How long anything else may wait in R behind an evaluation below evaluation
// priority 0 before the scheduler cancels that evaluation.
const GIVE_WAY = 5000;

export function createScheduler(options: SchedulerOptions): Scheduler {
  const { store, client } = options;
  const evaluationQueue = setting(options.evaluationQueue, 2, "evaluation_queue");
  const backgroundDelay = setting(
    options.backgroundEvaluationDelay,
    Infinity,
    "background_evaluation_delay",
  );
  let bound: ReadonlySet<string> = new Set(options.bound ?? []);

  const next = counter();
  // What stopped the scheduler, such as running out of revisions.
  let stopped: unknown;
  let holding = true;

  let board: Board | undefined;
  let graph: Graph | undefined;
  const entries = new Map<BlockId, Entry>();
  const statuses = new Map<BlockId, BlockStatus>();
  const changed = new Set<BlockId>();
  const subscribers = new Set<(changed: readonly BlockId[]) => void>();

  const demands = new Set<Demanding>();
  const oneTimes = new Set<OneTime>();
  const withParams = new Set<WithParams>();
  const sent = new Set<Sent>();
  const asked = new Set<Asked>();
  let seq = 0;

  // The evaluation priority of each block that something needs, as the last
  // pass found it.
  let needs = new Map<BlockId, number>();
  // Since when R has had nothing else to run than evaluations below 0.
  let idleSince: number | undefined;
  let passTimer: ReturnType<typeof setTimeout> | undefined;
  let wakeTimer: ReturnType<typeof setTimeout> | undefined;

  if (options.gateVisibility === false) {
    demands.add({
      owner: "blockr.core",
      blocks: (all) => Object.keys(all.blocks),
      priority: 0,
      last: undefined,
    });
  }

  function revision(): number {
    try {
      return next();
    } catch (error) {
      stopped = error;
      throw error;
    }
  }

  // The scheduler works out what to send once the task in which something
  // changed has ended, after every subscriber of the store has heard of the
  // change, so that a front-end that changes its demand from its own
  // subscription is heard first.
  function schedule(): void {
    passTimer ??= setTimeout(pass, 0);
  }

  function wake(at: number): void {
    if (wakeTimer !== undefined) clearTimeout(wakeTimer);
    wakeTimer = setTimeout(
      () => {
        wakeTimer = undefined;
        pass();
      },
      Math.max(0, at - Date.now()),
    );
  }

  // Following the board

  function entryOf(id: BlockId, next: Board, of: Graph): Entry {
    const block = next.blocks[id];
    return {
      revision: revision(),
      registryId: block?.registry_id ?? "",
      state: block?.state ?? {},
      inputs: of.inputs.get(id) ?? [],
      outcome: undefined,
      held: undefined,
    };
  }

  // A board that opens holds no result in R, which forgets every block, so
  // every block of it reads unevaluated, with a revision above any its id had.
  function opened(next: Board): void {
    if (board !== undefined) {
      const old = Object.keys(board.blocks);
      // R forgets every block as it opens a board, and this forgets what an
      // evaluation sent since may have kept.
      if (old.length > 0) client.forget(old);
      bound = new Set();
    }

    for (const evaluation of sent) evaluation.orphaned = true;
    const error = new Error("Another board opened.");
    for (const oneTime of oneTimes) oneTime.settle({ error });
    oneTimes.clear();
    for (const evaluation of withParams) fail(evaluation, error);

    for (const id of statuses.keys()) changed.add(id);
    entries.clear();
    statuses.clear();
    for (const demand of demands) demand.last = undefined;

    board = next;
    graph = graphOf(next, store.blockRegistry(), bound);
    for (const id of graph.order) entries.set(id, entryOf(id, next, graph));
  }

  // A block's revision goes up whenever its recipe changes: its class, its
  // state, the links into it or the revision of any block upstream of it,
  // while a rename, an attribute, a stack, the front-end's state and a board
  // option change none.
  function applied(delta: Delta, next: Board): void {
    const removed = new Set(delta.blocks?.rm ?? []);
    const gone = [...entries.keys()].filter(
      (id) => !Object.hasOwn(next.blocks, id) || removed.has(id),
    );
    if (gone.length > 0) {
      left(new Set(gone));
      client.forget(gone);
    }

    board = next;
    const now = graphOf(next, store.blockRegistry(), bound);
    graph = now;
    const bumped = new Set<BlockId>();

    for (const id of now.order) {
      const entry = entries.get(id);
      if (entry === undefined) {
        entries.set(id, entryOf(id, next, now));
        bumped.add(id);
        continue;
      }
      const block = next.blocks[id];
      if (block === undefined) continue;
      const inputs = now.inputs.get(id) ?? [];
      const moved =
        entry.registryId !== block.registry_id ||
        !(entry.state === block.state || jsonEqual(entry.state, block.state)) ||
        !sameInputs(entry.inputs, inputs) ||
        inputs.some((input) => input.block !== undefined && bumped.has(input.block));
      entry.registryId = block.registry_id;
      entry.state = block.state;
      entry.inputs = inputs;
      if (moved) {
        entry.revision = revision();
        bumped.add(id);
      }
    }
  }

  // Forgets the status of blocks that left the board, as R forgets their
  // results.
  function left(ids: ReadonlySet<BlockId>): void {
    for (const id of ids) {
      entries.delete(id);
      if (statuses.delete(id)) changed.add(id);
    }
    for (const oneTime of oneTimes) {
      for (const id of ids) oneTime.blocks.delete(id);
      if (oneTime.blocks.size === 0) {
        oneTimes.delete(oneTime);
        oneTime.settle({ error: new Error("Every block of the one-time evaluation left the board.") });
      }
    }
    for (const evaluation of withParams) {
      for (const part of evaluation.parts.values()) {
        if (ids.has(part.block) && part.sent === undefined && part.result === undefined) {
          fail(
            evaluation,
            new Error(`Block "${part.block}" left the board before its evaluation with parameters was sent.`),
          );
          break;
        }
      }
    }
  }

  // Statuses

  function inFlight(id: BlockId, at: number): boolean {
    for (const evaluation of sent) {
      if (
        evaluation.block === id &&
        evaluation.revision === at &&
        !evaluation.orphaned &&
        !evaluation.cancelled
      ) {
        return true;
      }
    }
    return false;
  }

  // Why a block cannot run, if it cannot: an input that no link feeds and R
  // binds not, a variadic block without a link, or a block upstream that reads
  // failed, invalid or waiting.
  function why(id: BlockId, of: Graph): Condition | undefined {
    const unlinked = of.unlinked.get(id);
    if (unlinked !== undefined) return waitingFor({ unlinked });
    const upstream = upstreamOf(of, id).filter((up) => {
      const status = statuses.get(up)?.status;
      return status === "failed" || status === "invalid" || status === "waiting";
    });
    return upstream.length > 0 ? waitingFor({ upstream }) : undefined;
  }

  // Works out every block's status, upstream first, keeping the object of each
  // that reads as it did.
  function recompute(): void {
    if (graph === undefined) return;
    const running = new Set<BlockId>();
    for (const evaluation of sent) {
      if (!evaluation.orphaned) running.add(evaluation.block);
    }

    for (const id of graph.order) {
      const entry = entries.get(id);
      if (entry === undefined) continue;
      const busy = running.has(id);
      const reason = why(id, graph);
      const now =
        reason !== undefined
          ? waiting(reason, entry.revision, entry.outcome?.revision ?? null, busy)
          : entry.outcome === undefined
            ? unevaluated(entry.revision, busy)
            : replied(entry.outcome, entry.revision, busy);
      const before = statuses.get(id);
      if (before === undefined || !sameStatus(before, now)) {
        statuses.set(id, Object.freeze(now));
        changed.add(id);
      }
    }
  }

  // Whether a block needs evaluating to be current: it reads stale or
  // unevaluated, or ready while R has lost its result.
  function needsEvaluation(id: BlockId): boolean {
    const status = statuses.get(id)?.status;
    const entry = entries.get(id);
    if (entry === undefined) return false;
    return (
      status === "stale" ||
      status === "unevaluated" ||
      (status === "ready" && entry.held !== entry.revision)
    );
  }

  // Whether R holds the block's result at its current revision, or computes it
  // before anything sent after now.
  function available(id: BlockId): boolean {
    const entry = entries.get(id);
    if (entry === undefined) return false;
    return (
      (statuses.get(id)?.status === "ready" && entry.held === entry.revision) ||
      inFlight(id, entry.revision)
    );
  }

  function flush(): void {
    settleOneTimes();
    if (changed.size === 0) return;
    const ids = [...changed];
    changed.clear();
    for (const fn of [...subscribers]) {
      try {
        fn(ids);
      } catch (error) {
        report(error);
      }
    }
  }

  function settleOneTimes(): void {
    for (const oneTime of [...oneTimes]) {
      const ids = [...oneTime.blocks];
      if (ids.some(needsEvaluation)) continue;
      oneTimes.delete(oneTime);
      const result: Record<BlockId, BlockStatus> = {};
      for (const id of ids) {
        const status = statuses.get(id);
        if (status !== undefined) result[id] = status;
      }
      oneTime.settle({ statuses: result });
    }
  }

  // Sending

  function recipeOf(id: BlockId): Recipe | undefined {
    const block = board?.blocks[id];
    const entry = entries.get(id);
    if (block === undefined || entry === undefined) return undefined;
    return {
      registry_id: block.registry_id,
      state: { ...block.state },
      inputs: entry.inputs.map((input) =>
        input.block === undefined
          ? { input: input.input }
          : {
              input: input.input,
              block: input.block,
              revision: entries.get(input.block)?.revision ?? 0,
            },
      ),
    };
  }

  function dispatch(
    block: BlockId,
    at: number,
    recipe: Recipe,
    how: { part?: Part; background?: boolean },
  ): void {
    const evaluation: Sent = {
      seq: ++seq,
      at: Date.now(),
      block,
      revision: at,
      controller: new AbortController(),
      part: how.part,
      background: how.background ?? false,
      cancelled: false,
      orphaned: false,
    };
    sent.add(evaluation);
    if (how.part !== undefined) how.part.sent = evaluation;

    client
      .request(
        {
          kind: "evaluate",
          block,
          revision: at,
          recipe,
          ...(how.part === undefined ? {} : { params: how.part.of.params }),
        },
        { signal: evaluation.controller.signal },
      )
      .then(
        ({ reply }) => {
          answered(evaluation, reply);
        },
        (error: unknown) => {
          unanswered(evaluation, error);
        },
      );
  }

  function cancel(evaluation: Sent): void {
    evaluation.cancelled = true;
    evaluation.controller.abort();
  }

  // A reply sets the block's status where it is for the block's current
  // revision and the block reads anything but waiting, while `cancelled` and
  // `missing` count as none.
  function answered(evaluation: Sent, reply: Reply): void {
    sent.delete(evaluation);

    if (!evaluation.orphaned) {
      if (reply.status === "missing") {
        for (const lacked of reply.missing) {
          const entry = entries.get(lacked.block);
          if (entry?.held === lacked.revision) entry.held = undefined;
        }
      }

      const outcome = outcomeOf(reply, evaluation.revision);
      const entry = entries.get(evaluation.block);
      if (outcome !== undefined && entry !== undefined) {
        // R keeps the result of the latest evaluation it ran, which a reply
        // that failed leaves it without.
        entry.held = outcome.status === "ok" ? evaluation.revision : undefined;
        if (
          evaluation.revision === entry.revision &&
          statuses.get(evaluation.block)?.status !== "waiting"
        ) {
          entry.outcome = outcome;
        }
      }

      if (evaluation.part !== undefined) {
        partAnswered(evaluation.part, reply, outcome, evaluation.revision);
      }
    }

    recompute();
    flush();
    schedule();
  }

  function unanswered(evaluation: Sent, error: unknown): void {
    sent.delete(evaluation);

    if (!evaluation.orphaned) {
      if (error instanceof SessionEndedError) {
        // The session that held every result has ended.
        for (const entry of entries.values()) entry.held = undefined;
      } else if (evaluation.part === undefined) {
        // The transport could not send it, so the block fails with why, rather
        // than being sent again and again.
        const entry = entries.get(evaluation.block);
        if (entry?.revision === evaluation.revision) {
          entry.outcome = unsent(evaluation.revision, messageOf(error));
        }
      }
      if (evaluation.part !== undefined) fail(evaluation.part.of, error);
    }

    recompute();
    flush();
    schedule();
  }

  // One-time evaluations with parameters

  function partAnswered(
    part: Part,
    reply: Reply,
    outcome: Outcome | undefined,
    at: number,
  ): void {
    const evaluation = part.of;
    if (evaluation.done) return;

    if (outcome !== undefined) {
      part.result = Object.freeze(replied(outcome, at, false));
      finish(evaluation);
    } else if (reply.status === "missing") {
      // R ran nothing, so the evaluation goes again, with the recipe the block
      // has once R holds again what it reads.
      part.sent = undefined;
      part.recipe = undefined;
    } else {
      fail(
        evaluation,
        new Error(`R dropped the evaluation of block "${part.block}" with parameters, as it does for a block that leaves the board.`),
      );
    }
  }

  function finish(evaluation: WithParams): void {
    const result: Record<BlockId, BlockStatus> = {};
    for (const [id, part] of evaluation.parts) {
      if (part.result === undefined) return;
      result[id] = part.result;
    }
    evaluation.done = true;
    withParams.delete(evaluation);
    evaluation.settle({ statuses: result });
  }

  function fail(evaluation: WithParams, error: unknown): void {
    if (evaluation.done) return;
    evaluation.done = true;
    withParams.delete(evaluation);
    evaluation.settle({ error });
  }

  // Whether the evaluation of a block with parameters can go: once R holds
  // what the block reads at their current revisions, or computes them before.
  // A block that reads waiting has no result in sight, which rejects the
  // evaluation.
  function partState(part: Part, of: Graph): "send" | "wait" | { error: Error } {
    const status = statuses.get(part.block);
    if (status?.status === "waiting") return { error: waitingError(part.block, status) };
    return upstreamOf(of, part.block).every(available) ? "send" : "wait";
  }

  // The pass, which works out what to cancel and what to send

  function demanded(demand: Demanding, of: Board): Iterable<BlockId> {
    if (typeof demand.blocks !== "function") return demand.blocks;
    if (demand.last?.board === of) return demand.last.blocks;
    let blocks: ReadonlySet<BlockId>;
    try {
      blocks = new Set(demand.blocks(of));
    } catch (error) {
      // A function of the board that throws demands nothing until it next
      // returns.
      blocks = new Set();
      report(
        new Error(
          `The demand of ${demand.owner} threw, so it demands nothing until it returns: ${messageOf(error)}`,
          { cause: error },
        ),
      );
    }
    demand.last = { board: of, blocks };
    return blocks;
  }

  // The evaluation priority an evaluation has now: that of what needs its
  // block, -1 for evaluation in the background, or undefined where nothing
  // needs it any more.
  function priorityOf(evaluation: Sent): number | undefined {
    if (evaluation.part !== undefined) return evaluation.part.of.priority;
    return needs.get(evaluation.block) ?? (evaluation.background ? -1 : undefined);
  }

  // Since when anything else, an evaluation at a higher evaluation priority,
  // an output or a request, has waited in R behind an evaluation.
  function waitedSince(evaluation: Sent, priority: number): number | undefined {
    let since: number | undefined;
    for (const other of asked) {
      if (other.seq > evaluation.seq) since = Math.min(since ?? other.at, other.at);
    }
    for (const other of sent) {
      if (other.seq <= evaluation.seq || other.orphaned) continue;
      if ((priorityOf(other) ?? -Infinity) > priority) {
        since = Math.min(since ?? other.at, other.at);
      }
    }
    return since;
  }

  function pass(): void {
    passTimer = undefined;
    if (stopped !== undefined || board === undefined || graph === undefined) return;
    const now = Date.now();
    const current = board;
    const of = graph;
    const deadlines: number[] = [];

    // What is needed: what is demanded and what a one-time evaluation waits
    // on, each block at the highest evaluation priority of anything that
    // needs it, itself or a block downstream of it.
    const found = new Map<BlockId, number>();
    const raise = (id: BlockId, priority: number) => {
      if (!entries.has(id)) return;
      const was = found.get(id);
      if (was === undefined || priority > was) found.set(id, priority);
    };
    for (const demand of demands) {
      for (const id of demanded(demand, current)) raise(id, demand.priority);
    }
    for (const oneTime of oneTimes) {
      for (const id of oneTime.blocks) raise(id, oneTime.priority);
    }
    // What an evaluation with parameters reads stays needed until R has
    // replied to it, so that nothing cancels an evaluation it waits on in R:
    // what the block reads now until it is sent, and what its recipe names
    // from then on.
    for (const evaluation of withParams) {
      for (const part of evaluation.parts.values()) {
        if (part.result !== undefined) continue;
        const reads =
          part.recipe === undefined
            ? upstreamOf(of, part.block)
            : part.recipe.inputs.flatMap((input) =>
                input.block === undefined ? [] : [input.block],
              );
        for (const id of reads) raise(id, evaluation.priority);
      }
    }
    for (let i = of.order.length - 1; i >= 0; i--) {
      const id = of.order[i] ?? "";
      const priority = found.get(id);
      if (priority === undefined) continue;
      for (const up of upstreamOf(of, id)) raise(up, priority);
    }
    needs = found;

    // What to cancel: an evaluation whose revision has moved, one that R has
    // not started and nothing needs any more, and, where R can stop what it
    // has started, one below 0 that something else has waited behind for 5 s.
    const interrupt = client.capabilities().has("interrupt");
    let oldest = Infinity;
    for (const item of [...sent, ...asked]) oldest = Math.min(oldest, item.seq);

    for (const evaluation of sent) {
      if (evaluation.cancelled || evaluation.orphaned || evaluation.part !== undefined) continue;
      const entry = entries.get(evaluation.block);
      // R drops the evaluation of a block that left the board as it forgets
      // the block.
      if (entry === undefined) continue;
      if (entry.revision !== evaluation.revision) {
        cancel(evaluation);
        continue;
      }
      const priority = priorityOf(evaluation);
      if (priority === undefined) {
        if (evaluation.seq !== oldest) cancel(evaluation);
        continue;
      }
      if (!interrupt || priority >= 0) continue;
      const since = waitedSince(evaluation, priority);
      if (since === undefined) continue;
      if (now - since >= GIVE_WAY) cancel(evaluation);
      else deadlines.push(since + GIVE_WAY);
    }

    // What to send: the blocks needed that are not current, highest
    // evaluation priority first and in topological order within one, each
    // once R holds or computes before it what it reads.
    const queued: { block: BlockId; priority: number; part?: Part }[] = [];
    for (const [id, priority] of found) {
      const entry = entries.get(id);
      if (entry === undefined || !needsEvaluation(id)) continue;
      if (inFlight(id, entry.revision) || paramsFor(id, entry.revision)) continue;
      queued.push({ block: id, priority });
    }
    for (const evaluation of withParams) {
      for (const part of evaluation.parts.values()) {
        if (part.sent === undefined && part.result === undefined) {
          queued.push({ block: part.block, priority: evaluation.priority, part });
        }
      }
    }
    queued.sort(
      (a, b) =>
        (a.priority === b.priority ? 0 : a.priority > b.priority ? -1 : 1) ||
        (of.index.get(a.block) ?? 0) - (of.index.get(b.block) ?? 0),
    );

    let slots = evaluationQueue + 1 - sent.size;

    if (!holding) {
      for (const item of queued) {
        // An evaluation with parameters whose block cannot run rejects,
        // whether R has room for another evaluation or not.
        if (item.part !== undefined) {
          if (item.part.of.done) continue;
          const state = partState(item.part, of);
          if (state === "wait") continue;
          if (state !== "send") {
            fail(item.part.of, state.error);
            continue;
          }
          const recipe = recipeOf(item.block);
          const entry = entries.get(item.block);
          if (slots <= 0 || recipe === undefined || entry === undefined) continue;
          item.part.recipe = recipe;
          dispatch(item.block, entry.revision, recipe, { part: item.part });
          slots -= 1;
          continue;
        }
        if (slots <= 0 || !upstreamOf(of, item.block).every(available)) continue;
        const recipe = recipeOf(item.block);
        const entry = entries.get(item.block);
        if (recipe === undefined || entry === undefined) continue;
        dispatch(item.block, entry.revision, recipe, {});
        slots -= 1;
      }
    }

    // Evaluation in the background: once R has had nothing else to run, no
    // evaluation at 0 or above, no output and no request, for the delay, the
    // stale and unevaluated blocks that nothing needs, one at a time, at -1.
    const busy =
      asked.size > 0 ||
      [...sent].some((evaluation) => (priorityOf(evaluation) ?? 0) >= 0 || evaluation.orphaned) ||
      queued.some((item) => item.priority >= 0);

    if (busy || holding || !Number.isFinite(backgroundDelay)) {
      idleSince = undefined;
    } else {
      idleSince ??= now;
      const due = idleSince + backgroundDelay * 1000;
      if (now < due) {
        deadlines.push(due);
      } else if (slots > 0 && ![...sent].some((evaluation) => evaluation.background)) {
        background(of);
      }
    }

    if (wakeTimer !== undefined) {
      clearTimeout(wakeTimer);
      wakeTimer = undefined;
    }
    if (deadlines.length > 0) wake(Math.min(...deadlines));

    recompute();
    flush();
  }

  // Whether an evaluation with parameters still has to evaluate the block at
  // that revision, which the scheduler then evaluates no other way: one not
  // sent yet, which takes the revision the block has when it goes, or one sent
  // for that revision.
  function paramsFor(id: BlockId, at: number): boolean {
    for (const evaluation of withParams) {
      const part = evaluation.parts.get(id);
      if (part !== undefined && part.sent === undefined && part.result === undefined) {
        return true;
      }
    }
    for (const evaluation of sent) {
      if (evaluation.part?.block === id && evaluation.revision === at && !evaluation.orphaned) {
        return true;
      }
    }
    return false;
  }

  function background(of: Graph): void {
    const wanted = new Set<BlockId>();
    for (const id of of.order) {
      const status = statuses.get(id)?.status;
      if (!needs.has(id) && (status === "stale" || status === "unevaluated")) wanted.add(id);
    }
    // Their upstream, which may need evaluating for R to hold it again.
    for (let i = of.order.length - 1; i >= 0; i--) {
      const id = of.order[i] ?? "";
      if (!wanted.has(id)) continue;
      for (const up of upstreamOf(of, id)) if (!needs.has(up)) wanted.add(up);
    }
    for (const id of of.order) {
      const entry = entries.get(id);
      if (entry === undefined || !wanted.has(id) || !needsEvaluation(id)) continue;
      if (inFlight(id, entry.revision)) continue;
      const ready = upstreamOf(of, id).every((up) => {
        const above = entries.get(up);
        return statuses.get(up)?.status === "ready" && above?.held === above?.revision;
      });
      if (!ready) continue;
      const recipe = recipeOf(id);
      if (recipe === undefined) continue;
      dispatch(id, entry.revision, recipe, { background: true });
      return;
    }
  }

  function resendable(request: Asking): boolean {
    if (request.kind === "output" || request.output !== undefined) return true;
    if (request.registry_id === undefined) return false;
    const entry = own(store.blockRegistry(), request.registry_id);
    return entry === undefined || own(entry.requests, request.name)?.action !== true;
  }

  // Start

  let initial: Board | undefined;
  try {
    initial = store.board();
  } catch {
    initial = undefined;
  }
  if (initial !== undefined) {
    opened(initial);
    recompute();
    changed.clear();
  }

  store.subscribe((change, next) => {
    if ("opened" in change || board === undefined) opened(next);
    else applied(change.delta, next);
    recompute();
    flush();
    schedule();
  });

  const scheduler: Scheduler = {
    demand(owner, blocks, { priority } = {}) {
      const demand: Demanding = {
        owner,
        blocks: typeof blocks === "function" ? blocks : new Set(blocks),
        priority: checked(priority),
        last: undefined,
      };
      demands.add(demand);
      schedule();
      return {
        set(next, { priority: given } = {}) {
          if (!demands.has(demand)) {
            throw new Error(`The demand of ${owner} was released, so it can no longer be set.`);
          }
          demand.blocks = typeof next === "function" ? next : new Set(next);
          demand.priority = given === undefined ? demand.priority : checked(given);
          demand.last = undefined;
          schedule();
        },
        release() {
          if (demands.delete(demand)) schedule();
        },
      };
    },

    evaluate(blocks, params, { priority, signal } = {}) {
      const at = priority ?? 0;
      const invalid = priorityError(at);
      if (invalid !== undefined) return Promise.reject(invalid);
      if (signal?.aborted === true) return Promise.reject(asError(signal.reason));
      const ids = [...new Set(blocks)];

      return new Promise((resolve, reject) => {
        let onAbort = (): void => undefined;
        const settle = (
          result: { statuses: Record<BlockId, BlockStatus> } | { error: unknown },
        ) => {
          signal?.removeEventListener("abort", onAbort);
          if ("error" in result) reject(asError(result.error));
          else resolve(result.statuses);
        };

        if (params === undefined) {
          const oneTime: OneTime = {
            blocks: new Set(ids.filter((id) => entries.has(id))),
            priority: at,
            settle,
          };
          onAbort = () => {
            if (oneTimes.delete(oneTime)) {
              oneTime.settle({ error: signal?.reason });
              schedule();
            }
          };
          signal?.addEventListener("abort", onAbort, { once: true });
          if (oneTime.blocks.size === 0) {
            settle({ error: new Error(`The board holds none of the blocks ${quoted(ids)} to evaluate.`) });
            return;
          }
          oneTimes.add(oneTime);
          settleOneTimes();
        } else {
          const evaluation: WithParams = {
            priority: at,
            params,
            parts: new Map(),
            done: false,
            settle,
          };
          // Aborting drops the evaluations not sent yet, while R runs those it
          // has, since it never cancels one with parameters.
          onAbort = () => {
            fail(evaluation, signal?.reason);
            schedule();
          };
          signal?.addEventListener("abort", onAbort, { once: true });
          const lacking = ids.filter((id) => !entries.has(id));
          if (ids.length === 0 || lacking.length > 0) {
            fail(
              evaluation,
              new Error(
                ids.length === 0
                  ? "An evaluation with parameters names no block."
                  : `The board holds no ${lacking.length === 1 ? "block" : "blocks"} ${quoted(lacking)} to evaluate.`,
              ),
            );
            return;
          }
          for (const id of ids) {
            const status = statuses.get(id);
            if (status?.status === "waiting") {
              fail(evaluation, waitingError(id, status));
              return;
            }
            evaluation.parts.set(id, {
              of: evaluation,
              block: id,
              recipe: undefined,
              sent: undefined,
              result: undefined,
            });
          }
          withParams.add(evaluation);
        }
        schedule();
      });
    },

    invalidate(id) {
      if (graph === undefined || !entries.has(id)) return;
      const bumped = new Set([id]);
      for (const block of graph.order) {
        const entry = entries.get(block);
        if (entry === undefined) continue;
        if (block === id || upstreamOf(graph, block).some((up) => bumped.has(up))) {
          entry.revision = revision();
          bumped.add(block);
        }
      }
      recompute();
      flush();
      schedule();
    },

    status: (id) => statuses.get(id),

    subscribe(fn) {
      const wrapped = (ids: readonly BlockId[]) => {
        fn(ids);
      };
      subscribers.add(wrapped);
      return () => {
        subscribers.delete(wrapped);
      };
    },

    async request(request, requestOptions = {}) {
      // The results R said it lacks that the scheduler had evaluated again,
      // which it does once per request.
      const again = new Set<string>();

      for (;;) {
        const item: Asked = { seq: ++seq, at: Date.now() };
        asked.add(item);
        schedule();

        let result: { reply: Reply; body: Blob | null };
        try {
          result = await client.request(request, requestOptions);
        } catch (error) {
          if (error instanceof SessionEndedError && resendable(request)) continue;
          throw error;
        } finally {
          asked.delete(item);
          schedule();
        }

        const { reply } = result;
        if (reply.status !== "missing") return result;

        const lacked = reply.missing;
        const current = lacked.every(
          ({ block, revision: at }) =>
            entries.get(block)?.revision === at &&
            statuses.get(block)?.status === "ready" &&
            !again.has(JSON.stringify([block, at])),
        );
        if (!current) return result;

        for (const { block, revision: at } of lacked) {
          again.add(JSON.stringify([block, at]));
          const entry = entries.get(block);
          if (entry?.held === at) entry.held = undefined;
        }

        let found: Record<BlockId, BlockStatus>;
        try {
          found = await scheduler.evaluate(
            lacked.map(({ block }) => block),
            undefined,
            requestOptions.signal === undefined ? {} : { signal: requestOptions.signal },
          );
        } catch {
          return requestOptions.signal?.aborted === true
            ? { reply: { id: reply.id, status: "cancelled" }, body: null }
            : result;
        }
        if (
          !lacked.every(
            ({ block, revision: at }) =>
              found[block]?.revision === at && found[block].status === "ready",
          )
        ) {
          return result;
        }
      }
    },

    start() {
      holding = false;
      schedule();
    },

    hold() {
      holding = true;
    },

    session() {
      for (const entry of entries.values()) entry.held = undefined;
      schedule();
    },
  };

  return scheduler;
}

function sameInputs(a: readonly Input[], b: readonly Input[]): boolean {
  return (
    a.length === b.length &&
    a.every((input, i) => input.input === b[i]?.input && input.block === b[i].block)
  );
}

// Why an evaluation with parameters is not sent, from the condition of the
// block that reads waiting.
function waitingError(id: BlockId, status: BlockStatus): Error {
  const why = status.conditions[0]?.message ?? "This block is waiting.";
  return new Error(
    `${why.replace(/^This block/, `Block "${id}"`)} Its evaluation with parameters is not sent.`,
  );
}

// A blockr option's value, or its default where it is no number of 0 or more,
// which the scheduler reports.
function setting(value: number | undefined, fallback: number, name: string): number {
  if (value === undefined) return fallback;
  if (typeof value === "number" && value >= 0) return value;
  report(
    new Error(
      `The blockr option ${name} is ${String(value)}, where it takes a number of 0 or more, so the scheduler takes ${String(fallback)}.`,
    ),
  );
  return fallback;
}

function checked(priority: number | undefined): number {
  const at = priority ?? 0;
  const invalid = priorityError(at);
  if (invalid !== undefined) throw invalid;
  return at;
}

function priorityError(priority: number): TypeError | undefined {
  return typeof priority !== "number" || Number.isNaN(priority)
    ? new TypeError(`An evaluation priority is a number, not ${String(priority)}.`)
    : undefined;
}

function asError(error: unknown): Error {
  return error instanceof Error ? error : new Error(String(error));
}

// Reports what a caller's function threw, or what went wrong where nobody
// waits on it, without stopping the scheduler.
function report(error: unknown): void {
  queueMicrotask(() => {
    throw error;
  });
}
