// The scheduler tracks what the client knows about evaluation. Front-ends,
// extensions and packages ask it for evaluations, as Evaluation in the client
// has it (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.10). It
// does three things:
// - It keeps a revision for each block. The revision goes up whenever the
//   block's recipe changes or something invalidates the block.
// - It has R evaluate what is demanded, with everything upstream of it.
//   Evaluations go by the evaluation priority each demand gives, and in
//   topological order within one priority.
// - It keeps each block's status from R's replies.
//
// There is one scheduler per board. Core creates it beside the store and hands
// it to front-ends and extensions through `CoreApi`.

import { SessionEndedError } from "../protocol/client.ts";
import type { Client, RequestOptions } from "../protocol/client.ts";
import type { BlockRevision, Condition, Recipe, Reply } from "../protocol/messages.ts";
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
  // returned demand is released. The blocks are a set, such as what a view
  // shows, or a function of the board. The scheduler calls such a function
  // again after every change to the board. The evaluation priority is 0 unless
  // given. The owner, such as "blockr.dock", says whose demand it is. The
  // scheduler names the owner where needed, as when a function of the board
  // throws.
  demand(
    owner: string,
    blocks: Iterable<BlockId> | ((board: Board) => Iterable<BlockId>),
    options?: { priority?: number },
  ): Demand;
  // A one-time evaluation. It brings the blocks and their upstream up to date
  // without keeping them so. It resolves with each block's status by id once
  // none of the blocks reads stale or unevaluated, at whatever revision each
  // has by then. A block that leaves the board meanwhile is left out. The
  // evaluation rejects once it is left with no block, or when another board
  // opens. It stops once the signal aborts.
  //
  // With parameters, it evaluates each block once, whether the block is stale
  // or not. R passes the parameters to the block's block_eval() method. Each
  // evaluation goes once R holds what the block reads, at the revision the
  // block has then. Once sent, it is never cancelled. The evaluation resolves
  // with the statuses R's replies give. It rejects where a block reads waiting,
  // since no result is in sight.
  evaluate(
    blocks: Iterable<BlockId>,
    params?: Record<string, Json>,
    options?: { priority?: number; signal?: AbortSignal },
  ): Promise<Record<BlockId, BlockStatus>>;
  // Gives the block and every block downstream of it a new revision. They
  // evaluate again where something demands them, and turn stale where nothing
  // does. A block that the board lacks is passed over.
  invalidate(id: BlockId): void;
  // The block's status now, or undefined for a block that the board lacks. It
  // is the same object until subscribe() announces that it changed.
  status(id: BlockId): BlockStatus | undefined;
  // Calls `fn` after statuses change, with the blocks whose status changed.
  subscribe(fn: (changed: readonly BlockId[]) => void): () => void;

  // Core alone calls the methods below. Front-ends and extensions get only
  // those above.

  // Sends an output or a request to R through the client, as the client's own
  // request() does. This way the scheduler sees everything R is asked to run.
  // When the session ends before R replies, it sends an output, an output
  // class's request and a block class's request to the next session again. It
  // rejects an action, a board option's request and an extension's request
  // with the SessionEndedError, since they may have run. A reply of `missing`
  // goes back to the caller. The caller asks again once the blocks are
  // evaluated at their current revisions. Only a fault in R makes R lack a
  // result that reads ready at the block's current revision. The scheduler
  // then invalidates that block first.
  request(
    request: Extract<Parameters<Client["request"]>[0], { kind: "request" | "output" }>,
    options?: RequestOptions,
  ): Promise<{ reply: Reply; body: Blob | null }>;
  // Lets the scheduler send evaluations. It holds them from its creation until
  // then. Core calls it once the front-end's render() has returned.
  start(): void;
  // Holds evaluations again until start(), as while the page switches to
  // another front-end.
  hold(): void;
  // Hears that R answered the hello of a new session. That session holds no
  // results, so every block gets a new revision. Every block then reads stale
  // until it is evaluated again.
  session(): void;
}

// A demand that demand() returns. Only its holder replaces or releases it.
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
  // The blockr options that the scheduler reads. Core asks R for them as soon
  // as R has answered its hello.
  //
  // The `evaluation_queue` blockr option says how many evaluations the
  // scheduler keeps waiting in R, behind the one R runs. It is 2 unless given.
  // With 0, the scheduler sends the next evaluation only once the last reply is
  // back. With Infinity, it sends every evaluation at once.
  readonly evaluationQueue?: number;
  // The `background_evaluation_delay` blockr option, Infinity unless given.
  // Once R has had nothing else to run for that many seconds, the scheduler
  // evaluates the stale and unevaluated blocks that nothing demands. With
  // Infinity, it evaluates nothing in the background.
  readonly backgroundEvaluationDelay?: number;
  // The `gate_visibility` blockr option, true unless given. Set to false, it
  // has the scheduler demand every block.
  readonly gateVisibility?: boolean;
  // The inputs that R binds itself, for a block that `serve()` serves alone,
  // from R's reply to hello. They count as linked on the board R sent, until
  // another board opens.
  readonly bound?: readonly string[];
}

// The client's request() of an output or a request.
type Asking = Parameters<Scheduler["request"]>[0];

// What the scheduler keeps for a block.
interface Entry {
  revision: number;
  // What the revision was taken for: the class, the state and the inputs. A
  // change to any of them gives the block a new revision, and so does a new
  // revision of a block upstream.
  registryId: RegistryId;
  state: Readonly<Record<string, Json>>;
  inputs: readonly Input[];
  // What its latest reply said. The block's status comes from it.
  outcome: Outcome | undefined;
}

// An evaluation sent to R and not yet answered.
interface Sent {
  // When it was sent. One count orders everything the scheduler sends.
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
  // Whether another board opened since. Its reply then counts for nothing.
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

// A one-time evaluation with parameters. It is an evaluation of each of its
// blocks.
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
  // The recipe it went to R with, at the revision the block had then. It is
  // kept until R replies.
  recipe: Recipe | undefined;
  sent: Sent | undefined;
  result: BlockStatus | undefined;
}

// How long anything else may wait in R behind an evaluation below evaluation
// priority 0. After that, the scheduler cancels that evaluation.
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
  // changed has ended. By then every subscriber of the store has heard of the
  // change. A front-end that changes its demand from its own subscription is
  // then heard first.
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
    };
  }

  // A board that opens holds no result in R, since R forgets every block. So
  // every block of it reads unevaluated, with a revision above any its id had.
  function opened(next: Board): void {
    if (board !== undefined) {
      const old = Object.keys(board.blocks);
      // R forgets every block as it opens a board. This forget also clears what
      // an evaluation sent since may have left in R.
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
  // state, the links into it or the revision of any block upstream. A rename,
  // an attribute, a stack, the front-end's state and a board option change no
  // revision.
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

  // Why a block cannot run, if it cannot. A block cannot run in three cases:
  // - An input has no link, and R does not bind it.
  // - A variadic block has no link.
  // - A block upstream reads failed, invalid or waiting.
  function why(id: BlockId, of: Graph): Condition | undefined {
    const unlinked = of.unlinked.get(id);
    if (unlinked !== undefined) return waitingFor({ unlinked });
    const upstream = upstreamOf(of, id).filter((up) => {
      const status = statuses.get(up)?.status;
      return status === "failed" || status === "invalid" || status === "waiting";
    });
    return upstream.length > 0 ? waitingFor({ upstream }) : undefined;
  }

  // Works out every block's status, upstream first. A status that reads as
  // before keeps its object.
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

  // Whether a block needs evaluating to be current. It does where it reads
  // stale or unevaluated.
  function needsEvaluation(id: BlockId): boolean {
    const status = statuses.get(id)?.status;
    return status === "stale" || status === "unevaluated";
  }

  // Whether R holds the block's result at its current revision. A result that
  // is being computed counts too, since R computes it before anything sent
  // after it.
  function available(id: BlockId): boolean {
    const entry = entries.get(id);
    if (entry === undefined) return false;
    return statuses.get(id)?.status === "ready" || inFlight(id, entry.revision);
  }

  // Gives the blocks and every block downstream of them a new revision. They
  // then read stale until they are evaluated again.
  function bump(roots: ReadonlySet<BlockId>): void {
    if (graph === undefined) return;
    const bumped = new Set<BlockId>();
    for (const id of graph.order) {
      const entry = entries.get(id);
      if (entry === undefined) continue;
      if (roots.has(id) || upstreamOf(graph, id).some((up) => bumped.has(up))) {
        entry.revision = revision();
        bumped.add(id);
      }
    }
  }

  // Invalidates each block that R says it lacks while the block reads ready at
  // that revision. Only a fault in R causes that. The block is then evaluated
  // again where something needs it.
  function lacking(missing: readonly BlockRevision[]): void {
    const lost = new Set<BlockId>();
    for (const { block, revision: at } of missing) {
      if (entries.get(block)?.revision === at && statuses.get(block)?.status === "ready") {
        lost.add(block);
      }
    }
    if (lost.size > 0) bump(lost);
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
  // revision, and the block reads anything but waiting. A reply of `cancelled`
  // or `missing` counts as none.
  function answered(evaluation: Sent, reply: Reply): void {
    sent.delete(evaluation);

    if (!evaluation.orphaned) {
      if (reply.status === "missing") lacking(reply.missing);

      const outcome = outcomeOf(reply, evaluation.revision);
      const entry = entries.get(evaluation.block);
      if (
        outcome !== undefined &&
        evaluation.revision === entry?.revision &&
        statuses.get(evaluation.block)?.status !== "waiting"
      ) {
        entry.outcome = outcome;
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
      if (!(error instanceof SessionEndedError) && evaluation.part === undefined) {
        // The transport could not send it. The block then fails with why,
        // rather than being sent again and again.
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
      // R ran nothing, so the evaluation goes again. It takes the recipe the
      // block has once R holds what the block reads.
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

  // Whether the evaluation of a block with parameters can go. It goes once R
  // holds what the block reads at their current revisions, or computes them
  // first. A block that reads waiting has no result in sight, so the evaluation
  // rejects.
  function partState(part: Part, of: Graph): "send" | "wait" | { error: Error } {
    const status = statuses.get(part.block);
    if (status?.status === "waiting") return { error: waitingError(part.block, status) };
    return upstreamOf(of, part.block).every(available) ? "send" : "wait";
  }

  // The pass: what to cancel and what to send

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

  // The evaluation priority an evaluation has now. It is that of what needs its
  // block, or -1 for evaluation in the background. It is undefined where
  // nothing needs the block any more.
  function priorityOf(evaluation: Sent): number | undefined {
    if (evaluation.part !== undefined) return evaluation.part.of.priority;
    return needs.get(evaluation.block) ?? (evaluation.background ? -1 : undefined);
  }

  // Since when something else has waited in R behind an evaluation: an
  // evaluation at a higher evaluation priority, an output or a request.
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

    // What is needed: what is demanded, and what a one-time evaluation waits
    // on. Each block takes the highest evaluation priority of anything that
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
    // What an evaluation with parameters reads stays needed until R has replied
    // to it. Nothing then cancels an evaluation that it waits on in R. Until it
    // is sent, that is what the block reads now. From then on, it is what its
    // recipe names.
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

    // What to cancel, in three cases:
    // - An evaluation whose revision has moved.
    // - One that R has not started and nothing needs any more.
    // - One below 0 that something else has waited behind for 5 s, where R can
    //   stop what it has started.
    const interrupt = client.capabilities().has("interrupt");
    let oldest = Infinity;
    for (const item of [...sent, ...asked]) oldest = Math.min(oldest, item.seq);

    for (const evaluation of sent) {
      if (evaluation.cancelled || evaluation.orphaned || evaluation.part !== undefined) continue;
      const entry = entries.get(evaluation.block);
      // R drops the evaluation of a block that left the board, as it forgets
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

    // What to send: the blocks that are needed and not current. They go highest
    // evaluation priority first, and in topological order within one priority.
    // Each goes once R holds what it reads, or computes it first.
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

    // Evaluation in the background. R counts as idle while it has no evaluation
    // at 0 or above, no output and no request to run. Once R has been idle for
    // the delay, the scheduler evaluates the stale and unevaluated blocks that
    // nothing needs, one at a time, at -1.
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
  // that revision. The scheduler then evaluates the block no other way. It
  // counts one not sent yet, which takes the revision the block has when it
  // goes. It also counts one sent for that revision.
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
    for (const id of of.order) {
      const entry = entries.get(id);
      if (entry === undefined || !wanted.has(id) || inFlight(id, entry.revision)) continue;
      // One at a time: what the block reads has to be in R already.
      const ready = upstreamOf(of, id).every((up) => statuses.get(up)?.status === "ready");
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
          // Aborting drops the evaluations not sent yet. R runs those already
          // sent, since the scheduler never cancels one with parameters.
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
      if (!entries.has(id)) return;
      bump(new Set([id]));
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
      for (;;) {
        const item: Asked = { seq: ++seq, at: Date.now() };
        asked.add(item);
        schedule();

        try {
          const result = await client.request(request, requestOptions);
          if (result.reply.status === "missing") {
            lacking(result.reply.missing);
            recompute();
            flush();
          }
          return result;
        } catch (error) {
          if (error instanceof SessionEndedError && resendable(request)) continue;
          throw error;
        } finally {
          asked.delete(item);
          schedule();
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
      bump(new Set(entries.keys()));
      recompute();
      flush();
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

// A blockr option's value, or its default where the value is no number of 0 or
// more. The scheduler reports such a value.
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

// Reports what a caller's function threw, or an error that nobody waits on,
// without stopping the scheduler.
function report(error: unknown): void {
  queueMicrotask(() => {
    throw error;
  });
}
