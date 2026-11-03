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

import { graphOf } from "../graph/index.ts";
import type { Graph } from "../graph/index.ts";
import { SessionEndedError } from "../protocol/client.ts";
import type { Client, RequestOptions } from "../protocol/client.ts";
import type { BlockRevision, Recipe, Reply } from "../protocol/messages.ts";
import type { Delta } from "../store/delta.ts";
import type { Store } from "../store/store.ts";
import { messageOf, own, quoted } from "../store/util.ts";
import type { BlockId, Board } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import { needsOf } from "./needs.ts";
import type { Need } from "./needs.ts";
import { planOf } from "./plan.ts";
import type * as planned from "./plan.ts";
import { inputsOf } from "./recipes.ts";
import type { Inputs } from "./recipes.ts";
import { bump, counter, opening, revise } from "./revisions.ts";
import { outcomeOf, replied, statusesOf, unsent, waitingError } from "./status.ts";
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

// An evaluation sent to R and not yet answered.
interface Sent extends planned.Sent {
  readonly controller: AbortController;
  readonly part: Part | undefined;
  cancelled: boolean;
  orphaned: boolean;
}

// What ends a one-time evaluation: the statuses it resolves with, or why it
// rejects.
type Settle = (result: { statuses: Record<BlockId, BlockStatus> } | { error: unknown }) => void;

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
  readonly settle: Settle;
}

// A one-time evaluation with parameters. It is an evaluation of each of its
// blocks.
interface WithParams {
  readonly priority: number;
  readonly params: Record<string, Json>;
  readonly parts: Map<BlockId, Part>;
  done: boolean;
  readonly settle: Settle;
}

interface Part extends planned.Part {
  readonly of: WithParams;
  // The recipe it went to R with, at the revision the block had then. It is
  // kept until R replies.
  recipe: Recipe | undefined;
  sent: Sent | undefined;
  result: BlockStatus | undefined;
}

export function createScheduler(options: SchedulerOptions): Scheduler {
  const { store, client } = options;
  const queue = setting(options.evaluationQueue, 2, "evaluation_queue");
  const delay = setting(options.backgroundEvaluationDelay, Infinity, "background_evaluation_delay");
  let bound: ReadonlySet<string> = new Set(options.bound ?? []);

  const next = counter();
  // What stopped the scheduler, such as running out of revisions.
  let stopped: unknown;
  let holding = true;

  // The board as the scheduler last saw it, its graph, and its blocks' inputs.
  let board: Board | undefined;
  let graph: Graph = graphOf({ blocks: {}, links: {} });
  let inputs = new Map<BlockId, Inputs>();
  // Each block's revision, what its latest reply said, and its status.
  let revisions = new Map<BlockId, number>();
  const outcomes = new Map<BlockId, Outcome>();
  let statuses = new Map<BlockId, BlockStatus>();
  // The blocks whose status changed since the subscribers last heard.
  const unheard = new Set<BlockId>();
  const subscribers = new Set<(changed: readonly BlockId[]) => void>();

  const demands = new Set<Demanding>();
  const oneTimes = new Set<OneTime>();
  const withParams = new Set<WithParams>();
  const sent = new Set<Sent>();
  const asked = new Set<planned.Asked>();
  let seq = 0;

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

  function wake(at: number | undefined): void {
    if (wakeTimer !== undefined) clearTimeout(wakeTimer);
    wakeTimer = undefined;
    if (at === undefined) return;
    wakeTimer = setTimeout(
      () => {
        wakeTimer = undefined;
        pass();
      },
      Math.max(0, at - Date.now()),
    );
  }

  // Following the board

  // Takes the board that the scheduler follows from now on, with its graph and
  // its blocks' inputs.
  function read(next: Board): void {
    board = next;
    graph = graphOf(next);
    inputs = inputsOf(next, graph, store.blockRegistry(), bound);
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
    for (const demand of demands) demand.last = undefined;

    for (const id of statuses.keys()) unheard.add(id);
    statuses = new Map();
    outcomes.clear();
    read(next);
    revisions = opening(graph, revision);
  }

  function applied(delta: Delta, next: Board): void {
    const before = { board: board ?? next, inputs };
    read(next);
    const revised = revise(
      revisions,
      before,
      { board: next, inputs, graph },
      new Set(delta.blocks?.rm ?? []),
      revision,
    );
    revisions = revised.revisions;
    if (revised.gone.length > 0) {
      left(new Set(revised.gone));
      client.forget(revised.gone);
    }
  }

  // Forgets what the scheduler kept for blocks that left the board, as R
  // forgets their results.
  function left(ids: ReadonlySet<BlockId>): void {
    for (const id of ids) outcomes.delete(id);
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

  // Invalidates each block that R says it lacks while the block reads ready at
  // that revision. Only a fault in R causes that. The block is then evaluated
  // again where something needs it.
  function lacking(missing: readonly BlockRevision[]): void {
    const lost = new Set<BlockId>();
    for (const { block, revision: at } of missing) {
      if (revisions.get(block) === at && statuses.get(block)?.status === "ready") lost.add(block);
    }
    if (lost.size > 0) revisions = bump(graph, revisions, lost, revision);
  }

  // Statuses

  function restatus(): void {
    const running = new Set<BlockId>();
    for (const evaluation of sent) {
      if (!evaluation.orphaned) running.add(evaluation.block);
    }
    const result = statusesOf({ graph, revisions, outcomes, inputs, running, before: statuses });
    statuses = result.statuses;
    for (const id of result.changed) unheard.add(id);
  }

  // Settles the one-time evaluations whose blocks are current, then tells the
  // subscribers which statuses changed.
  function flush(): void {
    settleOneTimes();
    if (unheard.size === 0) return;
    const ids = [...unheard];
    unheard.clear();
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
      if (ids.some(notCurrent)) continue;
      oneTimes.delete(oneTime);
      const result: Record<BlockId, BlockStatus> = {};
      for (const id of ids) {
        const status = statuses.get(id);
        if (status !== undefined) result[id] = status;
      }
      oneTime.settle({ statuses: result });
    }
  }

  function notCurrent(id: BlockId): boolean {
    const status = statuses.get(id)?.status;
    return status === "stale" || status === "unevaluated";
  }

  // Sending

  function recipeOf(id: BlockId): Recipe | undefined {
    const block = board === undefined ? undefined : own(board.blocks, id);
    const recipe = inputs.get(id)?.recipe;
    if (block === undefined || recipe === undefined) return undefined;
    return {
      registry_id: block.registry_id,
      state: { ...block.state },
      inputs: recipe.map((input) =>
        input.block === undefined
          ? { input: input.input }
          : { input: input.input, block: input.block, revision: revisions.get(input.block) ?? 0 },
      ),
    };
  }

  function send(item: { readonly block: BlockId; readonly part?: Part; readonly background?: true }): void {
    const recipe = recipeOf(item.block);
    const at = revisions.get(item.block);
    if (recipe === undefined || at === undefined) return;
    if (item.part !== undefined) item.part.recipe = recipe;

    const evaluation: Sent = {
      seq: ++seq,
      at: Date.now(),
      block: item.block,
      revision: at,
      controller: new AbortController(),
      part: item.part,
      background: item.background ?? false,
      cancelled: false,
      orphaned: false,
    };
    sent.add(evaluation);
    if (item.part !== undefined) item.part.sent = evaluation;

    client
      .request(
        {
          kind: "evaluate",
          block: item.block,
          revision: at,
          recipe,
          ...(item.part === undefined ? {} : { params: item.part.of.params }),
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
      if (
        outcome !== undefined &&
        evaluation.revision === revisions.get(evaluation.block) &&
        statuses.get(evaluation.block)?.status !== "waiting"
      ) {
        outcomes.set(evaluation.block, outcome);
      }

      if (evaluation.part !== undefined) {
        partAnswered(evaluation.part, reply, outcome, evaluation.revision);
      }
    }

    restatus();
    flush();
    schedule();
  }

  function unanswered(evaluation: Sent, error: unknown): void {
    sent.delete(evaluation);

    if (!evaluation.orphaned) {
      if (!(error instanceof SessionEndedError) && evaluation.part === undefined) {
        // The transport could not send it. The block then fails with why,
        // rather than being sent again and again.
        if (revisions.get(evaluation.block) === evaluation.revision) {
          outcomes.set(evaluation.block, unsent(evaluation.revision, messageOf(error)));
        }
      }
      if (evaluation.part !== undefined) fail(evaluation.part.of, error);
    }

    restatus();
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

  // Starts a one-time evaluation without parameters, and returns what stops
  // it. It rejects at once where the board holds none of its blocks.
  function startOneTime(ids: readonly BlockId[], priority: number, settle: Settle) {
    const oneTime: OneTime = {
      blocks: new Set(ids.filter((id) => revisions.has(id))),
      priority,
      settle,
    };
    if (oneTime.blocks.size === 0) {
      settle({ error: new Error(`The board holds none of the blocks ${quoted(ids)} to evaluate.`) });
      return () => undefined;
    }
    oneTimes.add(oneTime);
    settleOneTimes();
    return (reason: unknown) => {
      if (oneTimes.delete(oneTime)) {
        settle({ error: reason });
        schedule();
      }
    };
  }

  // Starts a one-time evaluation with parameters, and returns what stops it.
  // Stopping drops the evaluations not sent yet. R runs those already sent,
  // since the scheduler never cancels one with parameters. The evaluation
  // rejects at once where it names a block that the board lacks or that reads
  // waiting.
  function startWithParams(
    ids: readonly BlockId[],
    params: Record<string, Json>,
    priority: number,
    settle: Settle,
  ) {
    const evaluation: WithParams = { priority, params, parts: new Map(), done: false, settle };
    const stop = (reason: unknown) => {
      fail(evaluation, reason);
      schedule();
    };

    const absent = ids.filter((id) => !revisions.has(id));
    if (ids.length === 0 || absent.length > 0) {
      fail(
        evaluation,
        new Error(
          ids.length === 0
            ? "An evaluation with parameters names no block."
            : `The board holds no ${absent.length === 1 ? "block" : "blocks"} ${quoted(absent)} to evaluate.`,
        ),
      );
      return stop;
    }
    for (const id of ids) {
      const status = statuses.get(id);
      if (status?.status === "waiting") {
        fail(evaluation, waitingError(id, status));
        return stop;
      }
      evaluation.parts.set(id, { of: evaluation, block: id, recipe: undefined, sent: undefined, result: undefined });
    }
    withParams.add(evaluation);
    return stop;
  }

  // The pass

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

  // Everything that needs blocks evaluated, each at its evaluation priority:
  // the demands, the one-time evaluations, and what an evaluation with
  // parameters reads. What an evaluation with parameters reads stays needed
  // until R has replied to it, so that nothing cancels an evaluation that it
  // waits on in R. Until it is sent, that is what the block reads now. From
  // then on, it is what its recipe names.
  function needed(of: Board): Need[] {
    const found: Need[] = [];
    for (const demand of demands) {
      found.push({ blocks: demanded(demand, of), priority: demand.priority });
    }
    for (const oneTime of oneTimes) {
      found.push({ blocks: oneTime.blocks, priority: oneTime.priority });
    }
    for (const evaluation of withParams) {
      for (const part of evaluation.parts.values()) {
        if (part.result !== undefined) continue;
        const reads =
          part.recipe === undefined
            ? graph.upstream(part.block)
            : part.recipe.inputs.flatMap((input) => input.block ?? []);
        found.push({ blocks: reads, priority: evaluation.priority });
      }
    }
    return found;
  }

  // The evaluations with parameters that are not sent yet.
  function pending(): Part[] {
    return [...withParams].flatMap((evaluation) =>
      [...evaluation.parts.values()].filter(
        (part) => part.sent === undefined && part.result === undefined,
      ),
    );
  }

  function pass(): void {
    passTimer = undefined;
    if (stopped !== undefined || board === undefined) return;

    const plan = planOf({
      graph,
      revisions,
      statuses,
      needs: needsOf(graph, needed(board)),
      sent,
      asked,
      parts: pending(),
      queue,
      interrupt: client.capabilities().has("interrupt"),
      holding,
      now: Date.now(),
      idleSince,
      delay,
    });

    idleSince = plan.idleSince;
    for (const evaluation of plan.cancel) cancel(evaluation);
    for (const { part, error } of plan.reject) fail(part.of, error);
    for (const item of plan.send) send(item);
    wake(plan.wakeAt);

    restatus();
    flush();
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
    restatus();
    unheard.clear();
  }

  store.subscribe((change, next) => {
    if ("opened" in change || board === undefined) opened(next);
    else applied(change.delta, next);
    restatus();
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
        // What the signal stops once it aborts. The evaluation sets it as it
        // starts.
        let stop: (reason: unknown) => void = () => undefined;
        const onAbort = () => {
          stop(signal?.reason);
        };
        signal?.addEventListener("abort", onAbort, { once: true });
        const settle: Settle = (result) => {
          signal?.removeEventListener("abort", onAbort);
          if ("error" in result) reject(asError(result.error));
          else resolve(result.statuses);
        };
        stop =
          params === undefined
            ? startOneTime(ids, at, settle)
            : startWithParams(ids, params, at, settle);
        schedule();
      });
    },

    invalidate(id) {
      if (!revisions.has(id)) return;
      revisions = bump(graph, revisions, new Set([id]), revision);
      restatus();
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
        const item: planned.Asked = { seq: ++seq, at: Date.now() };
        asked.add(item);
        schedule();

        try {
          const result = await client.request(request, requestOptions);
          if (result.reply.status === "missing") {
            lacking(result.reply.missing);
            restatus();
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
      revisions = bump(graph, revisions, new Set(revisions.keys()), revision);
      restatus();
      flush();
      schedule();
    },
  };

  return scheduler;
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
