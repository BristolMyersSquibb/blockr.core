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
//
// What the scheduler knows sits in signals. They change only in its passes and
// in its handlers of what happens, such as a change to the board, a reply from
// R or a call of one of its methods. Everything else follows from them with
// computed(): the graph, each block's inputs and status, and what is needed.
// One effect tells the subscribers which statuses changed, and has a pass work
// out what to send. The parts that decide are plain functions in the files
// beside this one.

import { action, batch, computed, effect, signal, untracked } from "@preact/signals-core";
import type { ReadonlySignal } from "@preact/signals-core";
import { graphOf } from "../graph/index.ts";
import { SessionEndedError } from "../protocol/client.ts";
import type { Client, RequestOptions } from "../protocol/client.ts";
import type { BlockRevision, Recipe, Reply } from "../protocol/messages.ts";
import type { Delta } from "../store/delta.ts";
import type { Store } from "../store/store.ts";
import type { BlockId, Board, RegistryId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { asError, messageOf, own, report } from "../util/index.ts";
import * as evaluations from "./evaluations.ts";
import type { OneTime, Settled } from "./evaluations.ts";
import { needsOf } from "./needs.ts";
import { planOf } from "./plan.ts";
import type { Asked, Plan, Sent } from "./plan.ts";
import { inputsOf } from "./recipes.ts";
import { bump, counter, opening, revise } from "./revisions.ts";
import { outcomeOf, statusesOf, unsent } from "./status.ts";
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
  readonly store: Pick<Store, "board" | "hasBoard" | "subscribe" | "blockRegistry">;
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

// A demand as the scheduler keeps it.
interface Demanding {
  readonly owner: string;
  readonly priority: number;
  // The blocks it demands of the board as it is.
  readonly blocks: ReadonlySignal<ReadonlySet<BlockId>>;
}

const NO_BOARD: Pick<Board, "blocks" | "links"> = { blocks: {}, links: {} };

export function createScheduler(options: SchedulerOptions): Scheduler {
  const { store, client } = options;
  const queue = setting(options.evaluationQueue, 2, "evaluation_queue");
  const delay = setting(options.backgroundEvaluationDelay, Infinity, "background_evaluation_delay");

  // What the scheduler knows:
  // - The board as it last saw it, the block registry the board opened with,
  //   and the inputs that R binds itself.
  // - Each block's revision, and what R's latest reply for it said.
  // - The evaluations sent to R and not yet answered, and the outputs and
  //   requests.
  // - The demands and the one-time evaluations.
  // - Whether it holds evaluations back.
  const board = signal<Board | undefined>(undefined);
  const registry = signal<Readonly<Record<RegistryId, RegistryEntry>>>({});
  const bound = signal<ReadonlySet<string>>(new Set(options.bound ?? []));
  const revisions = signal<ReadonlyMap<BlockId, number>>(new Map());
  const outcomes = signal<ReadonlyMap<BlockId, Outcome>>(new Map());
  const sent = signal<ReadonlyMap<number, Sent>>(new Map());
  const asked = signal<ReadonlyMap<number, Asked>>(new Map());
  const demands = signal<ReadonlyMap<number, Demanding>>(new Map());
  const oneTimes = signal<ReadonlyMap<number, OneTime>>(new Map());
  const holding = signal(true);

  // What follows from what the scheduler knows.
  const graph = computed(() => graphOf(board.value ?? NO_BOARD));
  const inputs = computed(() =>
    inputsOf(board.value ?? NO_BOARD, graph.value, registry.value, bound.value),
  );
  // A status that reads as before keeps its object, so each computation
  // starts from the last.
  let last: ReadonlyMap<BlockId, BlockStatus> = new Map();
  const statuses = computed(() => {
    last = statusesOf({
      graph: graph.value,
      revisions: revisions.value,
      outcomes: outcomes.value,
      inputs: inputs.value,
      running: running(sent.value),
      before: last,
    }).statuses;
    return last;
  });
  // The blocks needed, each at its evaluation priority: what the demands and
  // the one-time evaluations need, with everything upstream. Only a pass
  // reads it, so a demand's function of the board runs once a pass needs it,
  // and once for each board.
  const needs = computed(() =>
    needsOf(graph.value, [
      ...[...demands.value.values()].map((demand) => ({
        blocks: demand.blocks.value,
        priority: demand.priority,
      })),
      ...evaluations.neededBy(oneTimes.value, graph.value),
    ]),
  );

  // All that the scheduler knows, as one value that changes whenever any part
  // of it does.
  const state = computed(() => [
    board.value,
    registry.value,
    bound.value,
    revisions.value,
    outcomes.value,
    sent.value,
    asked.value,
    demands.value,
    oneTimes.value,
    holding.value,
  ]);

  // What the scheduler holds beside what it knows: what stops each evaluation
  // sent, and what settles the promise of each one-time evaluation.
  const controllers = new Map<number, AbortController>();
  const settles = new Map<number, (result: Settled) => void>();
  const subscribers = new Set<(changed: readonly BlockId[]) => void>();

  // One count gives ids to demands, one-time evaluations, and what the
  // scheduler sends. So the ids of what it sends go up in the order it sends
  // them.
  let ids = 0;
  const count = counter();
  // What stopped the scheduler, such as running out of revisions.
  let stopped: unknown;
  // Since when R has had nothing else to run than evaluations below 0, as the
  // last pass found.
  let idleSince: number | undefined;
  // All that the scheduler knew as the last pass ended.
  let planned: unknown;
  let passTimer: ReturnType<typeof setTimeout> | undefined;
  let wakeTimer: ReturnType<typeof setTimeout> | undefined;

  function revision(): number {
    try {
      return count();
    } catch (error) {
      stopped = error;
      throw error;
    }
  }

  // Following the board

  // A board that opens holds no result in R, since R forgets every block. So
  // every block of it reads unevaluated, with a revision above any its id had.
  // Every one-time evaluation rejects, and the replies to the evaluations sent
  // count for nothing.
  const opened = action((next: Board) => {
    const before = board.value;
    if (before !== undefined) {
      const old = Object.keys(before.blocks);
      // R forgets every block as it opens a board. This forget also clears what
      // an evaluation sent since may have left in R.
      if (old.length > 0) client.forget(old);
      bound.value = new Set();
    }

    sent.value = new Map(
      [...sent.value].map(([id, evaluation]) => [id, { ...evaluation, orphaned: true }]),
    );
    const error = new Error("Another board opened.");
    update({
      oneTimes: new Map(),
      settled: [...oneTimes.value.keys()].map((id) => [id, { error }]),
    });

    registry.value = store.blockRegistry();
    board.value = next;
    outcomes.value = new Map();
    revisions.value = opening(graph.value, revision);
  });

  // A change to the board moves the revisions of the blocks whose recipe it
  // changed. The blocks that left the board go, as R forgets them.
  const applied = action((delta: Delta, next: Board) => {
    const before = { board: board.value ?? next, inputs: inputs.value };
    board.value = next;
    const revised = revise(
      revisions.value,
      before,
      { board: next, inputs: inputs.value, graph: graph.value },
      new Set(delta.blocks?.rm ?? []),
      revision,
    );
    revisions.value = revised.revisions;
    if (revised.gone.length === 0) return;

    const gone = new Set(revised.gone);
    outcomes.value = new Map([...outcomes.value].filter(([id]) => !gone.has(id)));
    update(evaluations.left(oneTimes.value, gone));
    client.forget(revised.gone);
  });

  // Invalidates each block that R says it lacks while the block reads ready at
  // that revision. Only a fault in R causes that. The block is then evaluated
  // again where something needs it.
  const lacking = action((missing: readonly BlockRevision[]) => {
    const lost = new Set<BlockId>();
    for (const { block, revision: at } of missing) {
      if (revisions.value.get(block) === at && statuses.value.get(block)?.status === "ready") {
        lost.add(block);
      }
    }
    if (lost.size > 0) revisions.value = bump(graph.value, revisions.value, lost, revision);
  });

  // One-time evaluations

  // Takes the one-time evaluations after a change, and settles the promises of
  // those that it settled.
  function update(next: evaluations.Next): void {
    oneTimes.value = next.oneTimes;
    for (const [id, result] of next.settled) {
      const settle = settles.get(id);
      settles.delete(id);
      settle?.(result);
    }
  }

  // Settles one one-time evaluation, such as one whose signal aborted.
  // Stopping one with parameters drops the evaluations of its blocks that are
  // not sent yet. R runs those already sent, since the scheduler never cancels
  // one with parameters.
  const end = action((id: number, result: Settled) => {
    if (!oneTimes.value.has(id)) return;
    const rest = new Map(oneTimes.value);
    rest.delete(id);
    update({ oneTimes: rest, settled: [[id, result]] });
  });

  // Sending

  function recipeOf(id: BlockId): Recipe | undefined {
    const block = board.value === undefined ? undefined : own(board.value.blocks, id);
    const recipe = inputs.value.get(id)?.recipe;
    if (block === undefined || recipe === undefined) return undefined;
    return {
      registry_id: block.registry_id,
      state: { ...block.state },
      inputs: recipe.map((input) =>
        input.block === undefined
          ? { input: input.input }
          : { input: input.input, block: input.block, revision: revisions.value.get(input.block) ?? 0 },
      ),
    };
  }

  function send(item: Plan["send"][number]): void {
    const recipe = recipeOf(item.block);
    const at = revisions.value.get(item.block);
    if (recipe === undefined || at === undefined) return;

    // The one-time evaluation with parameters that the block is evaluated for,
    // if any.
    let oneTime: { id: number; priority: number; params: Record<string, Json> } | undefined;
    if (item.oneTime !== undefined) {
      const found = oneTimes.value.get(item.oneTime);
      if (found === undefined || !("params" in found)) return;
      oneTime = { id: item.oneTime, priority: found.priority, params: found.params };
    }

    const id = ++ids;
    sent.value = new Map(sent.value).set(id, {
      id,
      at: Date.now(),
      block: item.block,
      revision: at,
      oneTime: oneTime === undefined ? undefined : { id: oneTime.id, priority: oneTime.priority },
      background: item.background === true,
      cancelled: false,
      orphaned: false,
    });
    if (oneTime !== undefined) {
      oneTimes.value = evaluations.sent(oneTimes.value, oneTime.id, item.block, { id, recipe });
    }

    const controller = new AbortController();
    controllers.set(id, controller);
    client
      .request(
        {
          kind: "evaluate",
          block: item.block,
          revision: at,
          recipe,
          ...(oneTime === undefined ? {} : { params: oneTime.params }),
        },
        { signal: controller.signal },
      )
      .then(
        ({ reply }) => {
          answered(id, reply);
        },
        (error: unknown) => {
          unanswered(id, error);
        },
      );
  }

  function cancel(id: number): void {
    const evaluation = sent.value.get(id);
    if (evaluation === undefined) return;
    sent.value = new Map(sent.value).set(id, { ...evaluation, cancelled: true });
    controllers.get(id)?.abort();
  }

  // The evaluation of the id, now that R answered it or it got no answer. It
  // leaves what is sent.
  function done(id: number): Sent | undefined {
    const evaluation = sent.value.get(id);
    if (evaluation === undefined) return undefined;
    const rest = new Map(sent.value);
    rest.delete(id);
    sent.value = rest;
    controllers.delete(id);
    return evaluation;
  }

  // A reply's outcome becomes the block's where it is for the block's current
  // revision, and the block reads anything but waiting. A reply of `cancelled`
  // or `missing` has no outcome. A reply to an evaluation for a one-time
  // evaluation with parameters goes to that one-time evaluation too.
  const answered = action((id: number, reply: Reply) => {
    const evaluation = done(id);
    if (evaluation === undefined || evaluation.orphaned) return;
    if (reply.status === "missing") lacking(reply.missing);

    const outcome = outcomeOf(reply, evaluation.revision);
    if (
      outcome !== undefined &&
      evaluation.revision === revisions.value.get(evaluation.block) &&
      statuses.value.get(evaluation.block)?.status !== "waiting"
    ) {
      outcomes.value = new Map(outcomes.value).set(evaluation.block, outcome);
    }
    if (evaluation.oneTime !== undefined) {
      update(
        evaluations.answered(
          oneTimes.value,
          evaluation.oneTime.id,
          evaluation.block,
          reply,
          outcome,
          evaluation.revision,
        ),
      );
    }
  });

  // An evaluation that got no answer, since the transport could not send it or
  // the session ended first. The one-time evaluation with parameters that it
  // was for rejects with why. Otherwise, where the transport could not send
  // it, the block fails with why, rather than being sent again and again.
  const unanswered = action((id: number, error: unknown) => {
    const evaluation = done(id);
    if (evaluation === undefined || evaluation.orphaned) return;
    if (evaluation.oneTime !== undefined) {
      end(evaluation.oneTime.id, { error });
    } else if (
      !(error instanceof SessionEndedError) &&
      revisions.value.get(evaluation.block) === evaluation.revision
    ) {
      const outcome = unsent(evaluation.revision, messageOf(error));
      outcomes.value = new Map(outcomes.value).set(evaluation.block, outcome);
    }
  });

  // The pass

  // A pass works out what to send once the task in which something changed
  // has ended. By then every subscriber of the store has heard of the change.
  // A front-end that changes its demand from its own subscription is then
  // heard first.
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

  function pass(): void {
    passTimer = undefined;
    batch(() => {
      if (stopped === undefined && board.value !== undefined) {
        const plan = planOf({
          graph: graph.value,
          revisions: revisions.value,
          statuses: statuses.value,
          needs: needs.value,
          sent: sent.value.values(),
          asked: asked.value.values(),
          pending: evaluations.pendingOf(oneTimes.value),
          queue,
          interrupt: client.capabilities().has("interrupt"),
          holding: holding.value,
          now: Date.now(),
          idleSince,
          delay,
        });
        idleSince = plan.idleSince;
        for (const id of plan.cancel) cancel(id);
        for (const { oneTime, error } of plan.reject) end(oneTime, { error });
        for (const item of plan.send) send(item);
        wake(plan.wakeAt);
      }
      // What the pass did itself needs no pass of its own.
      planned = state.value;
    });
  }

  function resendable(request: Asking): boolean {
    if (request.kind === "output" || request.output !== undefined) return true;
    if (request.registry_id === undefined) return false;
    const entry = own(store.blockRegistry(), request.registry_id);
    return entry === undefined || own(entry.requests, request.name)?.action !== true;
  }

  // Demands

  // Keeps a demand under its id. Its blocks are a set, or what its function
  // returns for the board as it is. The function runs again only for a board
  // it has not run for. One that throws demands nothing until it returns.
  function demanding(
    id: number,
    owner: string,
    blocks: Iterable<BlockId> | ((board: Board) => Iterable<BlockId>),
    priority: number,
  ): void {
    let demanded: ReadonlySignal<ReadonlySet<BlockId>>;
    if (typeof blocks === "function") {
      demanded = computed(() => {
        const of = board.value;
        if (of === undefined) return new Set<BlockId>();
        return untracked(() => {
          try {
            return new Set(blocks(of));
          } catch (error) {
            report(
              new Error(
                `The demand of ${owner} threw, so it demands nothing until it returns: ${messageOf(error)}`,
                { cause: error },
              ),
            );
            return new Set<BlockId>();
          }
        });
      });
    } else {
      demanded = signal<ReadonlySet<BlockId>>(new Set(blocks));
    }
    demands.value = new Map(demands.value).set(id, { owner, priority, blocks: demanded });
  }

  // Start

  if (options.gateVisibility === false) {
    demanding(++ids, "blockr.core", (all) => Object.keys(all.blocks), 0);
  }

  if (store.hasBoard()) opened(store.board());

  // The statuses that the subscribers last heard of.
  let heard: ReadonlyMap<BlockId, BlockStatus> = new Map();

  // Whenever what the scheduler knows changes, the one-time evaluations
  // without parameters whose blocks are current settle, and the subscribers
  // hear which statuses changed. A pass is due, unless what the scheduler
  // knows is what the last pass left.
  effect(() => {
    const now = statuses.value;
    const settled = evaluations.settle(oneTimes.value, now);
    if (settled.settled.length > 0) {
      untracked(() => {
        update(settled);
      });
    }

    const changed = changedSince(heard, now);
    heard = now;
    if (changed.length > 0) {
      untracked(() => {
        for (const fn of [...subscribers]) {
          try {
            fn(changed);
          } catch (error) {
            report(error);
          }
        }
      });
    }

    if (state.value !== planned) schedule();
  });

  store.subscribe((change, next) => {
    if ("opened" in change || board.peek() === undefined) opened(next);
    else applied(change.delta, next);
  });

  return {
    demand: action((owner, blocks, { priority } = {}) => {
      const id = ++ids;
      demanding(id, owner, blocks, checked(priority));
      return {
        set: action((next, { priority: given } = {}) => {
          const demand = demands.value.get(id);
          if (demand === undefined) {
            throw new Error(`The demand of ${owner} was released, so it can no longer be set.`);
          }
          demanding(id, owner, next, given === undefined ? demand.priority : checked(given));
        }),
        release: action(() => {
          if (!demands.value.has(id)) return;
          const rest = new Map(demands.value);
          rest.delete(id);
          demands.value = rest;
        }),
      };
    }),

    evaluate: action((blocks, params, { priority, signal: abort } = {}) => {
      const at = priority ?? 0;
      const invalid = priorityError(at);
      if (invalid !== undefined) return Promise.reject(invalid);
      if (abort?.aborted === true) return Promise.reject(asError(abort.reason));

      return new Promise<Record<BlockId, BlockStatus>>((resolve, reject) => {
        const id = ++ids;
        const onAbort = () => {
          end(id, { error: abort?.reason });
        };
        abort?.addEventListener("abort", onAbort, { once: true });
        settles.set(id, (result) => {
          abort?.removeEventListener("abort", onAbort);
          if ("error" in result) reject(asError(result.error));
          else resolve(result.statuses);
        });

        const oneTime = evaluations.oneTimeOf([...new Set(blocks)], params, at, statuses.value);
        if (oneTime instanceof Error) {
          update({ oneTimes: oneTimes.value, settled: [[id, { error: oneTime }]] });
        } else {
          oneTimes.value = new Map(oneTimes.value).set(id, oneTime);
        }
      });
    }),

    invalidate: action((id) => {
      if (!revisions.value.has(id)) return;
      revisions.value = bump(graph.value, revisions.value, new Set([id]), revision);
    }),

    status: (id) => statuses.peek().get(id),

    subscribe(fn) {
      const wrapped = (changed: readonly BlockId[]) => {
        fn(changed);
      };
      subscribers.add(wrapped);
      return () => {
        subscribers.delete(wrapped);
      };
    },

    async request(request, requestOptions = {}) {
      for (;;) {
        const id = ++ids;
        asked.value = new Map(asked.peek()).set(id, { id, at: Date.now() });
        try {
          const result = await client.request(request, requestOptions);
          if (result.reply.status === "missing") lacking(result.reply.missing);
          return result;
        } catch (error) {
          if (error instanceof SessionEndedError && resendable(request)) continue;
          throw error;
        } finally {
          const rest = new Map(asked.peek());
          rest.delete(id);
          asked.value = rest;
        }
      }
    },

    start: action(() => {
      holding.value = false;
    }),

    hold: action(() => {
      holding.value = true;
    }),

    session: action(() => {
      revisions.value = bump(graph.value, revisions.value, new Set(revisions.value.keys()), revision);
    }),
  };
}

// The blocks that R is evaluating: those of the evaluations sent and not yet
// answered, except those sent before another board opened.
function running(sent: ReadonlyMap<number, Sent>): Set<BlockId> {
  const blocks = new Set<BlockId>();
  for (const evaluation of sent.values()) {
    if (!evaluation.orphaned) blocks.add(evaluation.block);
  }
  return blocks;
}

// The blocks whose status is another object than before, in the order of the
// statuses now, and then those that left.
function changedSince(
  before: ReadonlyMap<BlockId, BlockStatus>,
  now: ReadonlyMap<BlockId, BlockStatus>,
): BlockId[] {
  const changed: BlockId[] = [];
  for (const [id, status] of now) {
    if (before.get(id) !== status) changed.push(id);
  }
  for (const id of before.keys()) {
    if (!now.has(id)) changed.push(id);
  }
  return changed;
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
