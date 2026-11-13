// The notifications of the open board, which CoreApi's `notifications` holds,
// as Notifications has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#notifications).
// Core makes one holder for each board that opens, with the board's store and
// before it imports the modules that R's reply names, so that a failure raised
// before the frontend draws still reaches it. It holds, in the order they came:
// - One notification for each condition of each block's status, which it
//   derives from the scheduler, ahead of any frontend, rather than copies. Its
//   id comes from the block, the severity, the source and the message, so that
//   the same condition in the block's next reply keeps its id, and two that
//   share all four are one. Its source is R, or core for the two conditions
//   that the scheduler writes itself.
// - One for each condition of R's latest reply to each request that a module
//   or a plugin made, in place of those of the request's reply before, so that
//   a request that fails again keeps its notifications, and one that succeeds
//   withdraws them.
// - Those that core raises, each with an id from what it holds, so that a
//   failure raised twice, as by two mounts of one module, is one notification,
//   which holds until each raise of it is withdrawn.
// - Those that the code of packages raises, a plugin through notify() and a
//   module that core mounts through its host, each with an id of its own.
// It withdraws every notification that concerns a block, a link or a stack as
// that leaves the board, and those of the code of a block as the block leaves,
// and every one as the board ends, as the next board opens, which it
// announces.
// It holds none that is raised about a block, a link or a stack that the board
// does not hold, since the work that raises one can outlive what it concerns,
// and it reports one that a package's code raises about a part that the board
// has never held, or that a plugin that the app does not run raises, since only
// a mistake in the code names such a part or plugin.

import { announceTo, onAbort } from "../mount/index.ts";
import type { Condition } from "../protocol/index.ts";
import { scheduledCode } from "../scheduler/index.ts";
import type { BlockStatus, Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { BlockId, PluginClassId } from "../types/board.ts";
import type { CoreApi, EvaluatePhase, Notification, NotificationSource, Owner, Part } from "../types/core-api.ts";
import { checkRaised, jsonEqual, packageFromId, report } from "../util/index.ts";
import type { Notify, Raise, Replied } from "../util/index.ts";

export interface Holder {
  // CoreApi's `notifications`.
  readonly api: CoreApi["notifications"];
  // Raises one of core's own notifications, and returns what withdraws it.
  readonly raise: Raise;
  // Holds the notifications of the conditions of R's latest reply to a
  // request, in place of those of the request's reply before.
  readonly replied: Replied;
  // Raises a notification that the code of a package gives, from its owner,
  // and returns what withdraws it.
  readonly notify: Notify;
  // Holds a notification for each condition of each block's status from now
  // on, as the scheduler announces each change of one.
  follow(scheduler: Pick<Scheduler, "status" | "subscribe">): void;
}

export interface HolderOptions {
  // Ends the holder as it aborts, as the next board opens.
  readonly signal?: AbortSignal;
  // The app's plugins, by the ids of their classes, as R's reply to `hello`
  // names them. A plugin that is none of them raises nothing.
  readonly plugins?: Readonly<Record<PluginClassId, unknown>>;
}

// The phases of a block's evaluation, which R's condition there names.
const EVALUATE_PHASES: Readonly<Record<EvaluatePhase, true>> = {
  expr: true,
  inputs: true,
  block_eval: true,
  result_summary: true,
};

const NONE: readonly Notification[] = Object.freeze([]);

const nothing = (): void => undefined;

export function createNotifications(
  store: Pick<Store, "board" | "hasBoard" | "options" | "subscribe">,
  options: HolderOptions = {},
): Holder {
  const { signal } = options;
  const plugins = options.plugins ?? {};
  // Every notification that holds, by id, in the order they came.
  const held = new Map<string, Notification>();
  // The raises that hold each of core's notifications, by its id.
  const raised = new Map<string, Set<object>>();
  // The ids of what holds in place of what held before, by what they replace
  // each other for, a block's status or a request, with the part that it
  // concerns.
  const replacing = new Map<string, { readonly part: Part; readonly ids: readonly string[] }>();
  // The blocks, links and stacks that have left the board, by key. A
  // notification raised about one of them holds nothing, and so does one that
  // a package's code raises about a part that the board has never held, which
  // core reports as well.
  const left = new Set<string>();
  const listeners = new Set<(all: readonly Notification[]) => void>();
  let all = NONE;
  let count = 0;
  let ended = false;
  const stops: (() => void)[] = [];

  function publish(): void {
    all = Object.freeze([...held.values()]);
    announceTo(listeners, all);
  }

  // Whether the board holds the block, the link or the stack. A board option
  // and a plugin stay for the opening, so every one counts.
  function present(part: Part): boolean {
    if (!("block" in part || "link" in part || "stack" in part)) return true;
    if (!store.hasBoard()) return false;
    const board = store.board();
    if ("block" in part) return Object.hasOwn(board.blocks, part.block);
    if ("link" in part) return Object.hasOwn(board.links, part.link);
    return Object.hasOwn(board.stacks, part.stack);
  }

  // Whether the part is one that a package's code may name: one that the
  // board holds, a board option that a loaded package defines or that the
  // board holds a value of, or one of the app's plugins.
  function known(part: Part): boolean {
    if ("option" in part) {
      return (
        Object.hasOwn(store.options.registered(), part.option) ||
        (store.hasBoard() && Object.hasOwn(store.board().options, part.option))
      );
    }
    if ("plugin" in part) return Object.hasOwn(plugins, part.plugin);
    return present(part);
  }

  const offStore = store.subscribe((change) => {
    if ("opened" in change) return;
    const gone = new Set([
      ...(change.delta.blocks?.rm ?? []).map((block) => key({ block })),
      ...(change.delta.links?.rm ?? []).map((link) => key({ link })),
      ...(change.delta.stacks?.rm ?? []).map((stack) => key({ stack })),
    ]);
    if (gone.size === 0) return;
    for (const part of gone) left.add(part);
    const goes = (part: Part | undefined): boolean => part !== undefined && gone.has(key(part));
    let moved = false;
    for (const [id, notification] of held) {
      if (!goes(notification.about) && !(notification.source.kind === "module" && goes(notification.source.owner))) {
        continue;
      }
      held.delete(id);
      raised.delete(id);
      moved = true;
    }
    for (const [what, holding] of replacing) {
      if (goes(holding.part)) replacing.delete(what);
    }
    if (moved) publish();
  });
  stops.push(offStore);

  function end(): void {
    if (ended) return;
    ended = true;
    for (const stop of stops) stop();
    const had = held.size > 0;
    held.clear();
    raised.clear();
    replacing.clear();
    left.clear();
    if (had) publish();
    listeners.clear();
  }
  if (signal?.aborted === true) end();
  else if (signal !== undefined) onAbort(signal, end);

  // Holds `next` for `what` in place of what held for it, and says whether
  // that changed what holds. Those that hold still keep their place.
  function replace(what: string, part: Part, next: ReadonlyMap<string, Notification>): boolean {
    let moved = false;
    for (const id of replacing.get(what)?.ids ?? []) {
      if (!next.has(id) && held.delete(id)) moved = true;
    }
    for (const [id, notification] of next) {
      const had = held.get(id);
      if (had !== undefined && jsonEqual(had, notification)) continue;
      held.set(id, notification);
      moved = true;
    }
    if (next.size === 0) replacing.delete(what);
    else replacing.set(what, { part, ids: [...next.keys()] });
    return moved;
  }

  // Holds the notifications of the conditions of a block's status in place of
  // those it held, and says whether that changed what holds.
  function fromStatus(block: BlockId, status: BlockStatus | undefined): boolean {
    const about = { block };
    const next = new Map<string, Notification>();
    if (status !== undefined && present(about)) {
      for (const condition of status.conditions) {
        const source = statusSource(condition);
        const id = JSON.stringify(["status", block, condition.severity, source, condition.message]);
        if (!next.has(id)) next.set(id, frozen(id, { severity: condition.severity, message: condition.message, source, about }));
      }
    }
    return replace(JSON.stringify(["status", block]), about, next);
  }

  const replied: Replied = (about, request, conditions) => {
    if (ended) return;
    const next = new Map<string, Notification>();
    if (present(about)) {
      for (const condition of conditions) {
        const source: NotificationSource = { kind: "r", class: condition.class, request };
        const id = JSON.stringify(["request", about, condition.severity, source, condition.message]);
        if (!next.has(id)) next.set(id, frozen(id, { severity: condition.severity, message: condition.message, source, about }));
      }
    }
    if (replace(JSON.stringify(["request", about, request]), about, next)) publish();
  };

  const raise: Raise = (given) => {
    if (ended || (given.about !== undefined && !present(given.about))) return nothing;
    const id = JSON.stringify([
      "raised",
      given.severity,
      given.message,
      sourceCopy(given.source),
      given.about === undefined ? null : partCopy(given.about),
    ]);
    const token = {};
    const tokens = raised.get(id) ?? new Set<object>();
    tokens.add(token);
    raised.set(id, tokens);
    if (!held.has(id)) {
      held.set(id, frozen(id, given));
      publish();
    }
    return () => {
      const holding = raised.get(id);
      if (holding?.delete(token) !== true || holding.size > 0) return;
      raised.delete(id);
      if (held.delete(id)) publish();
    };
  };

  const notify: Notify = (owner, pkg, notification) => {
    const given = checkRaised(notification);
    if (ended) return nothing;
    const about = given.about === undefined ? owner : (given.about ?? undefined);
    if (about !== undefined && !known(about)) {
      if (!left.has(key(about))) {
        report(new Error(`Core drops a notification that ${raiser(owner)} raised about ${named(about)}, ${unknown(about)}.`));
      }
      return nothing;
    }
    const id = `module-${String(++count)}`;
    held.set(
      id,
      frozen(id, {
        severity: given.severity,
        message: given.message,
        source: { kind: "module", owner, code: given.code, ...(pkg === null ? {} : { package: pkg }) },
        ...(about === undefined ? {} : { about }),
      }),
    );
    publish();
    let withdrawn = false;
    return () => {
      if (withdrawn) return;
      withdrawn = true;
      if (held.delete(id)) publish();
    };
  };

  return {
    api: {
      notify(plugin, notification) {
        if (typeof plugin !== "string" || plugin === "") {
          throw new TypeError(
            `A plugin's notification names the plugin by the id of its class, a non-empty string, not ${JSON.stringify(plugin)}.`,
          );
        }
        const given = checkRaised(notification);
        if (ended) return nothing;
        if (!Object.hasOwn(plugins, plugin)) {
          report(new Error(`Core drops a notification that the plugin "${plugin}" raised, which the app does not run.`));
          return nothing;
        }
        return notify({ plugin }, packageFromId(plugin), given);
      },
      get: () => all,
      subscribe(fn) {
        const wrapped = (notifications: readonly Notification[]): void => {
          fn(notifications);
        };
        listeners.add(wrapped);
        return () => {
          listeners.delete(wrapped);
        };
      },
    },
    raise,
    replied,
    notify,
    follow(scheduler) {
      if (ended) return;
      const heard = (blocks: readonly BlockId[]): void => {
        let moved = false;
        for (const block of blocks) {
          if (fromStatus(block, scheduler.status(block))) moved = true;
        }
        if (moved) publish();
      };
      stops.push(scheduler.subscribe(heard));
      if (store.hasBoard()) heard(Object.keys(store.board().blocks));
    },
  };
}

// Who raised a condition of a block's status: R, or core for one that the
// scheduler wrote itself.
function statusSource(condition: Condition): NotificationSource {
  const code = scheduledCode(condition);
  if (code !== undefined) return { kind: "core", code };
  const { phase } = condition;
  return {
    kind: "r",
    class: condition.class,
    request: { type: "evaluate", ...(isEvaluatePhase(phase) ? { phase } : {}) },
  };
}

function isEvaluatePhase(phase: string | undefined): phase is EvaluatePhase {
  return phase !== undefined && Object.hasOwn(EVALUATE_PHASES, phase);
}

// The key of a part, by which core tells one that has left the board.
function key(part: Part): string {
  return JSON.stringify(part);
}

// A part, as a message names it.
function named(part: Part): string {
  if ("block" in part) return `the block "${part.block}"`;
  if ("link" in part) return `the link "${part.link}"`;
  if ("stack" in part) return `the stack "${part.stack}"`;
  if ("option" in part) return `the board option "${part.option}"`;
  return `the plugin "${part.plugin}"`;
}

// Who raised a notification of a package's code, as a message names them.
function raiser(owner: Owner): string {
  return "plugin" in owner ? named(owner) : `the code of ${named(owner)}`;
}

// Why a part is unknown, as a message gives it.
function unknown(part: Part): string {
  if ("option" in part) return "which no loaded package defines";
  if ("plugin" in part) return "which the app does not run";
  return "which the board has never held";
}

// The notification with its id, made afresh of what it was given and frozen
// through and through, so that nothing that gave it changes it.
function frozen(id: string, given: Omit<Notification, "id">): Notification {
  return deepFrozen({
    id,
    severity: given.severity,
    message: given.message,
    source: sourceCopy(given.source),
    ...(given.about === undefined ? {} : { about: partCopy(given.about) }),
  });
}

function sourceCopy(source: NotificationSource): NotificationSource {
  switch (source.kind) {
    case "r":
      return { kind: "r", class: [...source.class], request: { ...source.request } };
    case "core":
      return { kind: "core", code: source.code, ...(source.package === undefined ? {} : { package: source.package }) };
    case "module":
      return {
        kind: "module",
        owner: ownerCopy(source.owner),
        code: source.code,
        ...(source.package === undefined ? {} : { package: source.package }),
      };
  }
}

function partCopy(part: Part): Part {
  if ("link" in part) return { link: part.link };
  if ("stack" in part) return { stack: part.stack };
  return ownerCopy(part);
}

function ownerCopy(owner: Owner): Owner {
  if ("block" in owner) return { block: owner.block };
  if ("option" in owner) return { option: owner.option };
  return { plugin: owner.plugin };
}

function deepFrozen<T>(value: T): T {
  if (typeof value === "object" && value !== null) {
    for (const member of Object.values(value)) deepFrozen(member);
    Object.freeze(value);
  }
  return value;
}
