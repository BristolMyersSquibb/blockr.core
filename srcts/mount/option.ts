// Mounting a board option's UI, or the renderer that the app names for the
// board option in the blockr option `renderers`, which R sends with its reply
// to `hello`, through an OptionHost made for the mount, as the contract has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract):
// 1. Core loads the renderer's module, where the app names one, or else takes
//    the UI module of the board option's definition.
// 2. It adds the board option to the board, with its definition's default
//    where the board lacks it, and adds it again where a delta removes it
//    while the UI is mounted. The UI reads its own board option's value alone,
//    so core adds no other board option for it.
// 3. It calls the module's render() with the element, the host and a signal,
//    which it aborts once the mount ends. Where the default did not go onto
//    the board, as where it failed, core leaves the element empty instead,
//    and ends the UI where the default does not go onto the board again once a
//    delta removed the value. The notification that concerns the board option
//    says why, once for the board.
// The host gives the value as the board holds it, and announces each change of
// it. A submit runs core's command that sets a board option, and a request goes
// to the board option's requests in R, whose answers core never keeps. Once
// another board opens, core ends the mount.
// Where the renderer fails in render(), the error names the renderer's module
// and its package. Where the UI of the board option's definition fails, the
// error names the board option, since only one package can register its id.
// Either way, core leaves the element empty, so that the frontend alone says
// where the failure shows, and raises a notification that concerns the board
// option, which holds until the next board opens. Its source names the module
// that draws: the renderer, by its id and with its package, or else the board
// option's own module, with the board option's package.
// What the module raises through its host's notify() names that module as
// its source, and holds until the mount ends at the latest. Each condition of
// R's reply to a request, ok or error, raises a notification that concerns
// the board option, which holds until R next replies to a request of the same
// name. Its source names that module too, which made the request.

import type { Commands } from "../commands/index.ts";
import type { Renderer } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { OptionId } from "../types/board.ts";
import type { SourceModule } from "../types/core-api.ts";
import type { HostRequestOpts, OptionHost } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import { createLog, log } from "../log/index.ts";
import { asError, checkRaised, coreNotification, errorMessage, isObject, own, packageFromId, sourceModule } from "../util/index.ts";
import type { Notify, Raise, Replied } from "../util/index.ts";
import { ModuleError } from "./load.ts";
import type { Loader } from "./load.ts";
import { RequestError } from "./requests.ts";
import { announceTo, attempt, mountableModule, onAbort, REPLACED } from "./shared.ts";
import type { Mountable } from "./shared.ts";

export interface OptionDeps {
  readonly store: Pick<Store, "board" | "options" | "subscribe">;
  readonly commands: Pick<Commands, "run">;
  readonly scheduler: Pick<Scheduler, "request">;
  readonly loader: Loader;
  // The renderers that the app names for board options, by the renderer's id.
  readonly renderers: () => Readonly<Record<string, Renderer>>;
  // Aborts as another board opens, which ends the mount.
  readonly replaced: AbortSignal;
  // Keeps the board options on the board until stop() runs, resolves `added`
  // with why any did not go onto the board, and calls `failed` where one that
  // a delta removed does not go onto the board again (./block.ts).
  readonly keepOptions: (
    ids: readonly OptionId[],
    by: string,
    failed: (failures: readonly string[]) => void,
  ) => { readonly added: Promise<readonly string[]>; readonly stop: () => void };
  // Raises a notification of the board (../core/notifications.ts).
  readonly raise: Raise;
  // Holds a notification for each condition of R's latest reply to each of
  // the board option's requests.
  readonly replied: Replied;
  // Raises the notification that the module gives through its host.
  readonly notify: Notify;
  // The package that lists a board option, from R's reply to `hello`.
  readonly optionPackage: (option: OptionId) => string | undefined;
}

interface OptionArgs {
  el: HTMLElement;
  host: OptionHost;
  signal: AbortSignal;
}

// Mounts the UI of the board option in the element until the returned cleanup
// runs. It throws where no package registers such a board option.
export function mountOption(deps: OptionDeps, el: HTMLElement, option: OptionId): () => void {
  const { store, loader } = deps;
  if (deps.replaced.aborted) throw new Error(REPLACED);
  const def = own(store.options.registered(), option);
  if (def === undefined) {
    throw new Error(`No package registers the board option "${option}" whose UI to mount.`);
  }
  const chosen = Object.entries(deps.renderers()).find(([, renderer]) => renderer.option === option);
  const [id, renderer] = chosen ?? [];
  // The module that draws: the renderer that the app names, with its package,
  // or else the board option's own module.
  const module =
    id === undefined ? sourceModule({ option }, deps.optionPackage(option)) : sourceModule({ option }, packageFromId(id), id);

  const controller = new AbortController();
  const ended = (): boolean => controller.signal.aborted;
  const listeners = new Set<() => void>();
  let value: Json | undefined;
  let seen = store.board().options;
  let cleanup: (() => void) | undefined;
  // Stops keeping the board option on the board, once the module has loaded.
  let stopKeeping: (() => void) | undefined;
  // What withdraws each notification that the module raised and that holds.
  const notified = new Set<() => void>();

  function bound(): boolean {
    return !ended() && !deps.replaced.aborted;
  }

  const unboundError = (): Error =>
    new Error(`Another board opened, so the host of the board option "${option}" no longer changes it.`);

  // The value as the board holds it.
  function current(): Json | undefined {
    return own(store.board().options, option);
  }

  const host: OptionHost = {
    option,
    value: () => value as Json,
    on(event, fn) {
      // A module in plain JS may name another event, which the types refuse.
      const heard: string = event;
      if (heard !== "value") {
        throw new Error(`The host of a board option announces "value", not ${JSON.stringify(event)}.`);
      }
      const wrapped = (): void => {
        fn();
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    },
    submit(next) {
      if (!bound()) return Promise.reject(unboundError());
      return deps.commands.run("blockr.core-set_option", { id: option, value: next });
    },
    request<T = Json>(
      name: string,
      args: Readonly<Record<string, Json>> = {},
      opts: HostRequestOpts = {},
    ): Promise<T | Blob> {
      if (typeof name !== "string" || name === "") {
        return Promise.reject(new TypeError("A request of a board option is named by a non-empty string."));
      }
      if (!isObject(args)) return Promise.reject(new TypeError(`The args of the request "${name}" are an object.`));
      if (!bound()) return Promise.reject(unboundError());
      return optionRequest(deps, module, option, name, args, opts) as Promise<T | Blob>;
    },
    notify(notification) {
      const given = checkRaised(notification);
      if (ended()) return () => undefined;
      const withdraw = deps.notify(module, given);
      const mine = (): void => {
        notified.delete(mine);
        withdraw();
      };
      notified.add(mine);
      return mine;
    },
    log: createLog(module.package ?? null),
  };

  const offStore = store.subscribe((_change, board) => {
    if (board.options === seen) return;
    seen = board.options;
    const next = current();
    // While the board lacks the value, as after a delta removed it and before
    // core adds the default again, the host keeps the value it gave.
    if (next === undefined || next === value) return;
    value = next;
    announceTo(listeners);
  });

  const offEnd = onAbort(deps.replaced, end);

  function end(): void {
    if (ended()) return;
    controller.abort();
    offEnd();
    offStore();
    stopKeeping?.();
    listeners.clear();
    for (const withdraw of [...notified]) withdraw();
    if (cleanup !== undefined) attempt(cleanup);
    el.replaceChildren();
  }

  void (async () => {
    let mod: Mountable<OptionArgs>;
    try {
      if (renderer === undefined || id === undefined) {
        mod = def.ui;
      } else {
        const source = renderer.css === undefined ? { module: renderer.module } : { module: renderer.module, css: renderer.css };
        mod = mountableModule<OptionArgs>(await loader.load(source, packageFromId(id)), renderer.module, packageFromId(id));
      }
    } catch (error) {
      fail(error);
      return;
    }
    if (ended()) return;
    // Core has reported why the board holds no value, and raised it.
    const kept = deps.keepOptions([option], `the UI of the board option "${option}"`, end);
    stopKeeping = kept.stop;
    const failures = await kept.added;
    if (ended()) return;
    value = current();
    if (value === undefined && failures.length > 0) {
      kept.stop();
      return;
    }

    try {
      const rendered = await mod.render({ el, host, signal: controller.signal });
      if (typeof rendered !== "function") return;
      if (ended()) attempt(rendered);
      else cleanup = rendered;
    } catch (error) {
      const why = `failed in render(): ${errorMessage(error)}`;
      fail(
        renderer === undefined || id === undefined
          ? new Error(`The UI of the board option "${option}" ${why}`, { cause: error })
          : new ModuleError(renderer.module, packageFromId(id), why, { cause: error }),
      );
    }
  })();

  // A module that cannot draw, which leaves its element empty.
  function fail(error: unknown): void {
    if (ended()) return;
    log.error(asError(error));
    deps.raise(coreNotification("module_failed", "error", errorMessage(error), { module }));
  }

  return end;
}

// Sends the request `name` of the board option and resolves with its answer
// as R writes it, or with the File of a download. Core never keeps the answer
// of a board option's request.
async function optionRequest(
  deps: Pick<OptionDeps, "scheduler" | "replied">,
  module: SourceModule,
  option: OptionId,
  name: string,
  args: Readonly<Record<string, Json>>,
  opts: HostRequestOpts,
): Promise<Json | Blob> {
  const { reply, body } = await deps.scheduler.request(
    { kind: "request", type: "option", option, name, args: { ...args } },
    {
      ...(opts.file === undefined ? {} : { body: opts.file }),
      ...(opts.signal === undefined ? {} : { signal: opts.signal }),
      ...(opts.progress === undefined ? {} : { progress: opts.progress.bind(opts) }),
    },
  );
  const conditions = "conditions" in reply ? reply.conditions : [];
  switch (reply.status) {
    case "ok":
      deps.replied(module, { type: "option", name }, conditions);
      if (body !== null) return body;
      return "value" in reply && reply.value !== undefined ? reply.value : null;
    case "cancelled":
      throw new RequestError(`R cancelled the request "${name}" of the board option "${option}".`, { status: "cancelled" });
    default:
      deps.replied(module, { type: "option", name }, conditions);
      throw new RequestError(`R replied ${reply.status} to the request "${name}" of the board option "${option}".`, {
        status: reply.status,
        conditions,
      });
  }
}
