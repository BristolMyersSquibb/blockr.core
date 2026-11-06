// Mounting a board option's UI, or the renderer that the app names for the
// board option in the blockr option `renderers`, which R sends with its reply
// to `hello`, through an OptionHost made for the mount, as the contract has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract):
// 1. Core loads the renderer's module, where the app names one, or else takes
//    the UI module of the board option's definition.
// 2. It adds the board options that the module names to the board, each with
//    its definition's default where the board lacks it.
// 3. It resolves the board option's value, the board's or else its default for
//    this opening, and calls the module's render() with the element, the host
//    and a signal, which it aborts once the mount ends.
// The host gives the value as the board holds it, or else the default, and
// announces each change of it. A submit runs core's command that sets a board
// option, and a request goes to the board option's requests in R, whose
// answers core never keeps. Once another board opens, core ends the mount.

import type { Commands } from "../commands/index.ts";
import type { Renderer } from "../protocol/index.ts";
import type { Scheduler } from "../scheduler/index.ts";
import type { Option, Store } from "../store/index.ts";
import type { OptionId } from "../types/board.ts";
import type { HostRequestOpts, OptionHost } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import { isObject, messageOf, own, packageOf, report } from "../util/index.ts";
import { ModuleError } from "./load.ts";
import type { Loader } from "./load.ts";
import { RequestError } from "./requests.ts";
import { announceTo, attempt, moduleOf, onAbort, REPLACED, showError } from "./shared.ts";
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
  readonly addOptions: (ids: readonly OptionId[], by: string) => Promise<void>;
  // The board option's default, the same for every mount on the board.
  readonly defaultOf: (id: OptionId, def: Option) => Promise<Json>;
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

  const controller = new AbortController();
  const ended = (): boolean => controller.signal.aborted;
  const listeners = new Set<() => void>();
  let value: Json | undefined;
  let fallback: Json | undefined;
  let seen = store.board().options;
  let cleanup: (() => void) | undefined;

  function bound(): boolean {
    return !ended() && !deps.replaced.aborted;
  }

  const unboundError = (): Error =>
    new Error(`Another board opened, so the host of the board option "${option}" no longer changes it.`);

  // The value as the board holds it, or else the default.
  function current(): Json | undefined {
    const held = own(store.board().options, option);
    return held === undefined ? fallback : held;
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
      return optionRequest(deps, option, name, args, opts) as Promise<T | Blob>;
    },
  };

  const offStore = store.subscribe((_change, board) => {
    if (board.options === seen) return;
    seen = board.options;
    const next = current();
    if (next === value) return;
    value = next;
    announceTo(listeners);
  });

  const offEnd = onAbort(deps.replaced, end);

  function end(): void {
    if (ended()) return;
    controller.abort();
    offEnd();
    offStore();
    listeners.clear();
    if (cleanup !== undefined) attempt(cleanup);
    el.replaceChildren();
  }

  void (async () => {
    let mod: Mountable<OptionArgs>;
    const [id, renderer] = chosen ?? [];
    try {
      if (renderer === undefined || id === undefined) {
        mod = def.ui;
      } else {
        const pkg = packageOf(id);
        const source = renderer.css === undefined ? { module: renderer.module } : { module: renderer.module, css: renderer.css };
        mod = moduleOf<OptionArgs>(await loader.load(source, pkg), renderer.module, pkg);
      }
      if (ended()) return;
      await deps.addOptions(mod.options ?? [], `the UI of the board option "${option}"`);
      if (ended()) return;
      fallback = Object.hasOwn(store.board().options, option) ? undefined : await deps.defaultOf(option, def);
    } catch (error) {
      fail(error);
      return;
    }
    if (ended()) return;
    value = current();

    try {
      const rendered = await mod.render({ el, host, signal: controller.signal });
      if (typeof rendered !== "function") return;
      if (ended()) attempt(rendered);
      else cleanup = rendered;
    } catch (error) {
      const module = renderer?.module ?? `the UI of the board option "${option}"`;
      fail(new ModuleError(module, id === undefined ? null : packageOf(id), `failed in render(): ${messageOf(error)}`, { cause: error }));
    }
  })();

  function fail(error: unknown): void {
    if (ended()) return;
    showError(el, messageOf(error));
    report(error);
  }

  return end;
}

// Sends the request `name` of the board option and resolves with its answer
// as R writes it, or with the File of a download. Core never keeps the answer
// of a board option's request.
async function optionRequest(
  deps: Pick<OptionDeps, "scheduler">,
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
  switch (reply.status) {
    case "ok":
      if (body !== null) return body;
      return "value" in reply && reply.value !== undefined ? reply.value : null;
    case "cancelled":
      throw new RequestError(`R cancelled the request "${name}" of the board option "${option}".`, { status: "cancelled" });
    default:
      throw new RequestError(`R replied ${reply.status} to the request "${name}" of the board option "${option}".`, {
        status: reply.status,
        conditions: "conditions" in reply ? reply.conditions : [],
      });
  }
}
