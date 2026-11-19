// The hooks through which a module written in React follows its host, as
// React's useSyncExternalStore follows a store: the host's getters give a
// snapshot, which stays the same object until the host announces a change.
// A component that edits a value of its host binds it,
// so that it meets the contract of a block's UI as every bound control does,
// and follows the requests it shows the answers of.
// A component that holds CoreApi, as the UI of a plugin does, follows the
// board, a block's status and the notifications in the same way.
// Core's own frontend and cards follow them through these hooks too.

import { useCallback, useLayoutEffect, useMemo, useRef, useState, useSyncExternalStore } from "react";
import { bind, follow } from "../binding/index.ts";
import type { Binding } from "../binding/index.ts";
import type { Condition } from "../protocol/index.ts";
import type { BlockStatus, Scheduler } from "../scheduler/index.ts";
import type { BlockId, Board, OptionId } from "../types/board.ts";
import type { CoreApi, CoreFailure, Notification } from "../types/core-api.ts";
import type { BlockHost, OptionHost, OutputHost, State } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import { errorMessage, own } from "../util/index.ts";

// What an output host holds at one moment.
export interface OutputSnapshot {
  readonly output: Json;
  readonly body: Blob | null;
  readonly params: Json;
  readonly options: Readonly<Record<OptionId, Json>>;
  readonly loading: boolean;
  readonly error: Condition | CoreFailure | null;
}

// The last snapshot of each host, which a component reads again until one of
// its parts changes.
const snapshots = new WeakMap<OutputHost, OutputSnapshot>();

// What the output host holds, for a renderer written in React, which draws
// again as the host announces "output".
export function useOutput(host: OutputHost): OutputSnapshot {
  const subscribe = useCallback((notify: () => void) => host.on("output", notify), [host]);
  const read = useCallback(() => outputSnapshot(host), [host]);
  return useSyncExternalStore(subscribe, read);
}

function outputSnapshot(host: OutputHost): OutputSnapshot {
  const next: OutputSnapshot = {
    output: host.output(),
    body: host.body(),
    params: host.params(),
    options: host.options(),
    loading: host.loading(),
    error: host.error(),
  };
  const last = snapshots.get(host);
  if (last !== undefined && (Object.keys(next) as (keyof OutputSnapshot)[]).every((key) => last[key] === next[key])) {
    return last;
  }
  snapshots.set(host, next);
  return next;
}

// A value that a component shows and edits:
// the value as the binding last showed it,
// why the host refused the last submit, if it did,
// which the component shows, as core's inputs do below themselves,
// and what reads and submits the value, as a Binding does.
export interface Bound {
  readonly value: Json | undefined;
  readonly error: string | null;
  get(): Json | undefined;
  commit(value: Json): Promise<boolean>;
  unset(): Promise<boolean>;
  type(value: Json | undefined): void;
  flush(): void;
}

// Binds a block's state arg by its name, or a board option's value,
// for as long as the component is mounted.
// The binding takes the notification of each submit that the host refuses,
// so that core raises none, since the component shows why as `error`.
export function useValue<S extends State>(host: BlockHost<S>, key: Extract<keyof S, string>): Bound;
export function useValue<V extends Json>(host: OptionHost<V>): Bound;
export function useValue(host: BlockHost | OptionHost, key?: string): Bound {
  const name = key ?? ("option" in host ? host.option : "");
  const [value, setValue] = useState<Json | undefined>(() => held(host, name));
  const [error, setError] = useState<string | null>(null);
  const binding = useRef<Binding | null>(null);

  // The binding starts before the browser paints, so that no event of the
  // user comes before it.
  useLayoutEffect(() => {
    const bound = bind(host as BlockHost, name, setValue, {
      settled: (refused) => {
        setError(refused === null ? null : errorMessage(refused));
      },
      notifier: () => undefined,
    });
    binding.current = bound;
    return () => {
      binding.current = null;
      bound.release();
    };
  }, [host, name]);

  return useMemo(
    () => ({
      value,
      error,
      get: () => binding.current?.get() ?? held(host, name),
      commit: (next: Json) => binding.current?.commit(next) ?? Promise.resolve(false),
      unset: () => binding.current?.unset() ?? Promise.resolve(false),
      type: (next: Json | undefined) => {
        binding.current?.type(next);
      },
      flush: () => {
        binding.current?.flush();
      },
    }),
    [value, error, host, name],
  );
}

// The last answer of a request, which stays while the request is asked
// again, why the latest request failed, if it did,
// and whether the request is being asked.
// The answer is for older data while the request is being asked,
// and once an ask failed,
// such as for the block's inputs before their revisions changed.
export interface Answered {
  readonly answer: Json | Blob | undefined;
  readonly error: string | null;
  // True from each ask until its answer or its failure,
  // as an output host's loading() is while core asks R for a newer output.
  readonly loading: boolean;
}

export interface RequestOptions {
  // The parameters the request passes, and what else it reads,
  // such as a state arg that its function names.
  readonly args?: Readonly<Record<string, Json>>;
  readonly reads?: Json;
}

// Follows a request of the block's class or of the board option,
// asked again whenever its parameters, what it reads
// or the revisions of the block's linked data inputs change.
export function useRequest<S extends State>(host: BlockHost<S>, name: string, options?: RequestOptions): Answered;
export function useRequest<V extends Json>(host: OptionHost<V>, name: string, options?: RequestOptions): Answered;
export function useRequest(host: BlockHost | OptionHost, name: string, options: RequestOptions = {}): Answered {
  // The request is asked as the component mounts, so it is loading from the
  // start.
  const [answered, setAnswered] = useState<Answered>({ answer: undefined, error: null, loading: true });
  const reads = JSON.stringify([options.args ?? {}, options.reads ?? null]);

  // The request is asked again before the browser paints,
  // so that no frame shows the answer for the last parameters
  // with `loading` false.
  useLayoutEffect(() => {
    const [args, extra] = JSON.parse(reads) as [Readonly<Record<string, Json>>, Json];
    const followed = follow(host, name, {
      args: () => args,
      reads: () => extra,
      loading: () => {
        setAnswered((last) => (last.loading ? last : { ...last, loading: true }));
      },
      answer: (answer) => {
        setAnswered({ answer, error: null, loading: false });
      },
      failed: (failure) => {
        setAnswered((last) => ({ answer: last.answer, error: errorMessage(failure), loading: false }));
      },
    });
    return () => {
      followed.release();
    };
  }, [host, name, reads]);

  return answered;
}

function held(host: BlockHost | OptionHost, key: string): Json | undefined {
  return "option" in host ? host.value() : own(host.state(), key);
}

// The board, which the store keeps the same object until it announces a
// change.
export function useBoard(core: Pick<CoreApi, "store">): Board {
  const subscribe = useCallback(
    (notify: () => void) =>
      core.store.subscribe(() => {
        notify();
      }),
    [core],
  );
  return useSyncExternalStore(subscribe, () => core.store.board());
}

// The block's status, which the scheduler keeps the same object until it
// announces that it changed.
export function useStatus(
  core: { readonly scheduler: Pick<Scheduler, "status" | "subscribe"> },
  block: BlockId,
): BlockStatus | undefined {
  const subscribe = useCallback(
    (notify: () => void) =>
      core.scheduler.subscribe((changed) => {
        if (changed.includes(block)) notify();
      }),
    [core, block],
  );
  return useSyncExternalStore(subscribe, () => core.scheduler.status(block));
}

// The notifications about the block, or every notification of the board given
// null, in the order they came.
// CoreApi keeps the same array of them until one comes or goes, so the
// component draws them again only then.
export function useNotified(
  core: { readonly notifications: Pick<CoreApi["notifications"], "get" | "subscribe"> },
  block: BlockId | null,
): readonly Notification[] {
  const subscribe = useCallback(
    (notify: () => void) =>
      core.notifications.subscribe(() => {
        notify();
      }),
    [core],
  );
  const all = useSyncExternalStore(subscribe, () => core.notifications.get());
  return useMemo(
    () =>
      block === null
        ? all
        : all.filter((notification) => notification.about !== undefined && "block" in notification.about && notification.about.block === block),
    [all, block],
  );
}
