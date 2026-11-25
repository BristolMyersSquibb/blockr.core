// The hooks through which a module written in React follows its host, as
// React's useSyncExternalStore follows a store: the host's getters give a
// snapshot, which stays the same object until the host announces a change.
// A component that edits a value of its host binds it,
// so that it meets the contract of a block's UI as every bound control does,
// and follows the requests it shows the answers of.
// A component that holds CoreApi, as the UI of a plugin does, follows the
// board, or the part of it that the component shows, a block's status and
// the notifications in the same way.
// Core's own frontend and cards follow them through these hooks too.

import { useCallback, useLayoutEffect, useMemo, useRef, useState, useSyncExternalStore } from "react";
import { bind, follow } from "../binding/index.ts";
import type { Binding } from "../binding/index.ts";
import type { Condition } from "../protocol/index.ts";
import type { BlockStatus, Scheduler } from "../scheduler/index.ts";
import type { BlockId, Board } from "../types/board.ts";
import type { BoardObject, CoreApi, CoreFailure, Notification } from "../types/core-api.ts";
import type { BlockHost, OptionHost, OutputHost, State } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import { errorMessage, jsonEqual, own } from "../util/index.ts";

// What an output host holds at one moment. A renderer reads the board options
// of its output family through the host's option(), whose values change only
// with the output.
export interface OutputSnapshot {
  readonly output: Json;
  readonly body: Blob | null;
  readonly params: Json;
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
// Given two functions in place of the name, it binds the value that the
// first reads from the block's state, and submits the change that the
// second builds, as bind() does with them, for an input that sets several
// state args or edits part of one.
// The binding takes the notification of each submit that the host refuses,
// so that core raises none, since the component shows why as `error`.
export function useValue<S extends State>(host: BlockHost<S>, key: Extract<keyof S, string>): Bound;
export function useValue<S extends State>(
  host: BlockHost<S>,
  read: (state: S) => Json | undefined,
  change: (value: Json | undefined, state: S) => Partial<S>,
): Bound;
export function useValue<V extends Json>(host: OptionHost<V>): Bound;
export function useValue(host: BlockHost | OptionHost, key?: string | Read, change?: Change): Bound {
  const read = typeof key === "function" ? key : null;
  const name = typeof key === "function" ? null : (key ?? ("option" in host ? host.option : ""));
  const given = read === null || change === undefined ? null : { read, change };
  const [value, setValue] = useState<Json | undefined>(() => held(host, name, { current: given }));
  const [error, setError] = useState<string | null>(null);
  const binding = useRef<Binding | null>(null);
  // The functions that the binding reads and builds with.
  const functions = useRef(given);

  // The binding starts before the browser paints, so that no event of the
  // user comes before it.
  useLayoutEffect(() => {
    binding.current = start(host, name, functions, setValue, setError);
    return () => {
      const bound = binding.current;
      binding.current = null;
      bound?.release();
    };
  }, [host, name]);

  // A binding made with two functions reads and builds with those of the
  // latest render.
  // Where those read another value from the state than the functions before
  // them, as where a component's input moves to another condition of a list,
  // the binding submits what the user typed with the functions before, and
  // starts again.
  useLayoutEffect(() => {
    if (read === null || change === undefined) return;
    const last = functions.current;
    if (last !== null && last.read === read && last.change === change) return;
    const moved = last === null || !readAlike(last.read, read, (host as BlockHost).state());
    if (moved) binding.current?.release();
    functions.current = { read, change };
    if (moved) binding.current = start(host, name, functions, setValue, setError);
  }, [read, change, host, name]);

  return useMemo(
    () => ({
      value,
      error,
      get: () => binding.current?.get() ?? held(host, name, functions),
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

// The functions of a binding made with two.
type Read = (state: State) => Json | undefined;
type Change = (value: Json | undefined, state: State) => Partial<State>;
interface Functions {
  readonly current: { readonly read: Read; readonly change: Change } | null;
}

// Binds the value under the name, or given none, the value that the
// functions in `functions` read, as they are at the moment the binding calls
// them.
// The binding hands `show` each value that it shows, and `refused` why the
// host refused a submit.
// It takes the notification of each submit that the host refuses, so that
// core raises none.
function start(
  host: BlockHost | OptionHost,
  name: string | null,
  functions: Functions,
  show: (value: Json | undefined) => void,
  refused: (error: string | null) => void,
): Binding {
  const options = {
    settled: (error: unknown) => {
      refused(error === null ? null : errorMessage(error));
    },
    notifier: () => undefined,
  };
  if (name !== null) return bind(host as BlockHost, name, show, options);
  return bind(
    host as BlockHost,
    (state) => functions.current?.read(state),
    (value, state) => functions.current?.change(value, state) ?? {},
    show,
    options,
  );
}

// The value as the host holds it, under the name, or given none, as the
// functions in `functions` read it.
function held(host: BlockHost | OptionHost, name: string | null, functions: Functions): Json | undefined {
  if ("option" in host) return host.value();
  return name === null ? functions.current?.read(host.state()) : own(host.state(), name);
}

// Whether both functions read the same value from the state, where neither
// throws.
function readAlike(a: Read, b: Read, state: State): boolean {
  try {
    return jsonEqual(a(state), b(state));
  } catch {
    return false;
  }
}

// The board, which the store keeps the same object until it announces a
// change.
// Given `select`, the hook gives what `select` takes from the board instead,
// such as each block's name. The component then draws again only where that
// changes, and not for a change of another part of the board, such as a
// block's state.
// What `select` takes is plain data: objects, arrays and values of JSON. The
// hook compares it by value with jsonEqual(), and gives the object that it
// gave before where the new data is equal to it. A Map or a Set does not do,
// since jsonEqual() finds any two of them equal.
// The function may also read what follows the board, such as the board
// actions that core.boardActions.on() offers.
// The hook calls an inline `select` again on each render, since it is a new
// function each time.
export function useBoard(core: Pick<CoreApi, "store">): Board;
export function useBoard<T>(core: Pick<CoreApi, "store">, select: (board: Board) => T): T;
export function useBoard<T>(core: Pick<CoreApi, "store">, select?: (board: Board) => T): Board | T {
  const subscribe = useCallback(
    (notify: () => void) =>
      core.store.subscribe(() => {
        notify();
      }),
    [core],
  );
  // What the component reads: the board, or what `select` took from the board
  // that the store last announced.
  const read = useMemo(() => {
    if (select === undefined) return (): Board | T => core.store.board();
    let last: { readonly board: Board; readonly value: T } | undefined;
    return (): Board | T => {
      const board = core.store.board();
      if (last?.board !== board) {
        const value = select(board);
        last = { board, value: last !== undefined && jsonEqual(last.value, value) ? last.value : value };
      }
      return last.value;
    };
  }, [core, select]);
  return useSyncExternalStore(subscribe, read);
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

// The notifications about the board object given, or every notification of
// the board given null, in the order they came: those about a block for its
// card, or those about a plugin for the plugin's UI and for the section that a
// frontend draws for it.
// CoreApi keeps the same array of them until one comes or goes, so the
// component draws them again only then.
// The board object goes by its kind and its id, so that a component may give
// a new object on each render, as `{ plugin: id }`.
export function useNotified(
  core: { readonly notifications: Pick<CoreApi["notifications"], "get" | "subscribe"> },
  about: BoardObject | null,
): readonly Notification[] {
  const subscribe = useCallback(
    (notify: () => void) =>
      core.notifications.subscribe(() => {
        notify();
      }),
    [core],
  );
  const all = useSyncExternalStore(subscribe, () => core.notifications.get());
  // The kind and the id of the board object, as "plugin" and the id of its
  // class, or neither given null.
  const [kind, id] = about === null ? [] : (Object.entries(about)[0] ?? []);
  return useMemo(
    () =>
      kind === undefined
        ? all
        : all.filter((notification) => notification.about !== undefined && own(notification.about, kind) === id),
    [all, kind, id],
  );
}
