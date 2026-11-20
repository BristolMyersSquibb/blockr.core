// The binding of a control to one value that the host holds:
// a state arg of a block, by its name,
// the value of a board option, by the board option's id,
// or a value that a function reads from a block's state,
// for a control that sets several state args or edits part of one,
// such as the join that two state args make, or one condition of a list.
// It needs neither the DOM nor React,
// so that any widget meets the contract of a block's UI once it is bound
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract),
// whether core's React inputs, a module's own or blockr.ui's:
// - A binding shows the value as the host holds it,
//   and shows it again whenever it changes,
//   since undo, the assistant and plugins change it too,
//   but not while the user is typing in the control.
// - A binding submits only when the user acts,
//   never to fill in a value once data arrives.
//   A pick, such as in a select, it submits at once, and so it unsets a state
//   arg that the user clears.
//   Typing, which changes the control with every keystroke,
//   it submits once the user pauses, presses Enter or leaves the control,
//   so that a burst of typing is one edit and one undo step,
//   and typing still pending as the binding ends it submits then.
// - A binding builds each change that it submits from the state as the host
//   holds it then, after the pause for typing too,
//   so that a control that edits part of a state arg, such as one condition
//   of a list, keeps an edit made in the meantime, such as a condition that
//   the assistant added.
//   It submits nothing that the host holds already.
// - Where the host refuses a submit, core tells the user why through a
//   notification about the block or the board option, unless the binding
//   passes the host a notifier, as one does for a control that shows why
//   itself, as core's inputs do below themselves.
//   Where the second of two functions fails to build a change, which no host
//   then sees, the binding tells the user why itself in the same way, and
//   logs what the function threw.
// A followed request is asked again whenever what it reads changes,
// the revisions of a block's linked data inputs among them,
// and hands on only the answer of the latest request asked.
// It says each time that it asks,
// so that a module can tell that the last answer it got is for older data
// until that ask answers or fails.

import type { OptionId } from "../types/board.ts";
import type { Notifier } from "../types/core-api.ts";
import type { BlockHost, HostRequestOpts, OptionHost, State } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import { asError, errorMessage, jsonEqual, own } from "../util/index.ts";

// How long a binding waits after the last keystroke before it submits,
// in milliseconds.
export const PAUSE = 500;

export interface Binding {
  // The value as the host holds it now.
  get(): Json | undefined;
  // Submits a value that the user picked, at once,
  // unless the host holds it already,
  // and resolves with whether the host took it.
  // Where the host refused it, the control is shown the value the host holds
  // again.
  commit(value: Json): Promise<boolean>;
  // Unsets the state arg, as the user clears an arg without a default, at
  // once, unless the host holds no value under it already, and resolves with
  // whether the host took it, as commit() does.
  // A binding made with two functions submits the change that the second
  // builds from undefined.
  // A board option always holds a value, so that of a board option refuses.
  unset(): Promise<boolean>;
  // Takes what the user typed, which it submits once the user pauses,
  // unless the user types on.
  // Until then, the control is shown nothing the host announces.
  // Given undefined, as for a number out of range, it submits nothing.
  type(value: Json | undefined): void;
  // Submits what the user typed at once,
  // as on Enter or as the user leaves the control.
  flush(): void;
  // Submits what the user typed, and stops following the host,
  // as the control goes.
  release(): void;
}

export interface BindOptions {
  // Hears how each submit ended: null where the host took it,
  // and otherwise what refused it, or what the second of two functions threw.
  readonly settled?: (error: unknown) => void;
  // Takes the notification of each submit that the host refuses, which core
  // then does not raise, for a control that shows why itself, through
  // `settled`. The binding passes it to the host's submit().
  // Given one, the binding raises no notification either where the second of
  // two functions fails to build a change.
  readonly notifier?: Notifier;
}

export interface FollowOptions {
  // The parameters it passes, and what else it reads,
  // such as a state arg that its function names.
  // The request is asked again whenever these change,
  // or the revisions of a block's linked data inputs.
  readonly args?: () => Readonly<Record<string, Json>>;
  readonly reads?: () => Json;
  // What it does each time it asks, until that ask answers or fails,
  // such as keep a module's inputs from marking the state
  // against the last answer, which is for older data meanwhile.
  readonly loading?: () => void;
  // What it does with an answer, and with why a request failed,
  // where it does more than keep the last answer it had.
  readonly answer: (answer: Json | Blob) => void;
  readonly failed?: (error: unknown) => void;
}

export interface Followed {
  // Asks again, as after the user acted, such as by opening a directory.
  refresh(): void;
  // Gives up the request in flight, and stops following the host.
  release(): void;
}

// Binds a control to the value under the key,
// calling `show` with the value now and whenever it changes,
// but while the user is typing in the control.
export function bind<S extends State>(
  host: BlockHost<S>,
  key: Extract<keyof S, string>,
  show: (value: Json | undefined) => void,
  options?: BindOptions,
): Binding;
// Binds a control to the value that `read` reads from the block's state,
// such as the join that two state args make, or one condition of a list.
// The binding calls `change` only as it submits a value,
// with the state as the host holds it then,
// for the change of the state that it submits,
// such as both state args of the join, or the list with that condition
// replaced.
export function bind<S extends State>(
  host: BlockHost<S>,
  read: (state: S) => Json | undefined,
  change: (value: Json | undefined, state: S) => Partial<S>,
  show: (value: Json | undefined) => void,
  options?: BindOptions,
): Binding;
// A board option's host holds one value, under the board option's id.
export function bind<V extends Json>(
  host: OptionHost<V>,
  key: OptionId,
  show: (value: Json | undefined) => void,
  options?: BindOptions,
): Binding;
export function bind(
  host: BlockHost | OptionHost,
  key: string | Read,
  next: Change | Show,
  last?: Show | BindOptions,
  given?: BindOptions,
): Binding {
  // A binding made with two functions takes `show` and the options after
  // them.
  const [change, show, options] =
    typeof key === "function"
      ? [next as Change, last as Show, given ?? {}]
      : [undefined, next as Show, (last as BindOptions | undefined) ?? {}];
  const source = createSource(host);
  const held = source.value(key, change);
  const submitting = options.notifier === undefined ? {} : { notifier: options.notifier };

  // The value the control last showed, what the user typed while the
  // binding waits for the pause, and whether the binding has ended.
  let shown: { readonly value: Json | undefined } | undefined;
  let typed: Json | undefined;
  let timer: ReturnType<typeof setTimeout> | undefined;
  let released = false;

  const display = (again = false): void => {
    if (released || timer !== undefined) return;
    const value = held.get();
    if (!again && shown !== undefined && jsonEqual(shown.value, value)) return;
    shown = { value };
    show(value);
  };

  // The notification of why `change` failed to build the last change, until
  // a later change builds.
  let unbuilt: (() => void) | undefined;

  // Says why `change` failed to build a change, which no host then sees, as
  // core says why the host refused a submit: through a notification about
  // the block, in place of the last one, unless the binding has a notifier.
  // It logs what `change` threw, since that is the module's fault.
  const unbuild = (error: unknown): void => {
    host.log.error(asError(error));
    unbuilt?.();
    unbuilt =
      options.notifier === undefined
        ? host.notify({
            severity: "error",
            code: "change_failed",
            message: `The control failed to build the change it submits: ${errorMessage(error)}`,
          })
        : undefined;
  };

  // Submits the value, or unsets the state arg given undefined, unless the
  // host holds it already, and resolves with whether the host took it.
  // Where `change` throws, the submit fails as one that the host refused
  // does.
  const submit = async (value: Json | undefined): Promise<boolean> => {
    let submitted: Promise<void> | null;
    try {
      submitted = held.submit(value, submitting);
    } catch (error) {
      unbuild(error);
      options.settled?.(error);
      return false;
    }
    unbuilt?.();
    unbuilt = undefined;
    try {
      if (submitted !== null) await submitted;
    } catch (error) {
      options.settled?.(error);
      return false;
    }
    options.settled?.(null);
    return true;
  };

  const send = (): void => {
    const value = typed;
    typed = undefined;
    if (value !== undefined) void submit(value);
  };

  const flush = (): void => {
    if (timer === undefined) return;
    clearTimeout(timer);
    timer = undefined;
    send();
  };

  const off = source.onValues(() => {
    display();
  });
  display(true);

  return {
    get: () => held.get(),
    async commit(value) {
      const took = await submit(value);
      if (!took) display(true);
      return took;
    },
    async unset() {
      const took = await submit(undefined);
      if (!took) display(true);
      return took;
    },
    type(value) {
      if (released) return;
      typed = value;
      clearTimeout(timer);
      timer = setTimeout(() => {
        timer = undefined;
        send();
      }, PAUSE);
    },
    flush,
    release() {
      if (released) return;
      released = true;
      off();
      flush();
    },
  };
}

// Follows a request of the block's class or of the board option,
// asked now and again whenever what it reads changes,
// giving the last one asked up,
// and calls `loading` each time it asks.
export function follow(host: BlockHost | OptionHost, name: string, options: FollowOptions): Followed {
  const source = createSource(host);

  let asked: string | undefined;
  let latest: AbortController | undefined;
  let released = false;

  const ask = (): void => {
    latest?.abort();
    const controller = new AbortController();
    latest = controller;
    options.loading?.();
    source.request(name, options.args?.() ?? {}, { signal: controller.signal }).then(
      (answer) => {
        if (latest === controller && !controller.signal.aborted) options.answer(answer);
      },
      (error: unknown) => {
        if (latest === controller && !controller.signal.aborted) options.failed?.(error);
      },
    );
  };

  const check = (): void => {
    if (released) return;
    const reads = JSON.stringify([source.inputs(), options.args?.() ?? {}, options.reads?.() ?? null]);
    if (reads === asked) return;
    asked = reads;
    ask();
  };

  const offValues = source.onValues(check);
  const offContext = source.onContext(check);
  check();

  return {
    refresh() {
      asked = undefined;
      check();
    },
    release() {
      if (released) return;
      released = true;
      offValues();
      offContext();
      latest?.abort();
    },
  };
}

// The options that a binding passes to its host's submit().
type SubmitOpts = Parameters<OptionHost["submit"]>[1];

// The functions of a binding made with two, and what shows its value.
type Read = (state: State) => Json | undefined;
type Change = (value: Json | undefined, state: State) => Partial<State>;
type Show = (value: Json | undefined) => void;

// What a binding shows and submits.
interface Value {
  // The value as the host holds it now.
  get(): Json | undefined;
  // Submits the value, or unsets it given undefined, or returns null where
  // the host holds it already.
  submit(value: Json | undefined, opts: SubmitOpts): Promise<void> | null;
}

// What a binding and a followed request read, follow and submit,
// from either host.
interface Source {
  // The value under the key: a state arg's name, or the board option's id.
  // A state arg's name reads the arg, and sets it alone.
  // Given a function in place of the key, the value is what that function
  // reads from a block's state, and `change` builds the change that sets it.
  // Throws where the host holds no value under the key.
  value(key: string | Read, change: Change | undefined): Value;
  // Follows the values, and what a request reads besides them.
  onValues(fn: () => void): () => void;
  onContext(fn: () => void): () => void;
  // The revisions of a block's linked data inputs, which its requests read.
  inputs(): Json;
  request(name: string, args: Readonly<Record<string, Json>>, opts: HostRequestOpts): Promise<Json | Blob>;
}

function createSource(host: BlockHost | OptionHost): Source {
  if ("option" in host) {
    return {
      value(key) {
        if (typeof key === "function") {
          throw new TypeError(`The value of the board option "${host.option}" is bound by its id, not through functions.`);
        }
        if (key !== host.option) {
          throw new Error(`The host of the board option "${host.option}" holds no value "${key}".`);
        }
        return {
          get: () => host.value(),
          submit(value, opts) {
            if (jsonEqual(host.value(), value)) return null;
            if (value === undefined) {
              return Promise.reject(new Error(`The board option "${host.option}" always holds a value, so none of it is unset.`));
            }
            return host.submit(value, opts);
          },
        };
      },
      onValues: (fn) => host.on("value", fn),
      onContext: () => () => undefined,
      inputs: () => null,
      request: (name, args, opts) => host.request(name, args, opts),
    };
  }
  return {
    value(key, change) {
      const read: Read = typeof key === "function" ? key : (state) => own(state, key);
      const build: Change | undefined = typeof key === "function" ? change : (value) => ({ [key]: value });
      if (typeof build !== "function") {
        throw new TypeError("A binding made with a function that reads its value takes a second, which builds the change it submits.");
      }
      return {
        get: () => read(host.state()),
        submit(value, opts) {
          const state = host.state();
          const next = build(value, state);
          return holds(state, next) ? null : host.submit(next, opts);
        },
      };
    },
    onValues: (fn) => host.on("state", fn),
    onContext: (fn) => host.on("context", fn),
    inputs: () => host.context().inputs.map((input) => ({ input: input.input, revision: input.revision })),
    request: (name, args, opts) => host.request(name, args, opts),
  };
}

// Whether the state holds each state arg of the change as the change gives
// it, so that a submit of the change would change nothing.
// The state holds an arg that the change gives as undefined where it holds
// no value under it.
function holds(state: State, change: Partial<State>): boolean {
  return Object.entries(change).every(([arg, value]) => jsonEqual(own(state, arg), value));
}

// The strings in an array, such as the answer of a request that R writes from
// a character vector, or none where it is no array.
export function strings(value: unknown): readonly string[] {
  return Array.isArray(value) ? value.filter((x): x is string => typeof x === "string") : [];
}
