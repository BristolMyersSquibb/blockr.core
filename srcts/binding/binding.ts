// The binding of a control to one value,
// a state arg of a block or the value of a board option,
// by its key, where the board holds it:
// a state arg's name, or the board option's id.
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
//   A pick, such as in a select, it submits at once.
//   Typing, which changes the control with every keystroke,
//   it submits once the user pauses, presses Enter or leaves the control,
//   so that a burst of typing is one edit and one undo step,
//   and typing still pending as the binding ends it submits then.
// - A binding reads the value from the host when it submits,
//   so that a control that changes part of it, such as one column of a list,
//   keeps an edit made in the meantime, such as a column the assistant added.
// A followed request is asked again whenever what it reads changes,
// the revisions of a block's linked data inputs among them,
// and hands on only the answer of the latest request asked.

import type { OptionId } from "../types/board.ts";
import type { BlockHost, HostRequestOpts, OptionHost, State } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import { jsonEqual, own } from "../util/index.ts";

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
  // and what refused it otherwise.
  readonly settled?: (error: unknown) => void;
}

export interface FollowOptions {
  // The parameters it passes, and what else it reads,
  // such as a state arg that its function names.
  // The request is asked again whenever these change,
  // or the revisions of a block's linked data inputs.
  readonly args?: () => Readonly<Record<string, Json>>;
  readonly reads?: () => Json;
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
// A board option's host holds one value, under the board option's id.
export function bind<V extends Json>(
  host: OptionHost<V>,
  key: OptionId,
  show: (value: Json | undefined) => void,
  options?: BindOptions,
): Binding;
export function bind(
  host: BlockHost | OptionHost,
  key: string,
  show: (value: Json | undefined) => void,
  options: BindOptions = {},
): Binding {
  const source = sourceOf(host);
  source.check(key);

  // The value the control last showed, what the user typed while the
  // binding waits for the pause, and whether the binding has ended.
  let shown: { readonly value: Json | undefined } | undefined;
  let typed: Json | undefined;
  let timer: ReturnType<typeof setTimeout> | undefined;
  let released = false;

  const display = (again = false): void => {
    if (released || timer !== undefined) return;
    const value = source.get(key);
    if (!again && shown !== undefined && jsonEqual(shown.value, value)) return;
    shown = { value };
    show(value);
  };

  // Submits the value, unless the host holds it already,
  // and resolves with whether the host took it.
  const submit = async (value: Json): Promise<boolean> => {
    if (jsonEqual(source.get(key), value)) return true;
    try {
      await source.submit(key, value);
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
    get: () => source.get(key),
    async commit(value) {
      const took = await submit(value);
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
// giving the last one asked up.
export function follow(host: BlockHost | OptionHost, name: string, options: FollowOptions): Followed {
  const source = sourceOf(host);

  let asked: string | undefined;
  let latest: AbortController | undefined;
  let released = false;

  const ask = (): void => {
    latest?.abort();
    const controller = new AbortController();
    latest = controller;
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

// What a binding and a followed request read, follow and submit,
// from either host.
interface Source {
  // Throws where the host holds no value under the key.
  check(key: string): void;
  get(key: string): Json | undefined;
  submit(key: string, value: Json): Promise<void>;
  // Follows the values, and what a request reads besides them.
  onValues(fn: () => void): () => void;
  onContext(fn: () => void): () => void;
  // The revisions of a block's linked data inputs, which its requests read.
  inputs(): Json;
  request(name: string, args: Readonly<Record<string, Json>>, opts: HostRequestOpts): Promise<Json | Blob>;
}

function sourceOf(host: BlockHost | OptionHost): Source {
  if ("option" in host) {
    const check = (key: string): void => {
      if (key !== host.option) {
        throw new Error(`The host of the board option "${host.option}" holds no value "${key}".`);
      }
    };
    return {
      check,
      get(key) {
        check(key);
        return host.value();
      },
      submit(key, value) {
        check(key);
        return host.submit(value);
      },
      onValues: (fn) => host.on("value", fn),
      onContext: () => () => undefined,
      inputs: () => null,
      request: (name, args, opts) => host.request(name, args, opts),
    };
  }
  return {
    check: () => undefined,
    get: (key) => own(host.state(), key),
    submit: (key, value) => host.submit({ [key]: value }),
    onValues: (fn) => host.on("state", fn),
    onContext: (fn) => host.on("context", fn),
    inputs: () => host.context().inputs.map((input) => ({ input: input.input, revision: input.revision })),
    request: (name, args, opts) => host.request(name, args, opts),
  };
}

// A choice of a select or a checklist: the value it sets, and how it reads.
export interface Choice {
  readonly value: string;
  readonly label: string;
}

// A choice for each value, which reads as the value.
export function asChoices(values: readonly string[]): readonly Choice[] {
  return values.map((value) => ({ value, label: value }));
}

// The strings in an array, such as the answer of a request that R writes from
// a character vector, or none where it is no array.
export function strings(value: unknown): readonly string[] {
  return Array.isArray(value) ? value.filter((x): x is string => typeof x === "string") : [];
}
