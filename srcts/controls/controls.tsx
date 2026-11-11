// Core's inputs for a package that writes its modules without React,
// such as one that builds nothing,
// through `@blockr/core/controls` or the module blockr.core-controls.
// They are the inputs of core's React SDK, drawn with core's copy of React,
// so that each input is written once.
// Each is bound to one value, a block's state arg or a board option's value,
// and so meets the contract of a block's UI
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract).
// They draw before the call that makes or changes one returns,
// and as the host announces a change,
// so that a module and its tests find them in `el` as they are,
// with no wait for React.
// A widget of a package's own, such as blockr.ui's, binds through bind().

import { Fragment, useLayoutEffect, useRef } from "react";
import type { ReactElement } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { bind as bindValue, follow as followRequest } from "../binding/index.ts";
import type { Binding, Choice, Followed, FollowOptions } from "../binding/index.ts";
import * as inputs from "../inputs/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import type { BlockHost, OptionHost, State } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import { errorMessage, own } from "../util/index.ts";

export interface NumberInput {
  // Sets the largest number the input takes, or none.
  setMax(max: number | undefined): void;
}

export interface Select {
  setChoices(choices: readonly Choice[]): void;
}

export interface Checklist {
  setChoices(choices: readonly string[]): void;
}

// The controls of a module, by the keys of the values they bind:
// a block's state args, or the board option's id.
export interface Controls<K extends string = string> {
  // The element that holds the controls in the order they were made,
  // each of which says below itself why the host refused what it submitted,
  // and below them the line that alert() fills.
  readonly el: HTMLElement;
  // Puts an element that a module draws itself among the controls.
  add(node: Node): void;
  // The value under the key as the host holds it now.
  get(key: K): Json | undefined;
  // Binds a control of the module's own to the value under the key,
  // calling `show` with the value now and whenever it changes,
  // but while the user is typing in the control.
  // Why the host refused what it submitted shows in the line below the
  // controls.
  bind(key: K, show: (value: Json | undefined) => void): Binding;
  // A text input for a string, a text area where it spans lines.
  text(key: K, options: { label: string; placeholder?: string; lines?: number }): void;
  // A number input for an integer, no smaller than `min`.
  number(key: K, options: { label: string; min?: number }): NumberInput;
  // A select of one string, which shows `placeholder` while the value is
  // unset.
  // It takes no choices until they are given, here or through setChoices(),
  // and shows a value meanwhile as it is.
  // Once they are, it shows a value that they lack as missing,
  // and marks itself invalid.
  // So its choices must list every value it takes, not a part of them.
  select(key: K, options: { label: string; placeholder: string; choices?: readonly Choice[] }): Select;
  checkbox(key: K, options: { label: string }): void;
  // A checkbox for each string that an array may hold,
  // which says `empty` once its choices are known and there are none.
  // It takes no choices until they are given, here or through setChoices(),
  // and shows a string the value holds and they lack,
  // marked as missing once they are given, so that the user can clear it.
  // So its choices must list every string it takes, not a part of them.
  // It submits the checked strings in the order the user checked them,
  // after those the value holds, or where `ordered`, in the order of the
  // choices, as for severities.
  checklist(
    key: K,
    options: { legend: string; choices?: readonly string[]; empty?: string; ordered?: boolean },
  ): Checklist;
  // A request of the block's class or of the board option,
  // asked as the module mounts and again whenever what it reads changes.
  follow(name: string, options: FollowOptions): Followed;
  // Says why something the user did failed, such as an upload,
  // or clears the line given null.
  alert(error: unknown): void;
  // Submits all typing still pending, gives up the requests in flight,
  // stops following the host and empties `el`.
  // The module returns it from its render(), for core to call as it unmounts
  // the module, and the controls call it themselves as core aborts the signal.
  readonly cleanup: () => void;
}

// What a control draws, in the order the controls were made,
// by an id that keeps its place as React draws them again.
interface Item {
  readonly id: number;
  readonly draw: () => ReactElement;
}

// The controls of a block's UI, bound to the block's state args,
// or of a board option's UI, bound to the board option's value under its id.
// The module puts `el` in its element and returns `cleanup` from its render().
export function controls<S extends State>(
  host: BlockHost<S>,
  signal: AbortSignal,
): Controls<Extract<keyof S, string>>;
// A board option's controls take its id as their key.
export function controls<V extends Json>(host: OptionHost<V>, signal: AbortSignal): Controls;
export function controls(host: BlockHost | OptionHost, signal: AbortSignal): Controls {
  const el = document.createElement("div");
  el.className = styles.stack;
  const root = createRoot(el);

  const items: Item[] = [];
  const bindings: Binding[] = [];
  const followed: Followed[] = [];
  let error: string | null = null;
  let ended = false;

  // Draws the controls as they are now, before it returns.
  const redraw = (): void => {
    if (ended) return;
    const drawn = items.map((item) => <Fragment key={item.id}>{item.draw()}</Fragment>);
    flushSync(() => {
      root.render(
        <>
          {drawn}
          {error === null ? null : (
            <p role="alert" className={styles.alert}>
              {error}
            </p>
          )}
        </>,
      );
    });
  };

  function put(draw: () => ReactElement): void {
    items.push({ id: items.length, draw });
    redraw();
  }

  function add(node: Node): void {
    put(() => <Slot node={node} />);
  }

  // Binds an input of core's React SDK to the value under the key,
  // which it draws with the value as the binding last showed it
  // and why the host refused what it last submitted, if it did.
  function input(key: string, draw: (bound: Bound) => ReactElement): void {
    let value: Json | undefined;
    let refusal: string | null = null;
    // The binding shows the value once as it is made,
    // before the input is put among the controls.
    let made = false;
    const binding = bindValue(
      host as BlockHost,
      key,
      (shown) => {
        value = shown;
        if (made) redraw();
      },
      {
        settled: (refused) => {
          refusal = refused === null ? null : errorMessage(refused);
          redraw();
        },
      },
    );
    bindings.push(binding);
    made = true;
    put(() =>
      draw({
        value,
        error: refusal,
        get: () => binding.get(),
        commit: (next) => binding.commit(next),
        type: (next) => {
          binding.type(next);
        },
        flush: () => {
          binding.flush();
        },
      }),
    );
  }

  function alert(next: unknown): void {
    error = next === null ? null : errorMessage(next);
    redraw();
  }

  function get(key: string): Json | undefined {
    if (!("option" in host)) return own(host.state(), key);
    if (key !== host.option) {
      throw new Error(`The host of the board option "${host.option}" holds no value "${key}".`);
    }
    return host.value();
  }

  function bind(key: string, show: (value: Json | undefined) => void): Binding {
    // The binding's overloads take the host's own type, which the union here
    // is not.
    const binding = bindValue(host as BlockHost, key, show, { settled: alert });
    bindings.push(binding);
    return binding;
  }

  function follow(name: string, options: FollowOptions): Followed {
    const request = followRequest(host, name, options);
    followed.push(request);
    return request;
  }

  function text(key: string, options: { label: string; placeholder?: string; lines?: number }): void {
    input(key, (bound) => <inputs.TextInput bound={bound} {...options} />);
  }

  function number(key: string, options: { label: string; min?: number }): NumberInput {
    let max: number | undefined;
    input(key, (bound) => <inputs.NumberInput bound={bound} label={options.label} min={options.min} max={max} />);
    return {
      setMax(next) {
        max = next;
        redraw();
      },
    };
  }

  function select(key: string, options: { label: string; placeholder: string; choices?: readonly Choice[] }): Select {
    let choices = options.choices;
    input(key, (bound) => (
      <inputs.Select bound={bound} label={options.label} placeholder={options.placeholder} choices={choices} />
    ));
    return {
      setChoices(next) {
        choices = next;
        redraw();
      },
    };
  }

  function checkbox(key: string, options: { label: string }): void {
    input(key, (bound) => <inputs.Checkbox bound={bound} label={options.label} />);
  }

  function checklist(
    key: string,
    options: { legend: string; choices?: readonly string[]; empty?: string; ordered?: boolean },
  ): Checklist {
    let choices = options.choices;
    input(key, (bound) => (
      <inputs.Checklist
        bound={bound}
        legend={options.legend}
        choices={choices}
        empty={options.empty}
        ordered={options.ordered}
      />
    ));
    return {
      setChoices(next) {
        choices = next;
        redraw();
      },
    };
  }

  function cleanup(): void {
    if (ended) return;
    ended = true;
    for (const binding of bindings) binding.release();
    for (const request of followed) request.release();
    root.unmount();
  }

  signal.addEventListener("abort", cleanup, { once: true });

  return { el, add, get, bind, text, number, select, checkbox, checklist, follow, alert, cleanup };
}

// An element that the module draws itself, which React puts in a wrapper that
// takes no part in the layout, and leaves to the module.
function Slot({ node }: { node: Node }): ReactElement {
  const ref = useRef<HTMLDivElement>(null);
  useLayoutEffect(() => {
    ref.current?.replaceChildren(node);
  }, [node]);
  return <div ref={ref} className="bc:contents" />;
}
