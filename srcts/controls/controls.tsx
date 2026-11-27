// Core's inputs for a package that writes its modules without React,
// such as one that builds nothing,
// through `@blockr/core/controls` or the module controls of blockr.core.
// They are the inputs of core's React SDK, drawn with core's copy of React,
// so that each input is written once.
// Each is bound to one value, as bind() binds it:
// a block's state arg, a board option's value,
// or a value that a function reads from a block's state,
// for a control that sets several state args or edits part of one.
// So each meets the contract of a block's UI
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
import type { BindOptions, Binding, Followed, FollowOptions } from "../binding/index.ts";
import * as inputs from "../inputs/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import type { BlockHost, OptionHost, State } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import { errorMessage, own } from "../util/index.ts";

export interface NumberField {
  // Sets the largest number the field takes, or none.
  setMaxValue(maxValue: number | undefined): void;
  // Says whether the largest number may be for older data,
  // as from the option `loading` of follow() until the request answers.
  // Meanwhile the field submits a number above it, and marks none.
  setOutdated(outdated: boolean): void;
}

export interface ComboBox {
  setItems(items: readonly Item[]): void;
  // Says whether the items may be for older data,
  // as from the option `loading` of follow() until the request answers.
  // Meanwhile the select keeps them, and shows a value that they lack as it
  // is.
  setOutdated(outdated: boolean): void;
}

// An item of a select of the controls: the value it sets by its id, and how
// it reads by its name, the id where it has none, as React Aria's docs write
// items.
export interface Item {
  readonly id: string;
  readonly name?: string;
}

// What a text field of the controls takes besides its key, by the names of the
// props of the SDK's TextField (../inputs/textfield.tsx).
export interface TextFieldOptions {
  readonly label: string;
  // What the field shows while it is empty.
  readonly placeholder?: string;
  // What the field says below itself about what it takes.
  readonly description?: string;
  // The rows of a text area, for text of several lines, where above 1.
  readonly lines?: number;
}

// What a number field of the controls takes besides its key, by the names of
// the props of the SDK's NumberField (../inputs/numberfield.tsx).
export interface NumberFieldOptions {
  readonly label: string;
  // What the field says below itself about what it takes.
  readonly description?: string;
  // The smallest and the largest number that the field takes, the largest
  // until setMaxValue() gives another.
  readonly minValue?: number;
  readonly maxValue?: number;
  // The step of the number, which the field's buttons and arrow keys move it
  // by, and which the number must be on, counting from minValue: 1 for a
  // whole number.
  readonly step?: number;
}

// What a select of the controls takes besides its key, by the names of the
// props of the SDK's ComboBox (../inputs/combobox.tsx).
export interface ComboBoxOptions {
  readonly label: string;
  // What the field shows while nothing is picked.
  readonly placeholder?: string;
  // The items, which the select filters by what the user types.
  readonly items?: readonly Item[];
  // Whether it picks one string, or an array of strings.
  readonly selectionMode?: "single" | "multiple";
  // Where it picks several, whether it submits them in the order of the
  // items, rather than in the order the user picked them.
  readonly ordered?: boolean;
  // Whether it always holds a pick, so that its field offers no button that
  // clears it.
  readonly isRequired?: boolean;
  // What its list says once its items are known and there are none.
  readonly renderEmptyState?: () => string;
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
  // A text field for a string, the SDK's TextField, a text area where it
  // spans lines, whose spelling the browser does not check, as of code.
  text(key: K, options: TextFieldOptions): void;
  // A number field for a number, the SDK's NumberField, no smaller than
  // `minValue`, and no larger than `maxValue` or the number that
  // setMaxValue() gives, but while setOutdated() says that it may be for
  // older data.
  number(key: K, options: NumberFieldOptions): NumberField;
  // A select, the SDK's ComboBox, of one string, or of an array of strings
  // where its selectionMode is "multiple", which shows `placeholder` while
  // nothing is picked.
  // It takes no items until they are given, here or through setItems(),
  // and shows a value meanwhile as it is.
  // Once they are, it shows a value that they lack as missing,
  // and marks itself invalid,
  // but not while setOutdated() says that they may be for older data.
  // So its items must hold every value it takes, not a part of them.
  // It submits one string as the user picks it, and several once, as its list
  // closes, after those the value holds, or where `ordered`, in the order of
  // the items.
  comboBox(key: K, options: ComboBoxOptions): ComboBox;
  checkbox(key: K, options: { label: string }): void;
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

// The controls of a block's UI, each of which takes two functions in place
// of its key, as bind() does: the first reads the value that the control
// shows from the block's state, and the second builds the change that it
// submits from a value and from the state as the host holds it then,
// as for a select of the join that two state args make, or a text field of
// one condition of a list.
export interface BlockControls<S extends State = State> extends Controls<Extract<keyof S, string>> {
  bind(key: Extract<keyof S, string>, show: Show): Binding;
  bind(read: Read<S>, change: Change<S>, show: Show): Binding;
  text(key: Extract<keyof S, string>, options: TextFieldOptions): void;
  text(read: Read<S>, change: Change<S>, options: TextFieldOptions): void;
  number(key: Extract<keyof S, string>, options: NumberFieldOptions): NumberField;
  number(read: Read<S>, change: Change<S>, options: NumberFieldOptions): NumberField;
  comboBox(key: Extract<keyof S, string>, options: ComboBoxOptions): ComboBox;
  comboBox(read: Read<S>, change: Change<S>, options: ComboBoxOptions): ComboBox;
  checkbox(key: Extract<keyof S, string>, options: { label: string }): void;
  checkbox(read: Read<S>, change: Change<S>, options: { label: string }): void;
}

// The functions that a control takes in place of its key, and what it hands
// the value it shows.
type Read<S extends State = State> = (state: S) => Json | undefined;
type Change<S extends State = State> = (value: Json | undefined, state: S) => Partial<S>;
type Show = (value: Json | undefined) => void;

// What a control binds: the value under a key, or the value that `read`
// reads, with the change that `change` builds.
type Target = string | { readonly read: Read; readonly change: Change };

// What a control draws, in the order the controls were made,
// by an id that keeps its place as React draws them again.
interface Entry {
  readonly id: number;
  readonly draw: () => ReactElement;
}

// The controls of a block's UI, bound to the block's state args,
// or of a board option's UI, bound to the board option's value under its id.
// The module puts `el` in its element and returns `cleanup` from its render().
export function controls<S extends State>(host: BlockHost<S>, signal: AbortSignal): BlockControls<S>;
// A board option's controls take its id as their key.
export function controls<V extends Json>(host: OptionHost<V>, signal: AbortSignal): Controls;
export function controls(host: BlockHost | OptionHost, signal: AbortSignal): BlockControls {
  const el = document.createElement("div");
  el.className = styles.stack;
  const root = createRoot(el);

  const entries: Entry[] = [];
  const bindings: Binding[] = [];
  const followed: Followed[] = [];
  let error: string | null = null;
  let ended = false;

  // Draws the controls as they are now, before it returns.
  const redraw = (): void => {
    if (ended) return;
    const drawn = entries.map((entry) => <Fragment key={entry.id}>{entry.draw()}</Fragment>);
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
    entries.push({ id: entries.length, draw });
    redraw();
  }

  function add(node: Node): void {
    put(() => <Slot node={node} />);
  }

  // Binds what the control binds, as bind() does given a key or the two
  // functions.
  function bindTo(target: Target, show: Show, options: BindOptions): Binding {
    // The binding's overloads take the host's own type, which the union here
    // is not.
    const binding =
      typeof target === "string"
        ? bindValue(host as BlockHost, target, show, options)
        : bindValue(host as BlockHost, target.read, target.change, show, options);
    bindings.push(binding);
    return binding;
  }

  // Binds an input of core's React SDK as bindTo() does,
  // which it draws with the value as the binding last showed it
  // and why the host refused what it last submitted, if it did.
  function input(target: Target, draw: (bound: Bound) => ReactElement): void {
    let value: Json | undefined;
    let refusal: string | null = null;
    // The binding shows the value once as it is made,
    // before the input is put among the controls.
    let made = false;
    const binding = bindTo(
      target,
      (shown) => {
        value = shown;
        if (made) redraw();
      },
      {
        settled: (refused) => {
          refusal = refused === null ? null : errorMessage(refused);
          redraw();
        },
        // The input shows why the host refused what it submitted, so core
        // raises no notification of it.
        notifier: () => undefined,
      },
    );
    made = true;
    put(() =>
      draw({
        value,
        error: refusal,
        get: () => binding.get(),
        commit: (next) => binding.commit(next),
        unset: () => binding.unset(),
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

  function bind(key: string | Read, next: Change | Show, last?: Show): Binding {
    const [target, show] = after(key, next, last);
    // The line below the controls shows why the host refused what the widget
    // submitted, so core raises no notification of it.
    return bindTo(target, show, { settled: alert, notifier: () => undefined });
  }

  function follow(name: string, options: FollowOptions): Followed {
    const request = followRequest(host, name, options);
    followed.push(request);
    return request;
  }

  function text(key: string | Read, next: Change | TextFieldOptions, last?: TextFieldOptions): void {
    const [target, options] = after(key, next, last);
    input(target, (bound) => <inputs.TextField bound={bound} spellCheck="false" {...options} />);
  }

  function number(key: string | Read, next: Change | NumberFieldOptions, last?: NumberFieldOptions): NumberField {
    const [target, options] = after(key, next, last);
    let maxValue = options.maxValue;
    let outdated = false;
    input(target, (bound) => <inputs.NumberField bound={bound} {...options} maxValue={maxValue} outdated={outdated} />);
    return {
      setMaxValue(next) {
        maxValue = next;
        redraw();
      },
      setOutdated(next) {
        outdated = next;
        redraw();
      },
    };
  }

  function comboBox(key: string | Read, next: Change | ComboBoxOptions, last?: ComboBoxOptions): ComboBox {
    const [target, options] = after(key, next, last);
    let items = options.items;
    let outdated = false;
    input(target, (bound) => (
      <inputs.ComboBox
        bound={bound}
        label={options.label}
        placeholder={options.placeholder}
        defaultItems={items}
        selectionMode={options.selectionMode ?? "single"}
        ordered={options.ordered}
        isRequired={options.isRequired}
        renderEmptyState={options.renderEmptyState}
        outdated={outdated}
      >
        {(item) => <inputs.ComboBoxItem id={item.id}>{item.name ?? item.id}</inputs.ComboBoxItem>}
      </inputs.ComboBox>
    ));
    return {
      setItems(next) {
        items = next;
        redraw();
      },
      setOutdated(next) {
        outdated = next;
        redraw();
      },
    };
  }

  function checkbox(key: string | Read, next: Change | { label: string }, last?: { label: string }): void {
    const [target, options] = after(key, next, last);
    input(target, (bound) => <inputs.Checkbox bound={bound}>{options.label}</inputs.Checkbox>);
  }

  function cleanup(): void {
    if (ended) return;
    ended = true;
    for (const binding of bindings) binding.release();
    for (const request of followed) request.release();
    root.unmount();
  }

  signal.addEventListener("abort", cleanup, { once: true });

  return { el, add, get, bind, text, number, comboBox, checkbox, follow, alert, cleanup };
}

// What a control binds, given its key or the two functions, and what it takes
// besides, which follows them.
function after<T>(key: string | Read, next: Change | T, last: T | undefined): [Target, T] {
  return typeof key === "function" ? [{ read: key, change: next as Change }, last as T] : [key, next as T];
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
