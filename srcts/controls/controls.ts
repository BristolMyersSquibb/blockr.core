// The inputs of core's React SDK, drawn without React, for a package that
// draws its modules with the DOM alone, such as one that builds nothing,
// through `@blockr/core/controls` or the module blockr.core-controls.
// Each control is bound to one value, a block's state arg or a board option's
// value, by the binding that core's React inputs build on,
// so that it meets the contract of a block's UI as they do
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract).
// A control updates in place, so that it keeps its focus and caret,
// and keeps nothing document-wide, such as an element id,
// so that a block's UI mounts in several places at once:
// a label holds its control.
// A widget of a package's own, such as blockr.ui's, binds through bind().

import { bind as bindValue, follow as followRequest, strings } from "../binding/index.ts";
import type { Binding, Choice, Followed, FollowOptions } from "../binding/index.ts";
import { styles } from "../styles/index.ts";
import type { BlockHost, OptionHost, State } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import { jsonEqual, messageOf, own } from "../util/index.ts";

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
  // and below them the line that says why the user's last action failed,
  // such as a submit that the host refused.
  readonly el: HTMLElement;
  // Puts an element that a module draws itself among the controls.
  add(node: Node): void;
  // The value under the key as the host holds it now.
  get(key: K): Json | undefined;
  // Binds a control to the value under the key,
  // calling `show` with the value now and whenever it changes,
  // but while the user is typing in the control.
  bind(key: K, show: (value: Json | undefined) => void): Binding;
  // A text input for a string, a text area where it spans lines.
  text(key: K, options: { label: string; placeholder?: string; lines?: number }): void;
  // A number input for an integer, no smaller than `min`.
  number(key: K, options: { label: string; min?: number }): NumberInput;
  // A select of one string, which shows `placeholder` while the value is
  // unset.
  select(key: K, options: { label: string; placeholder: string; choices?: readonly Choice[] }): Select;
  checkbox(key: K, options: { label: string }): void;
  // A checkbox for each string that an array may hold,
  // which says `empty` once its choices are known and there are none.
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
  // Submits all typing still pending,
  // gives up the requests in flight and stops following the host.
  // Core calls it as it unmounts the module,
  // as the cleanup that render() returns.
  readonly cleanup: () => void;
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

  const line = document.createElement("p");
  line.setAttribute("role", "alert");
  line.className = styles.alert;
  line.hidden = true;
  el.append(line);

  const bindings: Binding[] = [];
  const followed: Followed[] = [];

  signal.addEventListener("abort", () => {
    for (const request of followed) request.release();
  }, { once: true });

  function add(node: Node): void {
    line.before(node);
  }

  function alert(error: unknown): void {
    line.textContent = error === null ? "" : messageOf(error);
    line.hidden = error === null;
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
    const lines = options.lines ?? 1;
    const input = lines > 1 ? document.createElement("textarea") : document.createElement("input");
    if (input instanceof HTMLTextAreaElement) input.rows = lines;
    else input.type = "text";
    input.className = styles.box;
    input.spellcheck = false;
    if (options.placeholder !== undefined) input.placeholder = options.placeholder;

    const binding = bind(key, (value) => {
      const shown = typeof value === "string" ? value : "";
      if (input.value !== shown) input.value = shown;
    });
    typing(input, binding, lines > 1, () => input.value);
    add(field(options.label, input));
  }

  function number(key: string, options: { label: string; min?: number }): NumberInput {
    const input = document.createElement("input");
    input.type = "number";
    input.step = "1";
    input.required = true;
    input.className = styles.box;
    if (options.min !== undefined) input.min = String(options.min);
    let max: number | undefined;

    // The integer the input holds, if it holds one in its range.
    const read = (): number | undefined => {
      const text = input.value.trim();
      const value = text === "" ? Number.NaN : Number(text);
      if (!Number.isInteger(value)) return undefined;
      if (options.min !== undefined && value < options.min) return undefined;
      if (max !== undefined && value > max) return undefined;
      return value;
    };
    const mark = (): void => {
      input.setAttribute("aria-invalid", String(read() === undefined));
    };

    const binding = bind(key, (value) => {
      const shown = typeof value === "number" ? String(value) : "";
      if (input.value !== shown) input.value = shown;
      mark();
    });
    typing(input, binding, false, read);
    input.addEventListener("input", mark);
    add(field(options.label, input));

    return {
      setMax(next) {
        max = next;
        if (next === undefined) input.removeAttribute("max");
        else input.max = String(next);
        mark();
      },
    };
  }

  function select(
    key: string,
    options: { label: string; placeholder: string; choices?: readonly Choice[] },
  ): Select {
    const select = document.createElement("select");
    select.className = styles.box;
    let choices = options.choices ?? [];
    let current: string | undefined;
    // The options it shows, so that it rebuilds them only once they change,
    // and so keeps the list open while the user picks.
    let built = "";

    const draw = (): void => {
      // A value the choices lack, such as one an app script gave before the
      // choices arrive, still shows.
      const shown =
        current === undefined || choices.some((choice) => choice.value === current)
          ? choices
          : [...choices, { value: current, label: current }];
      const drawn = JSON.stringify([current === undefined, shown]);
      if (drawn !== built) {
        built = drawn;
        select.replaceChildren(
          ...(current === undefined ? [option("", options.placeholder, true)] : []),
          ...shown.map((choice) => option(choice.value, choice.label)),
        );
      }
      select.value = current ?? "";
    };

    const binding = bind(key, (value) => {
      current = typeof value === "string" ? value : undefined;
      draw();
    });
    select.addEventListener("change", () => {
      void binding.commit(select.value);
    });
    add(field(options.label, select));

    return {
      setChoices(next) {
        choices = next;
        draw();
      },
    };
  }

  function checkbox(key: string, options: { label: string }): void {
    const input = document.createElement("input");
    input.type = "checkbox";
    input.className = styles.checkbox;

    const binding = bind(key, (value) => {
      input.checked = value === true;
    });
    input.addEventListener("change", () => {
      void binding.commit(input.checked);
    });

    const label = document.createElement("label");
    label.className = styles.check;
    label.append(input, options.label);
    add(label);
  }

  function checklist(
    key: string,
    options: { legend: string; choices?: readonly string[]; empty?: string; ordered?: boolean },
  ): Checklist {
    const fieldset = document.createElement("fieldset");
    fieldset.className = styles.fieldset;
    const legend = document.createElement("legend");
    legend.className = styles.legend;
    legend.textContent = options.legend;
    const list = document.createElement("div");
    list.className = styles.list;
    fieldset.append(legend, list);

    let choices = options.choices ?? [];
    let known = options.choices !== undefined;
    let held: readonly string[] = [];
    let built: readonly string[] | undefined;
    const boxes = new Map<string, HTMLInputElement>();

    const draw = (): void => {
      // What the value holds and the choices lack, such as a column only one
      // input has, still shows, so that the user can clear it.
      const shown = [...choices, ...held.filter((value) => !choices.includes(value))];
      if (built === undefined || !jsonEqual(built, shown)) {
        built = shown;
        boxes.clear();
        list.replaceChildren(...shown.map((value) => box(value)));
      }
      if (known && options.empty !== undefined && shown.length === 0 && list.childElementCount === 0) {
        list.append(hint(options.empty));
      }
      for (const [value, input] of boxes) input.checked = held.includes(value);
    };

    function box(value: string): HTMLLabelElement {
      const input = document.createElement("input");
      input.type = "checkbox";
      input.value = value;
      input.className = styles.checkbox;
      input.addEventListener("change", () => {
        // The values the host holds now,
        // since the assistant may have added one since the checkboxes showed
        // them, with this one added at the end or taken out.
        const now = strings(binding.get());
        const next = input.checked
          ? now.includes(value) ? now : [...now, value]
          : now.filter((other) => other !== value);
        void binding.commit(
          options.ordered === true
            ? [...choices.filter((choice) => next.includes(choice)), ...next.filter((other) => !choices.includes(other))]
            : next,
        );
      });
      boxes.set(value, input);
      const label = document.createElement("label");
      label.className = styles.check;
      label.append(input, value);
      return label;
    }

    const binding = bind(key, (value) => {
      held = strings(value);
      draw();
    });
    add(fieldset);

    return {
      setChoices(next) {
        choices = next;
        known = true;
        built = undefined;
        draw();
      },
    };
  }

  function cleanup(): void {
    for (const binding of bindings) binding.release();
    for (const request of followed) request.release();
  }

  return { el, add, get, bind, text, number, select, checkbox, checklist, follow, alert, cleanup };
}

// A label that holds its control, so that it needs no id.
export function field(text: string, control: HTMLElement): HTMLLabelElement {
  const label = document.createElement("label");
  label.className = styles.field;
  const span = document.createElement("span");
  span.className = styles.label;
  span.textContent = text;
  label.append(span, control);
  return label;
}

export function hint(text: string): HTMLParagraphElement {
  const p = document.createElement("p");
  p.className = styles.hint;
  p.textContent = text;
  return p;
}

// Has a binding take what the user types into the input after each
// keystroke, and submit it at once on Enter,
// or on Ctrl or Cmd with Enter in a text area, and as the user leaves it.
function typing(input: HTMLElement, binding: Binding, multiline: boolean, read: () => Json | undefined): void {
  input.addEventListener("input", () => {
    binding.type(read());
  });
  input.addEventListener("keydown", (event) => {
    if (event.key !== "Enter" || event.isComposing) return;
    if (multiline && !(event.ctrlKey || event.metaKey)) return;
    binding.flush();
  });
  input.addEventListener("blur", () => {
    binding.flush();
  });
}

function option(value: string, label: string, placeholder = false): HTMLOptionElement {
  const option = document.createElement("option");
  option.value = value;
  option.textContent = label;
  if (placeholder) option.disabled = true;
  return option;
}
