// The inputs that core's UIs draw with React, each showing and editing one
// value that `useValue()` binds, a block's state arg or a board option's value,
// so that each meets the contract of a block's UI as its binding does
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract).
// An input keeps what the user types until its binding submits it,
// or, for a number outside the input's range,
// until the user presses Enter or leaves it,
// submits a pick at once,
// and says below itself why the host refused what it submitted.
// A label holds its input, so that it needs no id,
// as a block's UI may be mounted in several places at once.
// The select is in ./select.tsx.
// Its list and its rows take ids, which React makes anew for each mount.

import { useState } from "react";
import type { ReactElement, ReactNode } from "react";
import { styles } from "../styles/index.ts";
import type { Bound } from "../react/index.ts";
import type { Json } from "../types/json.ts";

// The inputs of a UI, one below the other.
export function Stack({ children }: { children: ReactNode }): ReactElement {
  return <div className={styles.stack}>{children}</div>;
}

// A label that holds its input.
export function Field({ label, children }: { label: string; children: ReactNode }): ReactElement {
  return (
    <label className={styles.field}>
      <span className={styles.label}>{label}</span>
      {children}
    </label>
  );
}

// A text input for a string, or a text area where it spans lines.
export function TextInput({
  bound,
  label,
  placeholder,
  lines = 1,
}: {
  bound: Bound;
  label: string;
  placeholder?: string;
  lines?: number;
}): ReactElement {
  const [text, typed] = useTyped(bound);
  const shown = text ?? (typeof bound.value === "string" ? bound.value : "");
  const props = {
    className: styles.box,
    value: shown,
    placeholder,
    spellCheck: false,
    onChange: (event: { target: { value: string } }) => {
      typed(event.target.value);
      bound.type(event.target.value);
    },
    onKeyDown: (event: { key: string; ctrlKey: boolean; metaKey: boolean; nativeEvent: KeyboardEvent }) => {
      if (event.key !== "Enter" || event.nativeEvent.isComposing) return;
      if (lines > 1 && !(event.ctrlKey || event.metaKey)) return;
      bound.flush();
    },
    onBlur: () => {
      bound.flush();
    },
  };
  return (
    <>
      <Field label={label}>{lines > 1 ? <textarea rows={lines} {...props} /> : <input type="text" {...props} />}</Field>
      <Refusal error={bound.error} />
    </>
  );
}

// A number input for an integer, no smaller than `min` and no larger than
// `max`, which submits only a number in that range,
// and marks one outside it and says why below itself.
// As the user presses Enter or leaves it, it checks the number it shows
// against the range as it is then, since the range may have changed after
// the user typed the number, as once a request answers for newer data,
// and submits the number where it fits.
// Otherwise it shows the state again.
// It does not go back while the user types,
// since typing passes through numbers outside the range,
// such as an empty field on the way to a new number.
// While it is `outdated`, as while the request that gives `max` asks again,
// its `max` may be for older data.
// It then submits a number above it and marks none,
// but still holds to `min`.
export function NumberInput({
  bound,
  label,
  min,
  max,
  outdated = false,
}: {
  bound: Bound;
  label: string;
  min?: number;
  max?: number;
  outdated?: boolean;
}): ReactElement {
  const [text, typed] = useTyped(bound);
  const shown = text ?? (typeof bound.value === "number" ? String(bound.value) : "");
  const largest = outdated ? undefined : max;
  const read = (input: string): Read => {
    const trimmed = input.trim();
    if (trimmed === "") return {};
    const value = Number(trimmed);
    if (!Number.isInteger(value)) return { reason: `${trimmed} is not a whole number.` };
    if (min !== undefined && value < min) {
      return { reason: `${trimmed} is below the minimum of ${String(min)}.` };
    }
    if (largest !== undefined && value > largest) {
      return { reason: `${trimmed} is above the maximum of ${String(largest)}.` };
    }
    return { value };
  };
  // Ends what the user typed, as they press Enter or leave the input:
  // submits the number where it fits the range as it is now,
  // and otherwise shows the state again.
  const settle = (): void => {
    if (text === null) {
      bound.flush();
      return;
    }
    const { value } = read(text);
    bound.type(value);
    bound.flush();
    if (value === undefined) typed(null);
  };
  const checked = read(shown);
  return (
    <>
      <Field label={label}>
        <input
          type="number"
          step={1}
          min={min}
          max={largest}
          required
          className={styles.box}
          value={shown}
          aria-invalid={checked.value === undefined}
          onChange={(event) => {
            typed(event.target.value);
            bound.type(read(event.target.value).value);
          }}
          onKeyDown={(event) => {
            if (event.key === "Enter" && !event.nativeEvent.isComposing) settle();
          }}
          onBlur={settle}
        />
      </Field>
      <Refusal error={checked.reason ?? bound.error} />
    </>
  );
}

// What a number input reads from a text: the number where it fits the range,
// and otherwise why it does not, but for an empty text, which needs no saying.
interface Read {
  readonly value?: number;
  readonly reason?: string;
}

export function Checkbox({ bound, label }: { bound: Bound; label: string }): ReactElement {
  return (
    <>
      <label className={styles.check}>
        <input
          type="checkbox"
          className={styles.checkbox}
          checked={bound.value === true}
          onChange={(event) => {
            void bound.commit(event.target.checked);
          }}
        />
        {label}
      </label>
      <Refusal error={bound.error} />
    </>
  );
}

// How a select shows a value that its choices lack.
export function missing(value: string): string {
  return `${value} (not available)`;
}

// Says why the host refused what an input submitted.
export function Refusal({ error }: { error: string | null }): ReactElement | null {
  return error === null ? null : (
    <p role="alert" className={styles.alert}>
      {error}
    </p>
  );
}

// What the user typed into an input since it last showed the bound value,
// and the setter of it, which drops it given null,
// so that the input shows the bound value again.
// The binding shows nothing while the user types,
// so a value it shows ends what the user typed, as the host took it or
// announced another.
function useTyped(bound: Bound): [string | null, (text: string | null) => void] {
  const [text, setText] = useState<string | null>(null);
  const [seen, setSeen] = useState<Json | undefined>(bound.value);
  if (!Object.is(seen, bound.value)) {
    setSeen(bound.value);
    setText(null);
  }
  return [text, setText];
}
