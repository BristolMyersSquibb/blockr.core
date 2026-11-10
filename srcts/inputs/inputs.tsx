// The inputs that core's UIs draw with React, each showing and editing one
// value that `useValue()` binds, a block's state arg or a board option's value,
// so that each meets the contract of a block's UI as its binding does
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract).
// An input keeps what the user types until its binding submits it,
// submits a pick at once,
// and says below itself why the host refused what it submitted.
// A label holds its input, so that it needs no id,
// as a block's UI may be mounted in several places at once.

import { useState } from "react";
import type { ReactElement, ReactNode } from "react";
import { strings } from "../binding/index.ts";
import type { Choice } from "../binding/index.ts";
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
// `max`, which submits only a number in that range and marks one outside it.
export function NumberInput({
  bound,
  label,
  min,
  max,
}: {
  bound: Bound;
  label: string;
  min?: number;
  max?: number;
}): ReactElement {
  const [text, typed] = useTyped(bound);
  const shown = text ?? (typeof bound.value === "number" ? String(bound.value) : "");
  const read = (input: string): number | undefined => {
    const value = input.trim() === "" ? Number.NaN : Number(input);
    if (!Number.isInteger(value)) return undefined;
    if (min !== undefined && value < min) return undefined;
    if (max !== undefined && value > max) return undefined;
    return value;
  };
  return (
    <>
      <Field label={label}>
        <input
          type="number"
          step={1}
          min={min}
          max={max}
          required
          className={styles.box}
          value={shown}
          aria-invalid={read(shown) === undefined}
          onChange={(event) => {
            typed(event.target.value);
            bound.type(read(event.target.value));
          }}
          onKeyDown={(event) => {
            if (event.key === "Enter" && !event.nativeEvent.isComposing) bound.flush();
          }}
          onBlur={() => {
            bound.flush();
          }}
        />
      </Field>
      <Refusal error={bound.error} />
    </>
  );
}

// A select of one string, which shows `placeholder` while the value is unset.
// It takes its choices as `undefined` until they are known, such as until a
// request answers, and shows a value meanwhile as it is.
// Once they are known, it shows a value that they lack, such as a column that
// the data no longer has, as missing, and marks itself invalid.
export function Select({
  bound,
  label,
  placeholder,
  choices,
}: {
  bound: Bound;
  label: string;
  placeholder: string;
  choices?: readonly Choice[];
}): ReactElement {
  const current = typeof bound.value === "string" ? bound.value : undefined;
  const known = choices ?? [];
  const lacked = current !== undefined && !known.some((choice) => choice.value === current);
  const invalid = lacked && choices !== undefined;
  const shown = lacked ? [...known, { value: current, label: invalid ? missing(current) : current }] : known;
  return (
    <>
      <Field label={label}>
        <select
          className={styles.box}
          value={current ?? ""}
          aria-invalid={invalid}
          onChange={(event) => {
            void bound.commit(event.target.value);
          }}
        >
          {current === undefined && (
            <option value="" disabled>
              {placeholder}
            </option>
          )}
          {shown.map((choice) => (
            <option key={choice.value} value={choice.value}>
              {choice.label}
            </option>
          ))}
        </select>
      </Field>
      <Refusal error={bound.error} />
    </>
  );
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

// A checkbox for each string that an array may hold,
// which says `empty` once its choices are known and there are none.
// It takes its choices as `undefined` until they are known.
// It shows a string the value holds and the choices lack,
// such as a column only one input has, so that the user can clear it,
// and marks it as missing once the choices are known.
// It submits the checked strings in the order the user checked them,
// after those the value holds, or where `ordered`, in the order of the
// choices, as for severities.
// It builds each submit from the value the host holds then,
// so that it keeps a string the assistant added in the meantime.
export function Checklist({
  bound,
  legend,
  choices,
  empty,
  ordered = false,
}: {
  bound: Bound;
  legend: string;
  choices?: readonly string[];
  empty?: string;
  ordered?: boolean;
}): ReactElement {
  const held = strings(bound.value);
  const known = choices ?? [];
  const shown = [...known, ...held.filter((value) => !known.includes(value))];

  const toggle = (value: string, checked: boolean): void => {
    const now = strings(bound.get());
    const next = checked
      ? now.includes(value)
        ? now
        : [...now, value]
      : now.filter((other) => other !== value);
    void bound.commit(
      ordered ? [...known.filter((choice) => next.includes(choice)), ...next.filter((other) => !known.includes(other))] : next,
    );
  };

  return (
    <fieldset className={styles.fieldset}>
      <legend className={styles.legend}>{legend}</legend>
      <div className={styles.list}>
        {shown.map((value) => {
          const invalid = choices !== undefined && !known.includes(value);
          return (
            <label key={value} className={styles.check}>
              <input
                type="checkbox"
                className={styles.checkbox}
                value={value}
                checked={held.includes(value)}
                aria-invalid={invalid}
                onChange={(event) => {
                  toggle(value, event.target.checked);
                }}
              />
              {invalid ? missing(value) : value}
            </label>
          );
        })}
        {choices !== undefined && empty !== undefined && shown.length === 0 && <p className={styles.hint}>{empty}</p>}
      </div>
      <Refusal error={bound.error} />
    </fieldset>
  );
}

// How a select or a checklist shows a value that its choices lack.
function missing(value: string): string {
  return `${value} (not available)`;
}

// Says why the host refused what an input submitted.
function Refusal({ error }: { error: string | null }): ReactElement | null {
  return error === null ? null : (
    <p role="alert" className={styles.alert}>
      {error}
    </p>
  );
}

// What the user typed into an input since it last showed the bound value,
// and the setter of it. The binding shows nothing while the user types,
// so a value it shows ends what the user typed, as the host took it or
// announced another.
function useTyped(bound: Bound): [string | null, (text: string) => void] {
  const [text, setText] = useState<string | null>(null);
  const [seen, setSeen] = useState<Json | undefined>(bound.value);
  if (!Object.is(seen, bound.value)) {
    setSeen(bound.value);
    setText(null);
  }
  return [text, setText];
}
