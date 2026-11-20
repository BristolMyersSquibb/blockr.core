// The inputs that core's UIs draw with React, each showing and editing one
// value that `useValue()` binds, a block's state arg or a board option's value,
// so that each meets the contract of a block's UI as its binding does
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract).
// Each submits a pick at once and says below itself why the host refused what
// it submitted.
// A label holds its input, so that it needs no id,
// as a block's UI may be mounted in several places at once.
// The select is in ./combobox.tsx, and the text and number fields, which keep
// what the user types until their binding submits it, are in ./textfield.tsx
// and ./numberfield.tsx.
// React Aria gives their parts ids, which React makes anew for each mount.

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

// A checkbox, which binds a boolean given `bound`, and otherwise checks for
// its caller, through `isSelected` and `onChange`, as React Aria's Checkbox
// names them.
export interface CheckboxProps {
  readonly label: string;
  readonly bound?: Bound;
  readonly isSelected?: boolean;
  readonly onChange?: (isSelected: boolean) => void;
}

export function Checkbox({ bound, label, isSelected = false, onChange }: CheckboxProps): ReactElement {
  return (
    <>
      <label className={styles.check}>
        <input
          type="checkbox"
          className={styles.checkbox}
          checked={bound === undefined ? isSelected : bound.value === true}
          onChange={(event) => {
            if (bound !== undefined) void bound.commit(event.target.checked);
            onChange?.(event.target.checked);
          }}
        />
        {label}
      </label>
      {bound === undefined ? null : <Refusal error={bound.error} />}
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
export function useTyped(bound: Bound): [string | null, (text: string | null) => void] {
  const [text, setText] = useState<string | null>(null);
  const [seen, setSeen] = useState<Json | undefined>(bound.value);
  if (!Object.is(seen, bound.value)) {
    setSeen(bound.value);
    setText(null);
  }
  return [text, setText];
}
