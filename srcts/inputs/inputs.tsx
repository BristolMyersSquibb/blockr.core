// What the inputs of core's React SDK share, each of which shows and edits
// one value that `useValue()` binds: a block's state arg, a board option's
// value, or a value that a function reads from a block's state, as the join
// that two state args make,
// so that each meets the contract of a block's UI as its binding does
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract):
// how an input shows a value that it lacks, the line below an input that says
// why the host refused what it submitted, and what the user typed into a
// field until its binding submits it.
// Here too are the stack of a UI's inputs and the field, a label that holds
// its input, so that it needs no id, as a block's UI may be mounted in
// several places at once.
// Each input sits in a file of its own beside this one.
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

// How an input shows a value that it lacks, such as a pick that none of its
// items holds or a number outside a slider's range.
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
