// What the inputs of core's React SDK share, each of which shows and edits
// one value that `useValue()` binds: a block's state arg, a board option's
// value, or a value that a function reads from a block's state, as the join
// that two state args make,
// so that each meets the contract of a block's UI as its binding does
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract):
// the line below an input that names the values it lacks, the line that says
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
import { Button } from "./button.tsx";
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

// Says below an input which of its values it lacks, such as picks that none
// of its items holds or a number outside a slider's range, by their ids or
// as numbers, as "Not available: wt", since no item gives them a text.
// The input shows such a value as it is wherever it shows its value, and
// marks itself invalid.
// Where no item shows such a value, as in a group of several or a Select of
// several, the line offers a button, Remove, through which the user drops
// them, `onRemove`.
export function NotAvailable({
  values,
  onRemove,
}: {
  readonly values: readonly string[];
  readonly onRemove?: () => void;
}): ReactElement | null {
  if (values.length === 0) return null;
  const named = values.join(", ");
  return (
    <div className="bc:flex bc:flex-wrap bc:items-center bc:gap-x-2">
      <p role="alert" className={styles.alert}>{`Not available: ${named}`}</p>
      {onRemove === undefined ? null : (
        <Button variant="quiet" aria-label={`Remove ${named}`} onPress={onRemove}>
          Remove
        </Button>
      )}
    </div>
  );
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
