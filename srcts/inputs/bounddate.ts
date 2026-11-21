// How a bound date or time input of the SDK shows and submits its value, a
// date, a time or a range, which `read` reads from the value of a block's
// state arg or of a board option, and `write` writes, as ./dates.ts lays out.
// Typing into the input's segments is an edit, which the input keeps and
// submits once the user leaves the input or presses Enter.
// React Aria changes the value with every keystroke into a segment, as the
// year goes from 2 to 20, 202 and 2026, and each of these is a date.
// So the input submits nothing as the user pauses, unlike the SDK's text
// field.
// A pick, as of a day in a calendar, it submits at once, and so a step of a
// segment by a key, as by the arrow up, as the SDK's number field submits a
// step of its number.
// React Aria takes the keys that step a segment for its own, so the input
// hears each key before React Aria does, through heard().
// It submits only a value that `fits` takes, as one in the input's range, and
// a value that does not fit gives way to the state again.
// An empty value unsets the state arg, as the select of one does once the
// user clears it.
// Where the input `isRequired`, an empty value gives way to the state again
// too.
// While the user edits, the input shows the edit, whatever the host
// announces, and the edit then replaces the value that the host announced, as
// the later of two edits.
// Once it submitted the edit, it shows the edit until the binding shows a
// value again, or until the host refuses it.
// It shows as missing a value that `read` reads as none, as the select shows a
// value that its items lack.
// Where the input unmounts while the user edits, the edit goes with it, as
// the picks of the select of several go with it where it unmounts while its
// list is open (./combobox.tsx).

import { useRef, useState } from "react";
import type { Bound } from "../react/index.ts";
import type { Json } from "../types/json.ts";
import { jsonEqual } from "../util/index.ts";
import { shownValue } from "./dates.ts";

export interface BoundDate<V> {
  // The value that the input shows, or null for none.
  readonly value: V | null;
  // The state's value, as the input shows it as missing, where `read` reads
  // it as none, or null.
  readonly missing: string | null;
  // Hears a key that the user presses in the input's segments, before React
  // Aria changes the value by it, which an element of the input calls.
  readonly heard: (key: string) => void;
  // Takes a change of the input's segments: a step, after a key that steps a
  // segment, which it submits at once, and otherwise an edit, which it
  // submits through flush().
  change(value: V | null): void;
  // Submits a pick at once.
  pick(value: V | null): void;
  // Submits the edit, as the user leaves the input or presses Enter.
  flush(): void;
}

export interface BoundDateOptions<V> {
  readonly read: (value: Json) => V | undefined;
  readonly write: (value: V) => Json;
  readonly fits: (value: V) => boolean;
  readonly required: boolean;
}

// The keys with which React Aria's spin button steps a segment of a date or a
// time: the arrows up and down, Page Up and Page Down, Home and End.
const STEPS: ReadonlySet<string> = new Set(["ArrowUp", "ArrowDown", "PageUp", "PageDown", "Home", "End"]);

// The value that the input shows in place of the state: the edit of the
// user, until the user leaves the input, or the value that it submitted, until
// the binding shows a value again.
interface Kept<V> {
  readonly value: V | null;
  readonly submitted: boolean;
}

export function useBoundDate<V>(bound: Bound, { read, write, fits, required }: BoundDateOptions<V>): BoundDate<V> {
  const held = bound.value === undefined ? null : read(bound.value);
  const [kept, setKept] = useState<Kept<V> | null>(null);
  // The edit as the events of the input read it, which may come before React
  // draws the input again.
  // It holds a value that the input submitted until the next edit, and
  // flush() submits that value no second time.
  const latest = useRef<Kept<V> | null>(null);
  const keep = (next: Kept<V> | null): void => {
    latest.current = next;
    setKept(next);
  };
  // Whether the key that the user pressed last steps a segment.
  const stepped = useRef(false);
  const [seen, setSeen] = useState(bound.value);
  if (!Object.is(seen, bound.value)) {
    setSeen(bound.value);
    if (kept?.submitted === true) setKept(null);
  }

  const submit = (value: V | null): void => {
    const next = value === null ? undefined : write(value);
    if (value === null ? required : !fits(value)) {
      keep(null);
      return;
    }
    if (jsonEqual(next, bound.value)) {
      keep(null);
      return;
    }
    keep({ value, submitted: true });
    void (next === undefined ? bound.unset() : bound.commit(next)).then((took) => {
      if (!took) keep(null);
    });
  };

  return {
    value: kept === null ? (held ?? null) : kept.value,
    missing: kept === null && bound.value !== undefined && held === undefined ? shownValue(bound.value) : null,
    heard: (key) => {
      stepped.current = STEPS.has(key);
    },
    change: (value) => {
      if (stepped.current) {
        stepped.current = false;
        submit(value);
      } else {
        keep({ value, submitted: false });
      }
    },
    pick: submit,
    flush: () => {
      const edit = latest.current;
      if (edit !== null && !edit.submitted) submit(edit.value);
    },
  };
}
