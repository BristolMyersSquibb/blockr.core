// The values of the items of a group of the SDK's inputs, the radio group,
// the checkbox group and the toggle button group, which React Aria's groups
// do not know: a group holds its items as elements, and each item reads the
// group's pick itself.
// Each item of the SDK tells the group that holds it its value as it mounts,
// and takes it back as it unmounts, so that a bound group finds a value that
// none of its items holds, which it then shows as missing, as the select does
// (./combobox.tsx).
// A group knows its items once they have told it theirs, before the browser
// paints, and until then marks nothing as missing.
// It knows them in the order of the page, in which an `ordered` group submits
// its picks.

import { createContext, useCallback, useContext, useLayoutEffect, useMemo, useRef, useState } from "react";
import type { RefObject } from "react";

export interface Items {
  // Takes the value of an item and what finds its element, and returns what
  // takes them back.
  add(value: string, element: () => Element | null): () => void;
}

// The items of the group that holds an item, or none outside a group, and
// around an item that a group draws itself for a value that it lacks.
export const ItemsContext = createContext<Items | null>(null);

// The values of the group's items, null until they told it theirs, and the
// registry that the group hands its items.
export function useItems(): { readonly known: readonly string[] | null; readonly items: Items } {
  const entries = useRef(new Map<symbol, { readonly value: string; readonly element: () => Element | null }>());
  const [known, setKnown] = useState<readonly string[] | null>(null);
  const sync = useCallback(() => {
    const next = inPageOrder([...entries.current.values()]);
    setKnown((last) => (last !== null && sameValues(last, next) ? last : next));
  }, []);
  const items = useMemo<Items>(
    () => ({
      add(value, element) {
        const key = Symbol(value);
        entries.current.set(key, { value, element });
        sync();
        return () => {
          entries.current.delete(key);
          sync();
        };
      },
    }),
    [sync],
  );
  // The group knows its items after each draw, a group of none included, and
  // their order where they moved.
  useLayoutEffect(() => {
    sync();
  });
  return { known, items };
}

// Tells the group that holds the item its value, while the item is mounted.
export function useItem(value: string | undefined, ref: RefObject<Element | null>): void {
  const items = useContext(ItemsContext);
  useLayoutEffect(() => {
    if (items === null || value === undefined) return undefined;
    return items.add(value, () => ref.current);
  }, [items, value, ref]);
}

// The values of the entries in the order of their elements on the page, those
// without an element after them, each value once.
function inPageOrder(entries: readonly { readonly value: string; readonly element: () => Element | null }[]): string[] {
  const placed = entries.map((entry) => ({ value: entry.value, element: entry.element() }));
  placed.sort((a, b) => {
    if (a.element === null || b.element === null) return a.element === null ? (b.element === null ? 0 : 1) : -1;
    return a.element.compareDocumentPosition(b.element) & Node.DOCUMENT_POSITION_FOLLOWING ? -1 : 1;
  });
  return [...new Set(placed.map((entry) => entry.value))];
}

function sameValues(a: readonly string[], b: readonly string[]): boolean {
  return a.length === b.length && a.every((value, i) => value === b[i]);
}

// The values in the order of the ids, those that the ids lack after them, as
// an `ordered` input submits its picks.
export function inOrder(values: readonly string[], order: readonly string[]): string[] {
  return [...order.filter((id) => values.includes(id)), ...values.filter((value) => !order.includes(value))];
}

// The picks of several that an input submits, built from the value that the
// host holds now: the strings that the user picked since `before`, after
// those the value holds, without those that the user dropped, or none where
// the user changed nothing.
export function merged(now: readonly string[], before: readonly string[], picks: readonly string[]): string[] | null {
  const added = picks.filter((value) => !before.includes(value));
  const removed = before.filter((value) => !picks.includes(value));
  if (added.length === 0 && removed.length === 0) return null;
  return [...now.filter((x) => !removed.includes(x)), ...added.filter((x) => !now.includes(x))];
}

// The values that the items lack, in the order of the values.
export function lacking(values: readonly string[], known: readonly string[] | null): string[] {
  return known === null ? [] : values.filter((value) => !known.includes(value));
}
