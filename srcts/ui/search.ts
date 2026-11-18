// The search of a select of core's UIs whose items the user finds by more
// than their names, such as a block by its id or a block class by a keyword.
// It keeps the items one of whose texts holds what the user typed into the
// select's field, as one text, by React Aria's filter, which ignores case and
// accents, so that "b ·" finds the block b by its detail "b · dataset block".
// The select takes them as its `items`, which React Aria leaves to its caller
// to filter, with the handlers below: the field's text, as the user types,
// and the list's opening by a press, which shows all items again, where the
// field shows the pick.
// Its list says `none` where there is no item at all, and "No matches" where
// the search keeps none.
// The props are the select's, so that it takes them all at once.

import { useState } from "react";
import { useFilter } from "../aria/index.ts";

export interface Search<T> {
  readonly items: readonly T[];
  readonly onInputChange: (text: string) => void;
  readonly onOpenChange: (isOpen: boolean, trigger?: string) => void;
  readonly renderEmptyState: () => string;
}

export function useSearch<T>(all: readonly T[], texts: (item: T) => readonly string[], none = "No options"): Search<T> {
  const { contains } = useFilter({ sensitivity: "base" });
  const [query, setQuery] = useState("");
  return {
    items: query === "" ? all : all.filter((item) => texts(item).some((text) => contains(text, query))),
    onInputChange: setQuery,
    onOpenChange: (isOpen, trigger) => {
      if (isOpen && trigger === "manual") setQuery("");
    },
    renderEmptyState: () => (all.length === 0 ? none : "No matches"),
  };
}
