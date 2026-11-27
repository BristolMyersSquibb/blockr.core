// How the checkboxes of the Edit board plugin's rows follow core.selection,
// as What it shows has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.15#what-it-shows).
// A row's checkbox shows whether the selection holds the row's object,
// whatever set the selection, such as the DAG as the user selects nodes.
// Ticking it adds the object to the selection, and unticking it takes the
// object out. A table changes only the objects of the rows that it shows, and
// leaves the rest of the selection as it was: the rows of its other pages,
// those of the other tables, and what else the selection holds.

import { useCallback, useSyncExternalStore } from "react";
import type { Selection } from "react-aria-components";
import type { BoardObject, CoreApi } from "../../types/core-api.ts";

// The kinds of board object that the plugin's tables list.
export type Kind = "block" | "link" | "stack";

// The board objects that the selection holds, which the plugin draws again as
// they change.
export function useSelected(core: Pick<CoreApi, "selection">): readonly BoardObject[] {
  const subscribe = useCallback(
    (notify: () => void) =>
      core.selection.subscribe(() => {
        notify();
      }),
    [core],
  );
  return useSyncExternalStore(subscribe, () => core.selection.get());
}

// The ids of the board objects of the kind given that the selection holds,
// which the checkboxes of a table show ticked.
export function selectedIds(selected: readonly BoardObject[], kind: Kind): ReadonlySet<string> {
  return new Set(selected.flatMap((object) => idOf(object, kind) ?? []));
}

// Selects the objects of the rows that the user ticked in a table, and takes
// those that the user unticked out of the selection, as React Aria's Table
// hands them. The rows given are those that the table shows, and "all" ticks
// every one of them, as the checkbox in the table's header does.
export function tickRows(core: Pick<CoreApi, "selection">, kind: Kind, shown: readonly string[], keys: Selection): void {
  const ticked = (id: string): boolean => keys === "all" || keys.has(id);
  const rows = new Set(shown);
  const current = core.selection.get();
  const kept = current.filter((object) => {
    const id = idOf(object, kind);
    return id === undefined || !rows.has(id) || ticked(id);
  });
  const held = selectedIds(current, kind);
  const added = shown.filter((id) => ticked(id) && !held.has(id)).map((id) => objectOf(kind, id));
  core.selection.set([...kept, ...added]);
}

// The board object of the kind given with the id given.
export function objectOf(kind: Kind, id: string): BoardObject {
  if (kind === "block") return { block: id };
  if (kind === "link") return { link: id };
  return { stack: id };
}

// The id of a board object of the kind given, or undefined for one of
// another kind.
function idOf(object: BoardObject, kind: Kind): string | undefined {
  if (kind === "block") return "block" in object ? object.block : undefined;
  if (kind === "link") return "link" in object ? object.link : undefined;
  return "stack" in object ? object.stack : undefined;
}
