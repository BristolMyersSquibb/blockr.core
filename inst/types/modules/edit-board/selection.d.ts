import type { Selection } from "../../aria/index.ts";
import type { BoardObject, CoreApi } from "../../types/core-api.ts";
export type Kind = "block" | "link" | "stack";
export declare function useSelected(core: Pick<CoreApi, "selection">): readonly BoardObject[];
export declare function selectedIds(selected: readonly BoardObject[], kind: Kind): ReadonlySet<string>;
export declare function tickRows(core: Pick<CoreApi, "selection">, kind: Kind, shown: readonly string[], keys: Selection): void;
export declare function objectOf(kind: Kind, id: string): BoardObject;
