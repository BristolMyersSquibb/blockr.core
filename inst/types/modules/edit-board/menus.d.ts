import type { ReactElement } from "react";
import type { BoardAction, PlaceArgs, Target } from "../../actions/index.ts";
import type { Board } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import type { Refused } from "../../ui/index.ts";
export interface Menus {
    readonly refusal: Refused | null;
    readonly shown: ReadonlyMap<string, PlaceArgs>;
    open(target: Target, key: string, button: HTMLElement): void;
    close(key: string, args: PlaceArgs): void;
    readonly back: string | null;
}
export declare function useMenus(core: CoreApi): Menus;
export declare function MenuButton({ menus, target, place, label }: {
    menus: Menus;
    target: Target;
    place: string;
    label: string;
}): ReactElement;
export declare function PlaceForm({ args, onClose }: {
    args: PlaceArgs;
    onClose: () => void;
}): ReactElement;
export declare function editTitle(board: Board, action: Pick<BoardAction, "package" | "id" | "label">, target: Target): string;
