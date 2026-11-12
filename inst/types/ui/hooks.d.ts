import type { Board } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
export declare function useBoard(core: Pick<CoreApi, "store">): Board;
export declare function useHistory(core: Pick<CoreApi, "store">): {
    canUndo: boolean;
    canRedo: boolean;
};
