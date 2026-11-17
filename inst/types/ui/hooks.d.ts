import type { CoreApi } from "../types/core-api.ts";
export declare function useHistory(core: Pick<CoreApi, "store">): {
    canUndo: boolean;
    canRedo: boolean;
};
