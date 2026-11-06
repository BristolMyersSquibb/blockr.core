import type { CoreApi } from "../types/core-api.ts";
import type { FrontEnds } from "./start.ts";
export declare function pageFrontEnds(el: HTMLElement): FrontEnds;
export declare function scoped(core: CoreApi): {
    core: CoreApi;
    release: () => void;
};
