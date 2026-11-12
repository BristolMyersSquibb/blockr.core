import type { CoreApi } from "../../types/core-api.ts";
export declare const api = 1;
declare const _default: {
    ui: {
        render({ el, core }: {
            el: HTMLElement;
            core: CoreApi;
            signal: AbortSignal;
        }): () => void;
    };
};
export default _default;
