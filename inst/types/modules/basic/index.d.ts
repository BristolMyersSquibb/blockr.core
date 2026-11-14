import type { PluginClassId } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
export declare const api = 1;
declare const _default: {
    options: string[];
    frontend: {
        render: typeof render;
    };
};
export default _default;
declare function render({ el, core }: {
    el: HTMLElement;
    core: CoreApi;
    plugin: PluginClassId;
    signal: AbortSignal;
}): () => void;
