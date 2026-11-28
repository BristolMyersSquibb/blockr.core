import type { BlockId, PluginClassId } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
export declare const OWNER = "blockr.core";
export declare const SHOWN = 1;
export declare const IN_VIEW = 2;
export type View = Pick<Window, "innerWidth" | "innerHeight"> & {
    readonly IntersectionObserver?: typeof IntersectionObserver;
};
export type Watched = {
    readonly block: BlockId;
} | {
    readonly plugin: PluginClassId;
};
export interface InView {
    watch(object: Watched, el: Element): () => void;
    element(object: Watched): Element | undefined;
    measure(): void;
}
export interface Shown extends InView {
    release(): void;
}
export declare function demandShown(core: Pick<CoreApi, "scheduler" | "onScreen">, view?: View): Shown;
