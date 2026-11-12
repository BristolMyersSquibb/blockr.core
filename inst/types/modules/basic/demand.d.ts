import type { BlockId } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
export declare const OWNER = "blockr.core-basic";
export declare const SHOWN = 1;
export declare const IN_VIEW = 2;
export type View = Pick<Window, "innerWidth" | "innerHeight"> & {
    readonly IntersectionObserver?: typeof IntersectionObserver;
};
export interface InView {
    watch(block: BlockId, el: Element): () => void;
    measure(): void;
}
export interface Shown extends InView {
    release(): void;
}
export declare function demandShown(core: Pick<CoreApi, "scheduler">, view?: View): Shown;
