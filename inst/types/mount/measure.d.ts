import type { OutputInfo } from "../protocol/index.ts";
export interface Measure {
    info(el: HTMLElement): OutputInfo | null;
    watch(el: HTMLElement, fn: () => void): () => void;
}
export declare function domMeasure(): Measure;
export declare function visibleBackground(el: HTMLElement): string | null;
export declare function asHex(colour: string): string | null;
