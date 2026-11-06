import type { Rendered } from "../types/hosts.ts";
export interface Mountable<A> {
    readonly options?: readonly string[];
    render(args: A): Rendered;
}
export declare function moduleOf<A>(exports: unknown, module: string, pkg: string | null): Mountable<A>;
export declare function showError(el: HTMLElement, message: string): void;
export declare function attempt(fn: () => void): void;
export declare function onAbort(signal: AbortSignal, fn: () => void): () => void;
export declare const REPLACED = "Another board opened, so core mounts nothing more of the board before.";
export declare function announceTo(listeners: Iterable<() => void>): void;
