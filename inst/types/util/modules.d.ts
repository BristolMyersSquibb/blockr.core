import type { Rendered } from "../types/hosts.ts";
export declare function loadedModule<A extends {
    readonly signal: AbortSignal;
}>(load: () => Promise<{
    render(args: A): Rendered;
}>): {
    render(args: A): Promise<(() => void) | undefined>;
};
