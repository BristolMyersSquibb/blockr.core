export interface Mounted {
    readonly el: HTMLElement;
    readonly signal: AbortSignal;
    unmount(): void;
}
export declare function mount(exports: unknown, host: unknown, el?: HTMLElement): Promise<Mounted>;
