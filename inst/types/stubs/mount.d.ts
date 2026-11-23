export interface Mounted {
    readonly el: HTMLElement;
    readonly signal: AbortSignal;
    unmount(): void;
}
export declare function mount(exports: unknown, host: unknown, el?: HTMLElement): Promise<Mounted>;
export declare function renderable(exports: unknown): Readonly<Record<string, unknown>> & {
    readonly render: (...args: never[]) => unknown;
};
