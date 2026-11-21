import type { ShowDialog, Target } from "../actions/index.ts";
export interface StubAdvanced {
    readonly id: string;
    readonly title: string;
    readonly target: Target;
    readonly el: HTMLElement;
    closed(): boolean;
    close(): void;
}
export declare function stubDialogs(): {
    readonly show: ShowDialog;
    readonly open: () => StubAdvanced | undefined;
};
