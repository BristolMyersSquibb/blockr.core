import type { Board } from "../../types/board.ts";
import type { BoardObject, CoreApi, Notification } from "../../types/core-api.ts";
export declare const SHOWN_FOR = 5000;
export declare const FADES_FOR = 300;
export interface Toast {
    readonly notification: Notification;
    readonly fading: boolean;
}
export interface Toasts {
    readonly list: () => readonly Toast[];
    readonly shownIn: () => HTMLElement | null;
    readonly subscribe: (fn: () => void) => () => void;
    readonly showIn: (el: HTMLElement) => () => void;
    readonly close: (id: string) => void;
    readonly keep: (id: string, by: "pointer" | "focus") => void;
    readonly release: (id: string, by: "pointer" | "focus") => void;
    readonly stop: () => void;
}
export declare function createToasts(notifications: Pick<CoreApi["notifications"], "get" | "subscribe">): Toasts;
export declare function useToasts(toasts: Toasts): readonly Toast[];
export declare function useShownIn(toasts: Toasts): HTMLElement | null;
export declare function concerned(about: BoardObject | undefined, core: {
    readonly options: Pick<CoreApi["options"], "registered">;
    readonly plugins: Pick<CoreApi["plugins"], "appPlugins">;
}, board: Board): string;
export declare function packageShown(notification: Notification): string | undefined;
export declare function isReload(notification: Notification): boolean;
