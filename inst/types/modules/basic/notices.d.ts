import type { CoreApi, Notification } from "../../types/core-api.ts";
export type Severity = Notification["severity"];
export interface Notice {
    readonly key: string;
    readonly text: string;
    readonly severity: Severity;
    readonly notification?: Notification;
}
export interface Notices {
    readonly list: () => readonly Notice[];
    readonly failed: (error: unknown) => void;
    readonly dismiss: (key: string) => void;
    readonly subscribe: (fn: () => void) => () => void;
    readonly stop: () => void;
}
export declare function createNotices(notifications: CoreApi["notifications"]): Notices;
export declare function useNotices(notices: Notices): readonly Notice[];
