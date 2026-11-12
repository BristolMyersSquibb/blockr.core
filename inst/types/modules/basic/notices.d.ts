import type { Note } from "../../types/core-api.ts";
export type Severity = Note["severity"];
export interface Notice {
    readonly id: number;
    readonly text: string;
    readonly severity: Severity;
}
export interface Notices {
    readonly list: () => readonly Notice[];
    readonly failed: (error: unknown) => void;
    readonly add: (note: Note) => void;
    readonly dismiss: (id: number) => void;
    readonly subscribe: (fn: () => void) => () => void;
}
export declare function createNotices(): Notices;
export declare function useNotices(notices: Notices): readonly Notice[];
