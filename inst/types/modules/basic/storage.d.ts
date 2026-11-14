import type { StorageControl } from "../../store/index.ts";
import type { CoreApi } from "../../types/core-api.ts";
export type Button = Extract<StorageControl, {
    kind: "button";
}>;
export interface Drawn {
    readonly controls: readonly StorageControl[];
    readonly failed: boolean;
    readonly problem?: string;
}
export declare function drawnControls(core: CoreApi): Drawn | undefined;
export declare function buttonsOf(controls: readonly StorageControl[]): Button[];
export declare function runButton(core: Pick<CoreApi, "log">, button: Button): Promise<void>;
