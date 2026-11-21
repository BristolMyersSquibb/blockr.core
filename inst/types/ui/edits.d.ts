import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
interface Listed {
    readonly id: string;
    readonly args?: Json;
}
export declare function runCommand(core: Pick<CoreApi, "commands">, id: string, args?: Json): Promise<boolean>;
export declare function runCommand(core: Pick<CoreApi, "commands">, list: readonly Listed[]): Promise<boolean>;
export interface Edits {
    readonly refusal: Refused | null;
    run(title: string, id: string, args?: Json): Promise<boolean>;
    run(title: string, list: readonly Listed[]): Promise<boolean>;
    attempt(title: string, start: (opts: {
        readonly notifier?: () => void;
    }) => Promise<unknown>): Promise<boolean>;
    clear(): void;
}
export interface Refused {
    readonly title: string;
    readonly reasons: readonly string[];
}
export declare function useEdits(core: Pick<CoreApi, "commands">, shows?: boolean): Edits;
export {};
