import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
interface Listed {
    readonly id: string;
    readonly args?: Json;
}
export declare function runCommand(core: Pick<CoreApi, "commands">, id: string, args?: Json): Promise<boolean>;
export declare function runCommand(core: Pick<CoreApi, "commands">, list: readonly Listed[]): Promise<boolean>;
export interface Edits {
    readonly refusal: string | null;
    run(id: string, args?: Json, which?: string): Promise<boolean>;
    run(list: readonly Listed[], which?: string): Promise<boolean>;
    clear(): void;
}
export declare function useEdits(core: Pick<CoreApi, "commands">, shows?: boolean): Edits;
export {};
