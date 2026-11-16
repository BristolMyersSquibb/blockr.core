import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
export declare function runCommand(core: Pick<CoreApi, "commands">, id: string, args?: Json): Promise<boolean>;
