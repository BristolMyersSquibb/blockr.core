import type { CoreApi } from "../../types/core-api.ts";
import type { Json } from "../../types/json.ts";
export declare function runCommand(core: Pick<CoreApi, "commands" | "notifications">, id: string, args?: Json): Promise<boolean>;
