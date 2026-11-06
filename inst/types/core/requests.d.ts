import type { Scheduler } from "../scheduler/index.ts";
import type { RequestOpts } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
export interface AddedDeps {
    readonly scheduler: Pick<Scheduler, "evaluate" | "request" | "status">;
}
export declare function addedRequest(deps: AddedDeps, type: string, fields: Readonly<Record<string, Json>>, opts: RequestOpts): Promise<Json | Blob>;
