import type { Scheduler } from "../scheduler/index.ts";
import type { Store } from "../store/index.ts";
import type { PluginId } from "../types/board.ts";
import type { RequestOpts } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
export interface PluginRequestDeps {
    readonly store: Pick<Store, "board">;
    readonly scheduler: Pick<Scheduler, "evaluate" | "request" | "status">;
}
export declare function pluginRequest(deps: PluginRequestDeps, plugin: PluginId, name: string, args: Readonly<Record<string, Json>>, opts: RequestOpts): Promise<Json | Blob>;
