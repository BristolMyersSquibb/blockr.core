import type { Scheduler } from "../scheduler/index.ts";
import type { PluginClassId } from "../types/board.ts";
import type { RequestOpts } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import type { Replied } from "../util/index.ts";
import type { RunningPlugins } from "./plugins.ts";
export interface PluginRequestDeps {
    readonly plugins: Pick<RunningPlugins, "appPlugins">;
    readonly scheduler: Pick<Scheduler, "evaluate" | "request" | "status">;
    readonly replied: Replied;
}
export declare function pluginRequest(deps: PluginRequestDeps, plugin: PluginClassId, name: string, args: Readonly<Record<string, Json>>, opts: RequestOpts): Promise<Json | Blob>;
