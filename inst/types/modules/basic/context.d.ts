import type { CoreApi } from "../../types/core-api.ts";
import type { Json } from "../../types/json.ts";
import type { InView } from "./demand.ts";
import type { Notices } from "./notices.ts";
export interface FrontEnd {
    readonly core: CoreApi;
    readonly notices: Notices;
    readonly inView: InView;
}
export declare const FrontEndContext: import("react").Context<FrontEnd | null>;
export declare function useFrontEnd(): FrontEnd;
export declare function runCommand(front: Pick<FrontEnd, "core" | "notices">, id: string, args?: Json): Promise<boolean>;
