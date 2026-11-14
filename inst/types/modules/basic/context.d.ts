import type { CoreApi } from "../../types/core-api.ts";
import type { InView } from "./demand.ts";
import type { Toasts } from "./toasts.ts";
export interface FrontEnd {
    readonly core: CoreApi;
    readonly toasts: Toasts;
    readonly inView: InView;
}
export declare const FrontEndContext: import("react").Context<FrontEnd | null>;
export declare function useFrontEnd(): FrontEnd;
