import type { PluginClassId } from "../../types/board.ts";
import type { CoreApi } from "../../types/core-api.ts";
import type { InView } from "./demand.ts";
import type { Sections } from "./plugins.tsx";
import type { Toasts } from "./toasts.ts";
export interface FrontEnd {
    readonly core: CoreApi;
    readonly toasts: Toasts;
    readonly inView: InView;
    readonly sections: Sections<PluginClassId>;
    readonly groups: Sections;
}
export declare const FrontEndContext: import("react").Context<FrontEnd | null>;
export declare function useFrontEnd(): FrontEnd;
