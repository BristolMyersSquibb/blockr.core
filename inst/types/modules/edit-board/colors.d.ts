import type * as Colors from "../../colors/index.ts";
import type { CoreApi } from "../../types/core-api.ts";
export type ColorsModule = typeof Colors;
export declare function useColors(core: CoreApi): ColorsModule | null;
