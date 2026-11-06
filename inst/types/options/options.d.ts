import type { Option } from "../store/index.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
export type BlockrOptions = Pick<CoreApi, "blockrOption">;
export declare function coreOptions(core: BlockrOptions): Option[];
export declare function nRows(core: BlockrOptions): Option;
export declare function showConditions(core: BlockrOptions): Option;
export declare function darkMode(core: BlockrOptions): Option;
export declare function setTheme(theme: Json): void;
