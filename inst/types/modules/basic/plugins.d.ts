import type { ReactElement } from "react";
import type { PluginClassId } from "../../types/board.ts";
import type { CoreApi, Plugin } from "../../types/core-api.ts";
export declare function placed(core: Pick<CoreApi, "plugins">): readonly (readonly [PluginClassId, Plugin])[];
export declare function Plugins(): ReactElement | null;
