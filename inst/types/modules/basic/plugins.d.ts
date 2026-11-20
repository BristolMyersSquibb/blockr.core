import type { ReactElement } from "react";
import type { PluginClassId } from "../../types/board.ts";
import type { CoreApi, Plugin, Severity } from "../../types/core-api.ts";
export interface Sections {
    readonly collapsed: (id: string) => boolean;
    readonly collapse: (id: string, collapsed: boolean) => void;
    readonly subscribe: (fn: () => void) => () => void;
}
export declare function createSections(): Sections;
export declare function hasSection(core: Pick<CoreApi, "plugins">, plugin: PluginClassId): boolean;
export declare function placed(core: Pick<CoreApi, "plugins">): readonly (readonly [PluginClassId, Plugin])[];
export declare function Plugins({ severities }: {
    severities: readonly Severity[];
}): ReactElement | null;
