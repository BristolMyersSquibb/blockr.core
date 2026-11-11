import type { Loader, LoaderOptions, Measure, Source } from "../mount/index.ts";
import type { Transport } from "../protocol/index.ts";
import type { Option } from "../store/index.ts";
import type { PluginClassId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
export type Front = {
    readonly id: string;
    readonly source: Source | undefined;
    readonly plugin: PluginClassId;
} | {
    readonly none: string;
};
export interface FrontEnds {
    prepare(front: Front, loader: Loader, signal: AbortSignal): Promise<void>;
    render(core: CoreApi, signal: AbortSignal, failures: readonly string[]): Promise<void>;
    card?(core: CoreApi, signal: AbortSignal): void;
}
export interface Changed {
    readonly package: string;
    readonly loaded: string;
    readonly now: string;
}
export interface StartOptions {
    readonly transport: Transport;
    readonly loader?: Omit<LoaderOptions, "modules" | "changed">;
    readonly options: (core: Pick<CoreApi, "blockrOption">) => readonly Option[];
    readonly frontEnds?: FrontEnds;
    readonly alone?: boolean;
    readonly measure?: Measure;
    readonly reload?: (changed: readonly Changed[]) => void;
    readonly fail?: (error: Error) => void;
}
export interface Started {
    readonly ready: Promise<CoreApi>;
    core(): CoreApi | undefined;
    stop(): void;
}
export declare function start(options: StartOptions): Started;
