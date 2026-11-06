import type { Loader, LoaderOptions, Measure, Source } from "../mount/index.ts";
import type { Transport } from "../protocol/index.ts";
import type { Option } from "../store/index.ts";
import type { CoreApi } from "../types/core-api.ts";
export interface FrontEnds {
    prepare(front: {
        readonly id: string;
        readonly source: Source | undefined;
    } | {
        readonly error: Error;
    }, loader: Loader): Promise<void>;
    render(core: CoreApi): Promise<void>;
}
export interface Changed {
    readonly package: string;
    readonly loaded: string;
    readonly now: string;
}
export interface StartOptions {
    readonly transport: Transport;
    readonly loader?: Omit<LoaderOptions, "modules">;
    readonly options: (core: Pick<CoreApi, "blockrOption">) => readonly Option[];
    readonly frontEnds?: FrontEnds;
    readonly measure?: Measure;
    readonly reload?: (changed: readonly Changed[]) => void;
    readonly fail?: (error: Error) => void;
}
export interface Started {
    readonly ready: Promise<CoreApi>;
    stop(): void;
}
export declare function start(options: StartOptions): Started;
