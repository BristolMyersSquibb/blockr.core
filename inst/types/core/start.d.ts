import type { LoaderOptions, Measure } from "../mount/index.ts";
import type { Transport } from "../protocol/index.ts";
import type { PluginClassId } from "../types/board.ts";
import type { CoreApi, FrontendModule } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import type { OtelModule } from "../log/index.ts";
export type Front = {
    readonly frontend: FrontendModule;
    readonly plugin: PluginClassId;
    readonly args: Readonly<Record<string, Json>>;
} | {
    readonly none: string;
};
export interface FrontEnds {
    prepare(front: Front, signal: AbortSignal): void;
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
    readonly frontEnds?: FrontEnds;
    readonly alone?: boolean;
    readonly measure?: Measure;
    readonly fail?: (error: Error) => void;
    readonly otel?: () => Promise<OtelModule>;
}
export interface Started {
    readonly ready: Promise<CoreApi>;
    core(): CoreApi | undefined;
    stop(): void;
}
export declare function start(options: StartOptions): Started;
