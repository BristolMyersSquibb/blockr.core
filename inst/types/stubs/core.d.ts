import type { Measure } from "../mount/index.ts";
import type { EvaluateReply, EvaluateRequest, HelloReply, OutputInfo, OutputRequest, Request } from "../protocol/index.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import type { BlockOutput } from "../types/outputs.ts";
export declare const HELLO: HelloReply;
export declare const INFO: OutputInfo;
export type Answer = (request: Request, body: Blob | undefined) => Json | Blob | undefined | Promise<Json | Blob | undefined>;
export interface StubCoreOptions {
    readonly hello?: HelloReply;
    readonly blockrOptions?: Readonly<Record<string, Json>>;
    readonly modules?: Readonly<Record<string, unknown>>;
    readonly evaluate?: (request: EvaluateRequest) => Partial<Omit<EvaluateReply, "id" | "block" | "revision">> | undefined;
    readonly output?: (request: OutputRequest) => {
        output: BlockOutput & Record<string, Json>;
        body?: Blob;
    } | undefined;
    readonly answer?: Answer;
    readonly el?: HTMLElement;
    readonly measure?: Measure;
    readonly refuse?: readonly Request["kind"][];
}
export interface StubCore {
    readonly core: CoreApi;
    readonly sent: readonly Request[];
    readonly settled: () => Promise<void>;
    readonly stop: () => void;
}
export declare function stubCore(options?: StubCoreOptions): Promise<StubCore>;
