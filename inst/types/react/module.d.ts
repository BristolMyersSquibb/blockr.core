import type { ComponentType } from "react";
import type { Condition } from "../protocol/index.ts";
import type { CoreFailure } from "../types/core-api.ts";
import type { OutputHost, OutputModule } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { BlockOutput } from "../types/outputs.ts";
export interface ModuleProps<H> {
    readonly host: H;
    readonly signal: AbortSignal;
}
export declare function reactModule<H>(Draw: ComponentType<ModuleProps<H>>): {
    render(args: {
        el: HTMLElement;
        host: H;
        signal: AbortSignal;
    }): () => void;
};
export interface OutputProps<O extends BlockOutput = BlockOutput, P = Json> {
    readonly output: O;
    readonly body: Blob | null;
    readonly params: P;
    readonly loading: boolean;
    readonly error: Condition | CoreFailure | null;
    readonly host: OutputHost;
}
export declare function outputRenderer<O extends BlockOutput = BlockOutput, P = Json>(Draw: ComponentType<OutputProps<O, P>>): OutputModule;
