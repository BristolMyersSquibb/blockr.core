import type { ComponentType } from "react";
import type { Condition } from "../protocol/index.ts";
import type { OptionId } from "../types/board.ts";
import type { OutputHost, OutputModule } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import type { BlockOutput } from "../types/outputs.ts";
export interface ModuleProps<H> {
    readonly host: H;
    readonly signal: AbortSignal;
}
export interface ModuleOptions {
    readonly options?: readonly OptionId[];
}
export declare function reactModule<H>(Draw: ComponentType<ModuleProps<H>>, options?: ModuleOptions): ModuleOptions & {
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
    readonly options: Readonly<Record<OptionId, Json>>;
    readonly loading: boolean;
    readonly error: Condition | null;
    readonly host: OutputHost;
}
export declare function outputRenderer<O extends BlockOutput = BlockOutput, P = Json>(Draw: ComponentType<OutputProps<O, P>>, options?: ModuleOptions): OutputModule;
