import type { BlockId, JsonBoard, OptionId, RegistryId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { BlockOutput } from "../types/outputs.ts";
import type { RegistryEntry } from "../types/registry.ts";
export declare const PROTOCOL_VERSION = "1.0";
export type Kind = "hello" | "blockr_options" | "evaluate" | "request" | "output" | "cancel" | "forget";
export type Capability = "cancel" | "interrupt" | "progress";
export type ReplyStatus = "ok" | "error" | "cancelled" | "invalid" | "missing";
export type ClientMessage = Request[];
export type RMessage = (Reply | Progress)[];
export type Request = HelloRequest | BlockrOptionsRequest | EvaluateRequest | BlockClassRequest | BoardOptionRequest | ExtensionRequest | OutputClassRequest | RendererRequest | OutputRequest | CancelRequest | ForgetRequest;
export type Reply = HelloReply | BlockrOptionsReply | EvaluateReply | RequestReply | OutputReply | ErrorReply | CancelledReply | MissingReply;
export interface Progress {
    id: string;
    progress: {
        value?: number;
        message?: string;
    };
}
export interface HelloRequest {
    id: string;
    kind: "hello";
    protocol: string;
    capabilities: Capability[];
}
export interface HelloReply {
    id: string;
    status: "ok";
    protocol: string;
    capabilities: Capability[];
    packages: Record<string, string>;
    block_registry: Record<RegistryId, RegistryEntry>;
    startup_modules: {
        package: string;
        module: string;
        css?: string;
    }[];
    modules: Record<string, {
        module: string;
        css?: string;
    }>;
    board_classes: Record<string, {
        package: string;
        front_end: string;
        version: number;
    }>;
    renderers: Record<string, Renderer>;
    lock: Lock | null;
    board: JsonBoard;
    bound?: string[];
}
export type Renderer = {
    module: string;
    css?: string;
} & ({
    class: string;
    option?: never;
} | {
    option: OptionId;
    class?: never;
});
export interface Lock {
    open: BlockId[];
}
export interface BlockrOptionsRequest {
    id: string;
    kind: "blockr_options";
    names: string[];
}
export interface BlockrOptionsReply {
    id: string;
    status: "ok";
    values: Record<string, Json>;
}
export interface EvaluateRequest {
    id: string;
    kind: "evaluate";
    block: BlockId;
    revision: number;
    recipe: Recipe;
    params?: Record<string, Json>;
}
export interface Recipe {
    registry_id: RegistryId;
    state: Record<string, Json>;
    inputs: RecipeInput[];
}
export interface BlockRevision {
    block: BlockId;
    revision: number;
}
export type RecipeInput = (BlockRevision & {
    input: string;
}) | {
    input: string;
    block?: never;
    revision?: never;
};
export interface EvaluateReply {
    id: string;
    status: "ok" | "error" | "invalid";
    block: BlockId;
    revision: number;
    summary: string | null;
    conditions: Condition[];
    omitted: Omitted;
    stdout: string;
    stderr: string;
}
export interface Condition {
    severity: "error" | "warning" | "message";
    class: string[];
    phase?: Phase;
    message: string;
}
export type Phase = "expr" | "inputs" | "block_eval" | "result_summary" | "output" | "request" | "status";
export interface Omitted {
    warning: number;
    message: number;
}
export interface BlockClassRequest {
    id: string;
    kind: "request";
    registry_id: RegistryId;
    block: BlockId;
    name: string;
    args: Record<string, Json>;
    reads: RecipeInput[];
    options?: Record<OptionId, Json>;
    body?: Body;
    option?: never;
    extension?: never;
    output?: never;
    renderer?: never;
}
export interface BoardOptionRequest {
    id: string;
    kind: "request";
    option: OptionId;
    name: string;
    args: Record<string, Json>;
    body?: Body;
    registry_id?: never;
    extension?: never;
    output?: never;
    renderer?: never;
}
export interface ExtensionRequest {
    id: string;
    kind: "request";
    extension: string;
    name: string;
    args: Record<string, Json>;
    reads: BlockRevision[];
    body?: Body;
    registry_id?: never;
    option?: never;
    output?: never;
    renderer?: never;
}
export interface OutputClassRequest {
    id: string;
    kind: "request";
    output: BlockRevision;
    name: string;
    args: Record<string, Json>;
    options: Record<OptionId, Json>;
    params: Record<string, Json>;
    body?: Body;
    registry_id?: never;
    option?: never;
    extension?: never;
    renderer?: never;
}
export interface RendererRequest {
    id: string;
    kind: "request";
    renderer: string;
    output: BlockRevision;
    name: string;
    args: Record<string, Json>;
    options: Record<OptionId, Json>;
    params: Record<string, Json>;
    body?: Body;
    registry_id?: never;
    option?: never;
    extension?: never;
}
export type RequestReply = {
    id: string;
    status: "ok";
    value: Json;
    body?: never;
    volatile?: true;
} | {
    id: string;
    status: "ok";
    body: Body;
    value?: never;
    volatile?: true;
};
export interface OutputRequest {
    id: string;
    kind: "output";
    block: BlockId;
    revision: number;
    options: Record<OptionId, Json>;
    info: OutputInfo;
    params: Record<string, Json>;
}
export interface OutputInfo {
    width: number;
    height: number;
    pixel_ratio: number;
    bg: string;
    fg: string;
    accent: string;
    font: string;
}
export interface OutputReply {
    id: string;
    status: "ok";
    output: BlockOutput & Record<string, Json>;
    body?: Body;
}
export interface Body {
    type: string;
    name?: string;
}
export interface ErrorReply {
    id: string;
    status: "error";
    conditions: Condition[];
    omitted: Omitted;
}
export interface CancelledReply {
    id: string;
    status: "cancelled";
}
export interface MissingReply {
    id: string;
    status: "missing";
    missing: BlockRevision[];
}
export interface CancelRequest {
    kind: "cancel";
    request: string;
    id?: never;
}
export interface ForgetRequest {
    kind: "forget";
    blocks: BlockId[];
    id?: never;
}
export interface Fixture<C extends ClientMessage = ClientMessage, R extends RMessage[] = RMessage[]> {
    description: string;
    client: C;
    r: R;
}
