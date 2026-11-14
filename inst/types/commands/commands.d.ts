import type { Lock } from "../protocol/index.ts";
import type { Delta, Origin, Store } from "../store/index.ts";
import type { Board } from "../types/board.ts";
import type { Json, JsonSchema } from "../types/json.ts";
export interface CommandContext {
    readonly lock: Lock;
    readonly mintId: (taken: (id: string) => boolean) => string;
    readonly blockrOption: (name: string) => Promise<Json | undefined>;
}
export interface Command<A extends Json = Json, P = A> {
    readonly id: string;
    readonly label: string;
    readonly description?: string;
    readonly args?: JsonSchema;
    readonly shortcut?: string;
    enabled?(board: Board, args: A, context: CommandContext): boolean;
    readonly undo?: false;
    prepare?(board: Board, args: A, context: CommandContext): Promise<P>;
    run(board: Board, args: P, context: CommandContext): Delta | readonly Delta[];
}
export interface Commands {
    register<A extends Json, P = A>(command: Command<A, P>): () => void;
    registered(): Readonly<Record<string, Command<Json, unknown>>>;
    enabled(id: string, args?: Json): boolean;
    run(id: string, args?: Json, origin?: Origin): Promise<void>;
    run(list: readonly {
        readonly id: string;
        readonly args?: Json;
    }[], origin?: Origin): Promise<void>;
    plan(id: string, args?: Json): Promise<Delta[]>;
    plan(list: readonly {
        readonly id: string;
        readonly args?: Json;
    }[]): Promise<Delta[]>;
}
export declare class CommandError extends Error {
    readonly failures: readonly string[];
    readonly index?: number;
    readonly enabled?: false;
    constructor(message: string, failures: readonly string[], options?: ErrorOptions & {
        readonly index?: number;
        readonly enabled?: false;
    });
}
export declare function refusal(id: string, failures: readonly string[], options?: {
    readonly enabled?: false;
}): CommandError;
export interface CommandsOptions {
    readonly signal?: AbortSignal;
    readonly lock?: Lock;
    readonly randId?: Json | undefined;
    readonly blockrOption?: (name: string) => Promise<Json | undefined>;
}
export declare function createCommands(store: Store, options?: CommandsOptions): Commands;
