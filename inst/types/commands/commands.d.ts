import type { Lock } from "../protocol/index.ts";
import type { Delta, Origin, Store } from "../store/index.ts";
import type { Board, CommandId } from "../types/board.ts";
import type { Notification } from "../types/core-api.ts";
import type { Json, JsonSchema } from "../types/json.ts";
export interface CommandContext {
    readonly lock: Lock;
    readonly mintId: (taken: (id: string) => boolean) => string;
    readonly blockrOption: (name: string) => Promise<Json | undefined>;
    readonly refusal: (failures: readonly string[]) => CommandError;
}
export interface Command<A extends Json = Json, P = A> {
    readonly id: string;
    readonly label: string;
    readonly description?: string;
    readonly args?: JsonSchema;
    readonly shortcut?: string;
    preflight?(board: Board, args: A, context: CommandContext): boolean;
    readonly undo?: false;
    prepare?(board: Board, args: A, context: CommandContext): Promise<P>;
    run(board: Board, args: P, context: CommandContext): Delta | readonly Delta[];
}
export interface Commands {
    register<A extends Json, P = A>(pkg: string, command: Command<A, P>): () => void;
    registered(): ReadonlyMap<CommandId, Command<Json, unknown>>;
    preflight(id: CommandId, args?: Json): boolean;
    run(id: CommandId, args?: Json, origin?: Origin, opts?: {
        notifier?(notifications: readonly Notification[]): void;
    }): Promise<boolean>;
    run(batch: readonly CommandCall[], origin?: Origin, opts?: {
        notifier?(notifications: readonly Notification[]): void;
    }): Promise<boolean>;
    plan(id: CommandId, args?: Json): Promise<Delta[]>;
    plan(batch: readonly CommandCall[]): Promise<Delta[]>;
}
export interface CommandCall {
    readonly id: CommandId;
    readonly args?: Json;
}
export declare class CommandError extends Error {
    readonly command?: CommandId;
    readonly by: "registry" | "preflight" | "command" | "code" | "store";
    readonly failures: readonly string[];
    readonly index?: number;
    constructor(fields: Pick<CommandError, "command" | "by" | "failures" | "index"> & {
        readonly cause?: unknown;
    });
}
export declare function refusal(id: CommandId, failures: readonly string[], by?: "registry" | "preflight" | "command"): CommandError;
export interface CommandsOptions {
    readonly signal?: AbortSignal;
    readonly lock?: Lock;
    readonly randId?: Json | undefined;
    readonly blockrOption?: (name: string) => Promise<Json | undefined>;
}
export declare function createCommands(store: Store, options?: CommandsOptions): Commands;
export declare function isLogged(error: unknown): boolean;
