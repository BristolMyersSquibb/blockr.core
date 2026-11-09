import type { Lock } from "../protocol/index.ts";
import type { Delta, Origin, Store } from "../store/index.ts";
import type { Board } from "../types/board.ts";
import type { Json, JsonSchema } from "../types/json.ts";
export interface Command<A extends Json = Json, P = A> {
    readonly id: string;
    readonly label: string;
    readonly description?: string;
    readonly args?: JsonSchema;
    readonly shortcut?: string;
    enabled?(board: Board, args: A): boolean;
    prepare?(board: Board, args: A): Promise<P>;
    run(board: Board, args: P): Delta | readonly Delta[];
}
export interface Commands {
    register<A extends Json, P = A>(command: Command<A, P>): () => void;
    registered(): Readonly<Record<string, Command<Json, unknown>>>;
    enabled(id: string, args?: Json): boolean;
    run(id: string, args?: Json, origin?: Origin): Promise<void>;
    plan(id: string, args?: Json): Promise<Delta[]>;
}
export declare class CommandError extends Error {
    readonly failures: readonly string[];
    constructor(message: string, failures: readonly string[], options?: ErrorOptions);
}
export declare function refusal(id: string, failures: readonly string[]): CommandError;
export interface CommandsOptions {
    readonly signal?: AbortSignal;
    readonly lock?: Lock | null;
    readonly randId?: Json | undefined;
}
export declare function createCommands(store: Store, options?: CommandsOptions): Commands;
