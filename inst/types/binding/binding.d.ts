import type { OptionId } from "../types/board.ts";
import type { BlockHost, OptionHost, State } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
export declare const PAUSE = 500;
export interface Binding {
    get(): Json | undefined;
    commit(value: Json): Promise<boolean>;
    type(value: Json | undefined): void;
    flush(): void;
    release(): void;
}
export interface BindOptions {
    readonly settled?: (error: unknown) => void;
}
export interface FollowOptions {
    readonly args?: () => Readonly<Record<string, Json>>;
    readonly reads?: () => Json;
    readonly answer: (answer: Json | Blob) => void;
    readonly failed?: (error: unknown) => void;
}
export interface Followed {
    refresh(): void;
    release(): void;
}
export declare function bind<S extends State>(host: BlockHost<S>, key: Extract<keyof S, string>, show: (value: Json | undefined) => void, options?: BindOptions): Binding;
export declare function bind<V extends Json>(host: OptionHost<V>, key: OptionId, show: (value: Json | undefined) => void, options?: BindOptions): Binding;
export declare function follow(host: BlockHost | OptionHost, name: string, options: FollowOptions): Followed;
export interface Choice {
    readonly value: string;
    readonly label: string;
}
export declare function choicesOf(values: readonly string[]): readonly Choice[];
export declare function strings(value: unknown): readonly string[];
