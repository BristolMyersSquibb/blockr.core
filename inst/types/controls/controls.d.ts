import type { Binding, Choice, Followed, FollowOptions } from "../binding/index.ts";
import type { BlockHost, OptionHost, State } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
export interface NumberInput {
    setMax(max: number | undefined): void;
    setOutdated(outdated: boolean): void;
}
export interface Select {
    setChoices(choices: readonly Choice[]): void;
    setOutdated(outdated: boolean): void;
}
export interface SelectOptions {
    readonly label: string;
    readonly placeholder?: string;
    readonly choices?: readonly Choice[];
    readonly multiple?: boolean;
    readonly ordered?: boolean;
    readonly empty?: string;
}
export interface Controls<K extends string = string> {
    readonly el: HTMLElement;
    add(node: Node): void;
    get(key: K): Json | undefined;
    bind(key: K, show: (value: Json | undefined) => void): Binding;
    text(key: K, options: {
        label: string;
        placeholder?: string;
        lines?: number;
    }): void;
    number(key: K, options: {
        label: string;
        min?: number;
    }): NumberInput;
    select(key: K, options: SelectOptions): Select;
    checkbox(key: K, options: {
        label: string;
    }): void;
    follow(name: string, options: FollowOptions): Followed;
    alert(error: unknown): void;
    readonly cleanup: () => void;
}
export declare function controls<S extends State>(host: BlockHost<S>, signal: AbortSignal): Controls<Extract<keyof S, string>>;
export declare function controls<V extends Json>(host: OptionHost<V>, signal: AbortSignal): Controls;
