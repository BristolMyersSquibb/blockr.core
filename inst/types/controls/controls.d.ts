import type { Binding, Followed, FollowOptions } from "../binding/index.ts";
import type { BlockHost, OptionHost, State } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
export interface NumberInput {
    setMax(max: number | undefined): void;
    setOutdated(outdated: boolean): void;
}
export interface ComboBox {
    setItems(items: readonly Item[]): void;
    setOutdated(outdated: boolean): void;
}
export interface Item {
    readonly id: string;
    readonly name?: string;
}
export interface ComboBoxOptions {
    readonly label: string;
    readonly placeholder?: string;
    readonly items?: readonly Item[];
    readonly selectionMode?: "single" | "multiple";
    readonly ordered?: boolean;
    readonly isRequired?: boolean;
    readonly renderEmptyState?: () => string;
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
    comboBox(key: K, options: ComboBoxOptions): ComboBox;
    checkbox(key: K, options: {
        label: string;
    }): void;
    follow(name: string, options: FollowOptions): Followed;
    alert(error: unknown): void;
    readonly cleanup: () => void;
}
export declare function controls<S extends State>(host: BlockHost<S>, signal: AbortSignal): Controls<Extract<keyof S, string>>;
export declare function controls<V extends Json>(host: OptionHost<V>, signal: AbortSignal): Controls;
