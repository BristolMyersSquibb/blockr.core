import type { Binding, Followed, FollowOptions } from "../binding/index.ts";
import type { BlockHost, OptionHost, State } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
export interface NumberField {
    setMaxValue(maxValue: number | undefined): void;
    setOutdated(outdated: boolean): void;
}
export interface ComboBox {
    setItems(items: readonly Item[]): void;
    setOutdated(outdated: boolean): void;
    setTotal(total: number | undefined): void;
}
export interface Item {
    readonly id: string;
    readonly name?: string;
}
export interface TextFieldOptions {
    readonly label: string;
    readonly placeholder?: string;
    readonly description?: string;
    readonly lines?: number;
}
export interface NumberFieldOptions {
    readonly label: string;
    readonly description?: string;
    readonly minValue?: number;
    readonly maxValue?: number;
    readonly step?: number;
}
export interface ComboBoxOptions {
    readonly label: string;
    readonly placeholder?: string;
    readonly items?: readonly Item[];
    readonly selectionMode?: "single" | "multiple";
    readonly ordered?: boolean;
    readonly isRequired?: boolean;
    readonly renderEmptyState?: () => string;
    readonly onSearch?: (text: string) => void;
}
export interface Controls<K extends string = string> {
    readonly el: HTMLElement;
    add(node: Node): void;
    get(key: K): Json | undefined;
    bind(key: K, show: (value: Json | undefined) => void): Binding;
    text(key: K, options: TextFieldOptions): void;
    number(key: K, options: NumberFieldOptions): NumberField;
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
