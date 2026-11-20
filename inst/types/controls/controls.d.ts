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
export interface BlockControls<S extends State = State> extends Controls<Extract<keyof S, string>> {
    bind(key: Extract<keyof S, string>, show: Show): Binding;
    bind(read: Read<S>, change: Change<S>, show: Show): Binding;
    text(key: Extract<keyof S, string>, options: TextFieldOptions): void;
    text(read: Read<S>, change: Change<S>, options: TextFieldOptions): void;
    number(key: Extract<keyof S, string>, options: NumberFieldOptions): NumberField;
    number(read: Read<S>, change: Change<S>, options: NumberFieldOptions): NumberField;
    comboBox(key: Extract<keyof S, string>, options: ComboBoxOptions): ComboBox;
    comboBox(read: Read<S>, change: Change<S>, options: ComboBoxOptions): ComboBox;
    checkbox(key: Extract<keyof S, string>, options: {
        label: string;
    }): void;
    checkbox(read: Read<S>, change: Change<S>, options: {
        label: string;
    }): void;
}
type Read<S extends State = State> = (state: S) => Json | undefined;
type Change<S extends State = State> = (value: Json | undefined, state: S) => Partial<S>;
type Show = (value: Json | undefined) => void;
export declare function controls<S extends State>(host: BlockHost<S>, signal: AbortSignal): BlockControls<S>;
export declare function controls<V extends Json>(host: OptionHost<V>, signal: AbortSignal): Controls;
export {};
