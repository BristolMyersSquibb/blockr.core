import type { ReactElement } from "react";
import type { AutocompleteProps, MenuProps } from "../aria/index.ts";
export interface CommandPaletteProps<T> extends Omit<AutocompleteProps, "children">, MenuProps<T> {
    readonly isOpen: boolean;
    readonly onOpenChange: (isOpen: boolean) => void;
    readonly placeholder?: string;
}
export declare function CommandPalette<T extends object>({ isOpen, onOpenChange, placeholder, inputValue, defaultInputValue, onInputChange, filter, disableAutoFocusFirst, disableVirtualFocus, onAction, shouldCloseOnSelect, ...menu }: CommandPaletteProps<T>): ReactElement;
