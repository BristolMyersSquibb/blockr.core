import type { ReactElement } from "react";
import type { BlockId, Board, LinkId } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { RegistryEntry } from "../types/registry.ts";
import type { Done } from "../ui/index.ts";
export type AddMode = {
    readonly kind: "add";
} | {
    readonly kind: "after";
    readonly from: BlockId;
} | {
    readonly kind: "before";
    readonly to: BlockId;
    readonly input?: string;
} | {
    readonly kind: "on";
    readonly link: LinkId;
};
export interface PickerProps {
    readonly core: Pick<CoreApi, "store" | "blockRegistry" | "commands" | "mintId">;
    readonly mode: AddMode;
    readonly done: Done;
    readonly filter?: string;
    readonly autofocus?: boolean;
}
export declare function BlockPicker({ core, mode, done, filter, autofocus }: PickerProps): ReactElement | null;
export declare function namesInto(board: Pick<Board, "links">, to: BlockId, except?: string): readonly string[];
export declare function inputNameError(value: string, taken: readonly string[]): string | undefined;
export declare function isVariadic(free: readonly string[]): boolean;
export interface ClassItem {
    readonly id: string;
    readonly name: string;
    readonly description: string;
    readonly icon: string;
    readonly pkg: string | null;
    readonly category: string;
    readonly keywords: readonly string[];
}
export declare function blockClasses(entries: readonly RegistryEntry[]): ClassItem[];
export declare function focusFirstInvalid(form: HTMLFormElement): void;
export declare function blockIdError(value: string, board: Pick<Board, "blocks">): string | undefined;
export declare function without<E extends Readonly<Record<string, string | undefined>>>(errors: E, key: keyof E): E;
