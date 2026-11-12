import type { ReactElement } from "react";
import type { BlockId, Board } from "../types/board.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
export type AddMode = {
    readonly kind: "add";
} | {
    readonly kind: "after";
    readonly from: BlockId;
} | {
    readonly kind: "before";
    readonly to: BlockId;
};
export interface PickerProps {
    readonly core: Pick<CoreApi, "store" | "blockRegistry">;
    readonly mode: AddMode;
    readonly run: (command: string, args: Json) => Promise<boolean>;
    readonly onAdded?: () => void;
    readonly autofocus?: boolean;
}
export declare function BlockPicker({ core, mode, run, onAdded, autofocus }: PickerProps): ReactElement;
export declare function namesInto(board: Pick<Board, "links">, to: BlockId, except?: string): readonly string[];
export declare function inputNameError(value: string, taken: readonly string[]): string | undefined;
export declare function isVariadic(free: readonly string[]): boolean;
export declare function focusFirstInvalid(form: HTMLFormElement): void;
export declare function blockIdError(value: string, board: Pick<Board, "blocks">): string | undefined;
export declare function without<E extends Readonly<Record<string, string | undefined>>>(errors: E, key: keyof E): E;
