import type { ReactElement, Ref } from "react";
import type { TokenFieldProps as AriaTokenFieldProps, TokenFieldValue, TokenInputProps, TokenProps } from "../aria/index.ts";
export interface TokenFieldProps<T extends TokenFieldValue = TokenFieldValue> extends Omit<AriaTokenFieldProps<T>, "children"> {
    readonly label?: string;
    readonly description?: string;
    readonly inputRef?: Ref<HTMLDivElement>;
    readonly children: TokenInputProps["children"];
}
export declare function TokenField<T extends TokenFieldValue = TokenFieldValue>({ label, description, className, inputRef, children, ...props }: TokenFieldProps<T>): ReactElement;
export declare function Token({ className, ...props }: TokenProps): ReactElement;
