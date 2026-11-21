import type { ReactElement, RefAttributes } from "react";
import type { ButtonProps as AriaButtonProps } from "../aria/index.ts";
import { buttons } from "../styles/index.ts";
export interface ButtonProps extends AriaButtonProps, RefAttributes<HTMLButtonElement> {
    readonly variant?: keyof typeof buttons;
    readonly "aria-keyshortcuts"?: string;
}
export declare function Button({ variant, className, "aria-keyshortcuts": shortcuts, ...props }: ButtonProps): ReactElement;
export declare function FieldButton({ className, ...props }: AriaButtonProps): ReactElement;
