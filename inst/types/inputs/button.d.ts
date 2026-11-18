import type { ReactElement } from "react";
import type { ButtonProps as AriaButtonProps } from "../aria/index.ts";
import { buttons } from "../styles/index.ts";
export interface ButtonProps extends AriaButtonProps {
    readonly variant?: keyof typeof buttons;
}
export declare function Button({ variant, className, ...props }: ButtonProps): ReactElement;
