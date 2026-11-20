import type { ReactElement } from "react";
import type { ButtonProps as AriaButtonProps } from "../aria/index.ts";
import { buttons } from "../styles/index.ts";
export interface ButtonProps extends AriaButtonProps {
    readonly variant?: keyof typeof buttons;
    readonly "aria-keyshortcuts"?: string;
}
export declare function Button({ variant, className, "aria-keyshortcuts": shortcuts, ...props }: ButtonProps): ReactElement;
