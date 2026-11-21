import type { ReactElement } from "react";
import type { LinkProps as AriaLinkProps } from "../aria/index.ts";
export interface LinkProps extends AriaLinkProps {
    readonly variant?: "primary" | "secondary";
}
export declare function Link({ variant, className, ...props }: LinkProps): ReactElement;
