import type { ReactElement, ReactNode } from "react";
import type { DisclosurePanelProps, DisclosureProps } from "../aria/index.ts";
export declare function Disclosure({ className, ...props }: DisclosureProps): ReactElement;
export interface DisclosureHeaderProps {
    readonly children: ReactNode;
    readonly level?: number;
    readonly className?: string;
}
export declare function DisclosureHeader({ children, level, className }: DisclosureHeaderProps): ReactElement;
export declare function DisclosurePanel({ className, ...props }: DisclosurePanelProps): ReactElement;
