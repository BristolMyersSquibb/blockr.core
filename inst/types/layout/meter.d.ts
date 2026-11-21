import type { ReactElement } from "react";
import type { MeterProps as AriaMeterProps, ProgressBarProps as AriaProgressBarProps } from "../aria/index.ts";
export interface MeterProps extends AriaMeterProps {
    readonly label?: string;
}
export declare function Meter({ label, className, ...props }: MeterProps): ReactElement;
export interface ProgressBarProps extends AriaProgressBarProps {
    readonly label?: string;
}
export declare function ProgressBar({ label, className, ...props }: ProgressBarProps): ReactElement;
