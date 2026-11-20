import type { ReactElement } from "react";
import type { ToastProps, ToastRegionProps } from "../aria/index.ts";
export { UNSTABLE_ToastContent as ToastContent, UNSTABLE_ToastQueue as ToastQueue } from "../aria/index.ts";
export type { QueuedToast } from "../aria/index.ts";
export declare function ToastRegion<T>({ className, ...props }: ToastRegionProps<T>): ReactElement;
export declare function Toast<T>({ className, style, ...props }: ToastProps<T>): ReactElement;
