import type { ReactElement, ReactNode } from "react";
import type { DialogProps, ModalOverlayProps } from "../aria/index.ts";
export declare function Modal({ className, ...props }: ModalOverlayProps): ReactElement;
export declare function Dialog({ className, ...props }: DialogProps): ReactElement;
export interface AlertDialogProps extends Omit<DialogProps, "children"> {
    readonly title: string;
    readonly children: ReactNode;
    readonly variant?: "info" | "destructive";
    readonly actionLabel: string;
    readonly cancelLabel?: string;
    readonly onAction?: () => void;
}
export declare function AlertDialog({ title, variant, actionLabel, cancelLabel, onAction, children, ...props }: AlertDialogProps): ReactElement;
