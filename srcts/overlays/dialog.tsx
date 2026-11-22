// The modal and the dialog of core's React SDK: React Aria's ModalOverlay and
// Modal (https://react-aria.adobe.com/Modal), drawn as one, and React Aria's
// Dialog (https://react-aria.adobe.com/Dialog), in core's theme, after the
// Modal and the Dialog of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Modal.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Dialog.tsx),
// and the dialog that asks, the starter kit's AlertDialog
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/AlertDialog.tsx).
// Each takes the props of its part of React Aria.
// The modal covers the page with a backdrop and holds the dialog in the
// middle of the window.
// React Aria draws it in a portal at the end of the page's body, keeps the
// focus inside it and makes the rest of the page inert while it is open, as
// the browser's own modal dialog does, so that the user reaches the dialog
// alone.
// It stacks at the z-index of the overlays of core's theme
// (../runtime/index.css).
// A select or a menu in the dialog opens its list in a portal of its own,
// above the modal, which React Aria leaves within the user's reach.
// Escape closes the modal, as does a press outside the dialog where the
// modal `isDismissable`.
// The dialog's title is React Aria's Heading in the slot "title", which
// names the dialog.
// The modal draws on the background that core's theme gives what sits over
// the board, bg-raised.
//
// The dialog that asks goes in the modal, as the dialog does. It is a dialog
// of the role alertdialog, which asks the user to confirm what its `title`
// and its text say, with the starter kit's props: `actionLabel`, the label of
// the button that confirms, which has the focus as the dialog opens,
// `onAction`, which that button runs before it closes the dialog, and
// `cancelLabel`, "Cancel" by default, the label of the button that closes it
// alone. Its `variant` "destructive" draws the button that confirms in the
// danger colour, for what cannot be undone, and marks the dialog with an
// exclamation mark in that colour, where "info", the default, draws the
// button in the accent colour and marks the dialog with an i.

import type { ReactElement, ReactNode } from "react";
import { Button, composeRenderProps, Dialog as AriaDialog, Heading, Modal as AriaModal, ModalOverlay } from "../aria/index.ts";
import type { DialogProps, ModalOverlayProps } from "../aria/index.ts";
import { buttons } from "../styles/index.ts";

export function Modal({ className, ...props }: ModalOverlayProps): ReactElement {
  return (
    <ModalOverlay {...props} className={OVERLAY}>
      <div className={CENTER}>
        <AriaModal {...props} className={composeRenderProps(className, (given) => `${MODAL} ${given ?? ""}`)} />
      </div>
    </ModalOverlay>
  );
}

export function Dialog({ className, ...props }: DialogProps): ReactElement {
  return <AriaDialog {...props} className={`${DIALOG} ${className ?? ""}`} />;
}

export interface AlertDialogProps extends Omit<DialogProps, "children"> {
  readonly title: string;
  readonly children: ReactNode;
  readonly variant?: "info" | "destructive";
  readonly actionLabel: string;
  readonly cancelLabel?: string;
  readonly onAction?: () => void;
}

export function AlertDialog({
  title,
  variant = "info",
  actionLabel,
  cancelLabel = "Cancel",
  onAction,
  children,
  ...props
}: AlertDialogProps): ReactElement {
  const destructive = variant === "destructive";
  return (
    <Dialog role="alertdialog" {...props}>
      {({ close }) => (
        <>
          <Heading slot="title" className={TITLE}>
            {title}
          </Heading>
          <span className={`bc:absolute bc:top-4 bc:right-4 ${destructive ? "bc:text-text-danger" : "bc:text-blue-700 bc:dark:text-blue-400"}`}>
            <Mark destructive={destructive} />
          </span>
          <p className="bc:mt-3 bc:mb-0 bc:font-sans bc:text-sm bc:text-text-muted">{children}</p>
          <div className="bc:mt-6 bc:flex bc:justify-end bc:gap-2">
            <Button className={buttons.secondary} onPress={close}>
              {cancelLabel}
            </Button>
            <Button
              className={destructive ? buttons.destructive : buttons.primary}
              autoFocus
              onPress={() => {
                onAction?.();
                close();
              }}
            >
              {actionLabel}
            </Button>
          </div>
        </>
      )}
    </Dialog>
  );
}

// The mark of a dialog that asks: an exclamation mark in a circle where what
// it asks cannot be undone, and an i in a circle otherwise.
function Mark({ destructive }: { destructive: boolean }): ReactElement {
  return (
    <svg
      aria-hidden="true"
      viewBox="0 0 16 16"
      className="bc:block bc:size-5"
      fill="none"
      stroke="currentColor"
      strokeWidth={1.4}
      strokeLinecap="round"
      strokeLinejoin="round"
    >
      <circle cx="8" cy="8" r="6.4" />
      <path d={destructive ? "M8 4.6v4.2M8 11.2v.1" : "M8 7.4v3.6M8 4.9v.1"} />
    </svg>
  );
}

// The backdrop covers the whole page, and the box inside it the window, as
// the variables that React Aria sets on the backdrop measure them, so that
// the dialog stays in the middle of the window as the page scrolls.
const OVERLAY = "bc:absolute bc:top-0 bc:left-0 bc:isolate bc:z-overlay bc:h-(--page-height) bc:w-full bc:bg-black/40";
const CENTER =
  "bc:sticky bc:top-0 bc:left-0 bc:box-border bc:flex bc:h-(--visual-viewport-height) bc:w-full bc:items-center bc:justify-center bc:p-4";
const MODAL =
  "bc:box-border bc:max-h-[calc(var(--visual-viewport-height)*.9)] bc:w-full bc:max-w-xl bc:overflow-auto bc:rounded-lg bc:border bc:border-border-default bc:bg-bg-raised bc:font-sans bc:text-gray-900 bc:shadow-xl bc:outline-none bc:dark:text-gray-100";
const DIALOG = "bc:relative bc:box-border bc:max-h-[inherit] bc:p-4 bc:outline-none";
const TITLE = "bc:m-0 bc:pr-8 bc:font-sans bc:text-lg bc:font-semibold";
