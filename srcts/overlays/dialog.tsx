// The modal and the dialog of core's React SDK: React Aria's ModalOverlay and
// Modal (https://react-aria.adobe.com/Modal), drawn as one, and React Aria's
// Dialog (https://react-aria.adobe.com/Dialog), in core's theme, after the
// Modal and the Dialog of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Modal.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Dialog.tsx).
// Each takes the props of its part of React Aria.
// The modal covers the page with a backdrop and holds the dialog in the
// middle of the window.
// React Aria draws it in a portal at the end of the page's body, keeps the
// focus inside it and makes the rest of the page inert while it is open, as
// the browser's own modal dialog does, so that the user reaches the dialog
// alone.
// A select or a menu in the dialog opens its list in a portal of its own,
// above the modal, which React Aria leaves within the user's reach.
// Escape closes the modal, as does a press outside the dialog where the
// modal `isDismissable`.
// The dialog's title is React Aria's Heading in the slot "title", which
// names the dialog.
// The modal draws on the background that core's theme gives what sits over
// the board, bg-raised, a step lighter than the colour of the page in the
// middle of the window (./raised.ts).

import type { ReactElement } from "react";
import { composeRenderProps, Dialog as AriaDialog, Modal as AriaModal, ModalOverlay } from "../aria/index.ts";
import type { DialogProps, ModalOverlayProps } from "../aria/index.ts";
import { useRaised } from "./raised.ts";

export function Modal({ className, ...props }: ModalOverlayProps): ReactElement {
  const raised = useRaised("middle");
  return (
    <ModalOverlay {...props} data-bc-overlay="" className={BACKDROP}>
      <div className={CENTER}>
        <AriaModal {...props} ref={raised} className={composeRenderProps(className, (given) => `${MODAL} ${given ?? ""}`)} />
      </div>
    </ModalOverlay>
  );
}

export function Dialog({ className, ...props }: DialogProps): ReactElement {
  return <AriaDialog {...props} className={`${DIALOG} ${className ?? ""}`} />;
}

// The backdrop covers the whole page, and the box inside it the window, as
// the variables that React Aria sets on the backdrop measure them, so that
// the dialog stays in the middle of the window as the page scrolls.
const BACKDROP = "bc:absolute bc:top-0 bc:left-0 bc:isolate bc:z-50 bc:h-(--page-height) bc:w-full bc:bg-black/40";
const CENTER =
  "bc:sticky bc:top-0 bc:left-0 bc:box-border bc:flex bc:h-(--visual-viewport-height) bc:w-full bc:items-center bc:justify-center bc:p-4";
const MODAL =
  "bc:box-border bc:max-h-[calc(var(--visual-viewport-height)*.9)] bc:w-full bc:max-w-xl bc:overflow-auto bc:rounded-lg bc:border bc:border-border-default bc:bg-bg-raised bc:font-sans bc:text-gray-900 bc:shadow-xl bc:outline-none bc:dark:text-gray-100";
const DIALOG = "bc:relative bc:box-border bc:max-h-[inherit] bc:p-4 bc:outline-none";
