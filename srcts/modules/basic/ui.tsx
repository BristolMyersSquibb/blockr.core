// The piece that the basic frontend alone draws with: its dialog, which is
// that of core's React SDK, React Aria's in core's theme. Its menus are those
// of the SDK too, and core draws the menus of board actions. The rest, such
// as its buttons, it shares with core's Edit board plugin (../../ui).

import type { ReactElement, ReactNode } from "react";
import { Dialog as SdkDialog, Heading, Modal } from "../../react/index.ts";
import { classes } from "../../ui/index.ts";

// A modal dialog, open while it is drawn, which closes on Escape, on its
// Close button and once its owner stops drawing it.
// The SDK's modal keeps the focus in it and makes the rest of the page inert,
// as the browser's own modal dialog does, but for the frontend's toasts,
// which React Aria leaves within reach (./toasts.ts).
// An element of its content that takes `autoFocus` has the focus as the
// dialog opens, and the dialog itself has it otherwise.
export function Dialog({
  title,
  onClose,
  children,
}: {
  title: string;
  onClose: () => void;
  children: ReactNode;
}): ReactElement {
  return (
    <Modal
      isOpen
      onOpenChange={(open) => {
        if (!open) onClose();
      }}
    >
      <SdkDialog>
        <div className="bc:grid bc:gap-3">
          <div className="bc:flex bc:items-center bc:justify-between bc:gap-2">
            <Heading slot="title" className={classes.heading}>
              {title}
            </Heading>
            <button type="button" className={classes.button} onClick={onClose}>
              Close
            </button>
          </div>
          {children}
        </div>
      </SdkDialog>
    </Modal>
  );
}
