// The pieces that the basic frontend alone draws with: its dialogs and its
// menus, which are those of core's React SDK, React Aria's in core's theme.
// The rest, such as its buttons, it shares with core's Edit board plugin
// (../../ui).

import { useLayoutEffect, useRef } from "react";
import type { ReactElement, ReactNode } from "react";
import { Button, Dialog as SdkDialog, Heading, Menu as SdkMenu, MenuItem as SdkMenuItem, MenuTrigger, Modal } from "../../react/index.ts";
import { classes } from "../../ui/index.ts";
import { useFrontEnd } from "./context.ts";

// A modal dialog, open while it is drawn, which closes on Escape, on its
// Close button and once its owner stops drawing it.
// The SDK's modal keeps the focus in it and makes the rest of the page inert,
// as the browser's own modal dialog does.
// So while it is open, the frontend's toasts show in an element at its end.
// The toasts stay fixed to the window's corner there, and the user can still
// close them.
// An element of its content that takes `autoFocus`, as the picker's select
// does, has the focus as the dialog opens, and the dialog itself has it
// otherwise.
export function Dialog({
  title,
  onClose,
  children,
}: {
  title: string;
  onClose: () => void;
  children: ReactNode;
}): ReactElement {
  const { toasts } = useFrontEnd();
  const toastsRef = useRef<HTMLDivElement>(null);
  useLayoutEffect(() => {
    const inside = toastsRef.current;
    if (inside === null) return;
    return toasts.showIn(inside);
  }, [toasts]);
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
        <div ref={toastsRef} />
      </SdkDialog>
    </Modal>
  );
}

export interface MenuItem {
  readonly label: string;
  readonly onSelect: () => void;
  readonly disabled?: boolean;
}

// A button that opens a list of actions below its right end, which closes as
// one is picked, on Escape and on a press elsewhere.
export function Menu({ label, items }: { label: string; items: readonly MenuItem[] }): ReactElement {
  return (
    <MenuTrigger placement="bottom end">
      <Button variant="quiet" aria-label={label}>
        <span aria-hidden="true" className="bc:text-base bc:leading-none">
          ⋯
        </span>
      </Button>
      <SdkMenu aria-label={label}>
        {items.map((item) => (
          <SdkMenuItem key={item.label} id={item.label} isDisabled={item.disabled} onAction={item.onSelect}>
            {item.label}
          </SdkMenuItem>
        ))}
      </SdkMenu>
    </MenuTrigger>
  );
}
