// The pieces that the basic frontend alone draws with: its dialogs and its
// menus. The rest, such as its buttons, it shares with core's Edit board
// plugin (../../ui).
// They are styled with Tailwind CSS under core's prefix. Their colours, fonts
// and radii come from the variables of core's theme. Core's stylesheet leaves
// out Tailwind's reset of the page, and a browser passes its font and colour
// on to neither a button nor a dialog, so each piece sets them itself.

import { useEffect, useLayoutEffect, useRef, useState } from "react";
import type { ReactElement, ReactNode } from "react";
import { classes } from "../../ui/index.ts";

// A modal dialog, open while it is drawn, which closes on Escape, on its
// Close button and once its owner stops drawing it. It puts the focus on the
// element of its content marked data-autofocus, if any, as the browser would
// otherwise put it on the Close button.
export function Dialog({
  title,
  onClose,
  children,
}: {
  title: string;
  onClose: () => void;
  children: ReactNode;
}): ReactElement {
  const ref = useRef<HTMLDialogElement>(null);
  useLayoutEffect(() => {
    const dialog = ref.current;
    if (dialog === null) return;
    if (!dialog.open) dialog.showModal();
    dialog.querySelector<HTMLElement>("[data-autofocus]")?.focus();
    return () => {
      if (dialog.open) dialog.close();
    };
  }, []);
  return (
    <dialog
      ref={ref}
      aria-label={title}
      onClose={onClose}
      className={`${classes.surface} bc:w-full bc:max-w-xl bc:rounded-lg bc:border bc:border-border-default bc:p-4 bc:font-sans bc:shadow-xl bc:backdrop:bg-black/40`}
    >
      <div className="bc:grid bc:gap-3">
        <div className="bc:flex bc:items-center bc:justify-between bc:gap-2">
          <h2 className={classes.heading}>{title}</h2>
          <button type="button" className={classes.button} onClick={onClose}>
            Close
          </button>
        </div>
        {children}
      </div>
    </dialog>
  );
}

export interface MenuItem {
  readonly label: string;
  readonly onSelect: () => void;
  readonly disabled?: boolean;
}

// A button that opens a list of actions, which closes as one is picked, on
// Escape and on a click elsewhere.
export function Menu({ label, items }: { label: string; items: readonly MenuItem[] }): ReactElement {
  const [open, setOpen] = useState(false);
  const ref = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (!open) return;
    const doc = ref.current?.ownerDocument ?? document;
    const away = (event: Event): void => {
      if (!(event.target instanceof Node && ref.current?.contains(event.target) === true)) setOpen(false);
    };
    const escape = (event: KeyboardEvent): void => {
      if (event.key === "Escape") setOpen(false);
    };
    doc.addEventListener("pointerdown", away);
    doc.addEventListener("keydown", escape);
    return () => {
      doc.removeEventListener("pointerdown", away);
      doc.removeEventListener("keydown", escape);
    };
  }, [open]);

  return (
    <div ref={ref} className="bc:relative">
      <button
        type="button"
        aria-label={label}
        aria-haspopup="menu"
        aria-expanded={open}
        className={classes.quiet}
        onClick={() => {
          setOpen(!open);
        }}
      >
        <span aria-hidden="true" className="bc:text-base bc:leading-none">
          ⋯
        </span>
      </button>
      {open ? (
        <ul
          role="menu"
          aria-label={label}
          className={`${classes.surface} bc:absolute bc:right-0 bc:z-20 bc:m-0 bc:mt-1 bc:grid bc:min-w-44 bc:list-none bc:gap-0.5 bc:rounded-md bc:border bc:border-border-default bc:p-1 bc:shadow-lg`}
        >
          {items.map((item) => (
            <li key={item.label} role="none">
              <button
                type="button"
                role="menuitem"
                disabled={item.disabled}
                className="bc:w-full bc:rounded bc:border-0 bc:bg-transparent bc:px-2 bc:py-1 bc:text-left bc:font-sans bc:text-sm bc:text-text-default bc:cursor-pointer bc:hover:bg-bg-subtle bc:disabled:cursor-default bc:disabled:opacity-50"
                onClick={() => {
                  setOpen(false);
                  item.onSelect();
                }}
              >
                {item.label}
              </button>
            </li>
          ))}
        </ul>
      ) : null}
    </div>
  );
}
