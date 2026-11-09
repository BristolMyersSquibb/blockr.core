// The pieces that the basic front-end draws with: its buttons, dialogs and
// menus, an inline field that renames, and the element in which core mounts a
// module.
// They are styled with Tailwind CSS under core's prefix. Their colours, fonts
// and radii come from the variables of core's theme. Core's stylesheet leaves
// out Tailwind's reset of the page, and a browser passes its font and colour
// on to neither a button nor a dialog, so each piece sets them itself.

import { useEffect, useLayoutEffect, useRef, useState } from "react";
import type { ReactElement, ReactNode } from "react";
import { styles } from "../../styles/index.ts";

export const classes = {
  button:
    "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:cursor-pointer bc:hover:bg-bg-subtle bc:disabled:cursor-default bc:disabled:opacity-50",
  primary:
    "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-border-accent bc:bg-bg-accent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-on-accent bc:cursor-pointer bc:disabled:cursor-default bc:disabled:opacity-50",
  quiet:
    "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border-0 bc:bg-transparent bc:px-1.5 bc:py-0.5 bc:font-sans bc:text-xs bc:text-text-muted bc:cursor-pointer bc:hover:bg-bg-subtle",
  // The board's page, which draws no background of its own, so that the
  // page's, which the browser draws in the page's theme, shows to its edges.
  page: "bc:text-gray-900 bc:dark:text-gray-100",
  // What sits over the board, as a dialog or a menu does.
  surface: "bc:bg-white bc:text-gray-900 bc:dark:bg-gray-900 bc:dark:text-gray-100",
  card: "bc:bg-bg-subtle",
  heading: "bc:m-0 bc:font-sans bc:text-base bc:font-semibold",
  list: "bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0",
} as const;

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

// A text field that edits a name in place: Enter or leaving the field
// submits what it holds, unless it is empty or unchanged, and Escape gives up.
export function NameField({
  label,
  name,
  onSubmit,
  onDone,
}: {
  label: string;
  name: string;
  onSubmit: (name: string) => void;
  onDone: () => void;
}): ReactElement {
  const [text, setText] = useState(name);
  const ref = useRef<HTMLInputElement>(null);
  const done = useRef(false);

  useLayoutEffect(() => {
    ref.current?.focus();
    ref.current?.select();
  }, []);

  function finish(submit: boolean): void {
    if (done.current) return;
    done.current = true;
    const trimmed = text.trim();
    if (submit && trimmed !== "" && trimmed !== name) onSubmit(trimmed);
    onDone();
  }

  return (
    <input
      ref={ref}
      type="text"
      aria-label={label}
      value={text}
      spellCheck={false}
      className={`${styles.box} bc:min-w-40`}
      onChange={(event) => {
        setText(event.target.value);
      }}
      onKeyDown={(event) => {
        if (event.key === "Enter" && !event.nativeEvent.isComposing) finish(true);
        else if (event.key === "Escape") finish(false);
      }}
      onBlur={() => {
        finish(true);
      }}
    />
  );
}

// The element in which core mounts a module, as `mount` does it, until the
// component goes.
// Core's cleanup runs once React has finished the work in which the component
// went. A module written in React then unmounts its own root at once, before
// core empties the element. Within that work, React would put the unmount off
// until after core had emptied the element, and then fail to remove what it
// drew.
export function Mounted({
  mount,
  label,
  className,
}: {
  mount: (el: HTMLElement) => () => void;
  label?: string;
  className?: string;
}): ReactElement {
  const ref = useRef<HTMLDivElement>(null);
  useEffect(() => {
    const el = ref.current;
    if (el === null) return;
    const cleanup = mount(el);
    return () => {
      queueMicrotask(cleanup);
    };
  }, [mount]);
  return <div ref={ref} aria-label={label} className={className} />;
}
