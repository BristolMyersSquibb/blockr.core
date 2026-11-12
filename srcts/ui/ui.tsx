// The pieces that core's basic frontend and its Edit board plugin draw with:
// the classes of their buttons, lists and surfaces, and an inline field that
// renames.
// They are styled with Tailwind CSS under core's prefix. Their colours, fonts
// and radii come from the variables of core's theme. Core's stylesheet leaves
// out Tailwind's reset of the page, and a browser passes its font and colour
// on to no button, so each piece sets them itself.

import { useLayoutEffect, useRef, useState } from "react";
import type { ReactElement } from "react";
import { styles } from "../styles/index.ts";

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
