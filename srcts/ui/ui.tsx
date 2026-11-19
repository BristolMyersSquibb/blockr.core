// The pieces that core's basic frontend and its Edit board plugin draw with:
// the classes of their buttons, lists and surfaces, an inline field that
// renames, what the text fields of their forms take where they hold an
// identifier, and the line that says why an edit was refused.
// They are styled with Tailwind CSS under core's prefix. Their colours, fonts
// and radii come from the variables of core's theme. Core's stylesheet leaves
// out Tailwind's reset of the page, and a browser passes its font and colour
// on to no button, so each piece sets them itself.

import { useRef, useState } from "react";
import type { ReactElement } from "react";
import { TextField } from "../react/index.ts";
import { buttons, styles } from "../styles/index.ts";

export const classes = {
  button: buttons.secondary,
  primary: buttons.primary,
  quiet: buttons.quiet,
  // The board's page, which draws no background of its own, so that the
  // page's, which the browser draws in the page's theme, shows to its edges.
  page: "bc:text-gray-900 bc:dark:text-gray-100",
  // What sits over the board, as a dialog or a menu does,
  // on the background that core's theme gives it, bg-raised.
  surface: "bc:bg-bg-raised bc:text-gray-900 bc:dark:text-gray-100",
  card: "bc:bg-bg-subtle",
  heading: "bc:m-0 bc:font-sans bc:text-base bc:font-semibold",
  list: "bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0",
} as const;

// A text field that edits a name in place, the SDK's TextField: Enter or
// leaving the field submits what it holds, unless it is empty or unchanged,
// and Escape gives up.
// It has the focus as it is drawn, with the name selected.
// It is at least 10rem wide, or with `fill` as wide as its container, as the
// cell of a table that it must not spill out of.
export function NameField({
  label,
  name,
  onSubmit,
  onDone,
  fill = false,
}: {
  label: string;
  name: string;
  onSubmit: (name: string) => void;
  onDone: () => void;
  fill?: boolean;
}): ReactElement {
  const [text, setText] = useState(name);
  const done = useRef(false);

  function finish(submit: boolean): void {
    if (done.current) return;
    done.current = true;
    const trimmed = text.trim();
    if (submit && trimmed !== "" && trimmed !== name) onSubmit(trimmed);
    onDone();
  }

  return (
    <TextField
      aria-label={label}
      value={text}
      autoFocus
      spellCheck="false"
      className={fill ? "bc:w-full" : "bc:min-w-40"}
      onChange={setText}
      onFocus={(event) => {
        event.target.select();
      }}
      onKeyDown={(event) => {
        if (event.key === "Enter" && !event.nativeEvent.isComposing) finish(true);
        else if (event.key === "Escape") finish(false);
        else event.continuePropagation();
      }}
      onBlur={() => {
        finish(true);
      }}
    />
  );
}

// What a text field of the forms takes where it holds an identifier rather
// than prose, which the browser then neither corrects nor capitalizes.
export const identifier = { spellCheck: "false", autoCapitalize: "none", autoCorrect: "off" } as const;

// Says why the store or the command registry refused the last edit of a
// control, close to the control, in the colour of an error, as the inputs of
// core's React SDK say why their host refused what they submitted. A screen
// reader reads it out as it shows. It draws nothing where the edit ran.
export function Refusal({ refusal }: { refusal: string | null }): ReactElement | null {
  return refusal === null ? null : (
    <p role="alert" className={styles.alert}>
      {refusal}
    </p>
  );
}
