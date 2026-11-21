// The pieces that core's basic frontend, its Edit board plugin and the forms
// of core's board actions draw with: the classes of their buttons, lists and
// surfaces, an inline field that renames, a field of a form that applies each
// edit at once, what the text fields of their forms take where they hold an
// identifier, the banner that says why an edit was refused, and what draws a
// form in a React root of its own.
// They are styled with Tailwind CSS under core's prefix. Their colours, fonts
// and radii come from the variables of core's theme. Core's stylesheet leaves
// out Tailwind's reset of the page, and a browser passes its font and colour
// on to no button, so each piece sets them itself.

import { useRef, useState } from "react";
import type { ReactElement, ReactNode } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { MARKS, SeverityIcon } from "../card/index.ts";
import { TextField } from "../react/index.ts";
import { buttons } from "../styles/index.ts";
import type { Refused } from "./edits.ts";

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

// A text field of a form that applies each edit as the user makes it, the
// SDK's TextField, as the form of Edit stack names a stack: Enter or leaving
// the field submits what it holds, trimmed, where it changed and its check
// takes it. Escape puts back the value, where the user changed it, and
// otherwise goes on to what holds the form, as to the row of a table that
// closes its form.
// The field says below itself why its check refuses what it holds, until the
// user types again. The form keys it by its value, so that it shows the value
// again once the board changes it.
// Where `identifier` is true, the field holds an identifier, which the
// browser neither corrects nor capitalizes.
export function EditField({
  label,
  value,
  description,
  check,
  onSubmit,
  identifier: holdsIdentifier = false,
}: {
  label: string;
  value: string;
  description?: string;
  check?: (value: string) => string | undefined;
  onSubmit: (value: string) => void;
  identifier?: boolean;
}): ReactElement {
  const [text, setText] = useState(value);
  const [error, setError] = useState<string | undefined>(undefined);

  function commit(): void {
    const trimmed = text.trim();
    const why = check?.(trimmed);
    setError(why);
    if (why === undefined && trimmed !== value) onSubmit(trimmed);
  }

  return (
    <TextField
      label={label}
      value={text}
      description={description}
      errorMessage={error}
      isInvalid={error !== undefined}
      autoComplete="off"
      {...(holdsIdentifier ? identifier : {})}
      onChange={(typed) => {
        setText(typed);
        setError(undefined);
      }}
      onKeyDown={(event) => {
        if (event.key === "Enter" && !event.nativeEvent.isComposing) {
          commit();
        } else if (event.key === "Escape" && text !== value) {
          setText(value);
          setError(undefined);
        } else {
          event.continuePropagation();
        }
      }}
      onBlur={commit}
    />
  );
}

// Draws a form in the element, in a React root of its own, before it returns,
// until the function that it returns runs, as an advanced action's render()
// does.
export function drawForm(el: HTMLElement, form: ReactNode): () => void {
  const root = createRoot(el);
  flushSync(() => {
    root.render(form);
  });
  return () => {
    root.unmount();
  };
}

// What a text field of the forms takes where it holds an identifier rather
// than prose, which the browser then neither corrects nor capitalizes.
export const identifier = { spellCheck: "false", autoCapitalize: "none", autoCorrect: "off" } as const;

// Says why the store or the command registry refused the last edit of a
// control, close to the control, in a banner that marks an error as a card's
// banners do: the sentence that names the edit, and below it the reason, or a
// list of the reasons where there are several. A screen reader reads it out
// as it shows. It draws nothing where the edit ran.
export function Refusal({ refusal }: { refusal: Refused | null }): ReactElement | null {
  if (refusal === null) return null;
  const mark = MARKS.error;
  const { title, reasons } = refusal;
  return (
    <div
      role="alert"
      className={`bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border-0 bc:border-l-4 bc:px-3 bc:py-2 ${mark.border} ${mark.tint}`}
    >
      <span className={`bc:mt-0.5 ${mark.text}`}>
        <SeverityIcon severity="error" />
      </span>
      <div className="bc:grid bc:min-w-0 bc:flex-1 bc:gap-1 bc:text-sm bc:break-words">
        <p className={`bc:m-0 bc:font-semibold ${mark.text}`}>{title}</p>
        {reasons.length === 1 ? (
          <p className="bc:m-0 bc:whitespace-pre-wrap">{reasons[0]}</p>
        ) : (
          <ul className="bc:m-0 bc:list-disc bc:pl-5">
            {reasons.map((reason, i) => (
              <li key={i} className="bc:whitespace-pre-wrap">
                {reason}
              </li>
            ))}
          </ul>
        )}
      </div>
    </div>
  );
}
