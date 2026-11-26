// The pieces that core's basic frontend, its Edit board plugin and the forms
// of core's board actions draw with: the classes of their buttons, lists and
// surfaces, an inline field that renames, the button that opens the menu of a
// board object, what the text fields of their forms take where they hold an
// identifier, the banner that says why an edit was refused, and what draws a
// form in a React root of its own.
// They are styled with Tailwind CSS under core's prefix. Their colours, fonts
// and radii come from blockr.ui's theme variables. Core's stylesheet leaves
// out Tailwind's reset of the page, and a browser passes its font and colour
// on to no button, so each piece sets them itself.

import { useRef, useState } from "react";
import type { ReactElement, ReactNode, RefObject } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { Button, TextField } from "@blockr/ui";
import { MARKS, SeverityIcon } from "../card/index.ts";
import { buttons } from "../styles/index.ts";
import type { Refused } from "./edits.ts";

export const classes = {
  button: buttons.secondary,
  primary: buttons.primary,
  quiet: buttons.quiet,
  // The board's page, in the page's colour and the colour of its text,
  // `background` and `foreground`
  // (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.2#the-defaults).
  page: "bc:bg-background bc:text-foreground",
  // What sits over the board, as a dialog or a menu does, in the colours of
  // every overlay, `popover`.
  surface: "bc:bg-popover bc:text-popover-foreground",
  // A card on the page, in the colours of a card, `card`.
  card: "bc:bg-card bc:text-card-foreground",
  heading: "bc:m-0 bc:font-sans bc:text-base bc:font-semibold",
  list: "bc:m-0 bc:grid bc:list-none bc:gap-2 bc:p-0",
} as const;

// A text field that edits a name in place, blockr.ui's TextField: Enter or
// leaving the field submits what it holds, unless it is empty or unchanged,
// and Escape gives up.
// It has the focus as it is drawn, with the name selected.
// It is at least 10rem wide.
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
      className="bc:min-w-40"
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

// The "…" button that opens the menu of board actions of a board object, as
// a card of the basic frontend and a row of the Edit board plugin draw it,
// named by the label given, as "Actions of Head". It is a Button of blockr.ui,
// which a row of React Aria's Table hears no press of, so that pressing it in
// a row does not tick the row.
export function ActionsButton({
  ref,
  label,
  onPress,
}: {
  ref: RefObject<HTMLButtonElement | null>;
  label: string;
  onPress: () => void;
}): ReactElement {
  return (
    <Button ref={ref} variant="quiet" aria-label={label} aria-haspopup="menu" onPress={onPress}>
      <span aria-hidden="true" className="bc:text-base bc:leading-none">
        ⋯
      </span>
    </Button>
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
