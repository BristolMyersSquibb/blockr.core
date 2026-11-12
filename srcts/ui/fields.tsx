// A text field of the forms that core's basic frontend and its Edit board
// plugin draw.

import { useId } from "react";
import type { ReactElement } from "react";
import { styles } from "../styles/index.ts";

// A labelled text field, with a line below it that says what it takes, or
// why what the user gave does not do, as an error that the field names in
// aria-describedby.
export function TextField({
  label,
  value,
  hint,
  error,
  placeholder,
  identifier = false,
  onChange,
  onLeave,
}: {
  label: string;
  value: string;
  hint?: string;
  error?: string;
  placeholder?: string;
  // Whether it holds an identifier rather than prose, which the browser
  // then neither corrects nor capitalizes.
  identifier?: boolean;
  onChange: (value: string) => void;
  // Called as the focus leaves the field, where a form checks what it holds.
  onLeave?: () => void;
}): ReactElement {
  const id = useId();
  const noteId = `${id}-note`;
  const note = error ?? hint;
  return (
    <div className={styles.field}>
      <label htmlFor={id} className={styles.label}>
        {label}
      </label>
      <input
        id={id}
        type="text"
        className={styles.box}
        value={value}
        placeholder={placeholder}
        spellCheck={identifier ? false : undefined}
        autoCapitalize={identifier ? "none" : undefined}
        autoCorrect={identifier ? "off" : undefined}
        autoComplete="off"
        aria-invalid={error === undefined ? undefined : true}
        aria-describedby={note === undefined ? undefined : noteId}
        onChange={(event) => {
          onChange(event.target.value);
        }}
        onBlur={onLeave}
      />
      {note === undefined ? null : (
        <p id={noteId} className={error === undefined ? styles.hint : styles.alert}>
          {note}
        </p>
      )}
    </div>
  );
}
