// A searchable dropdown, as the combobox of the WAI-ARIA Authoring Practices
// has it (https://www.w3.org/WAI/ARIA/apg/patterns/combobox/): a text field
// that filters a list of options as the user types, for picking one option out
// of many, or several in its multiple form, which shows each one picked as a
// chip with a button that takes it out again.
// The list opens below the field. It shows the options in groups where they
// have one, each with a line of detail where it has one, such as a block
// class's description.
// The keyboard works as in the pattern:
// - Down and Up open the list and move through its options.
// - Enter picks the option that the list marks, and Escape closes the list.
// - Tab leaves the field and closes the list.
// - Backspace in the empty field of the multiple form takes out the last
//   option picked.
// The field keeps the focus throughout, and names the option that the list
// marks in aria-activedescendant. A click on an option picks it as well.
// Picking an option changes nothing else on its own: a form that holds the
// field acts once the user submits it.

import { useId, useLayoutEffect, useMemo, useRef, useState } from "react";
import type { KeyboardEvent, ReactElement } from "react";
import { styles } from "../styles/index.ts";
import { classes } from "./ui.tsx";

export interface ComboOption {
  readonly value: string;
  // What the option shows, and what the field shows once it is picked.
  readonly label: string;
  // A second line, such as a block class's description or a block's id.
  readonly detail?: string;
  // A short tag beside the label, such as the package of a block class.
  readonly badge?: string;
  // The heading under which the list shows the option.
  readonly group?: string;
  // Further words that the search finds the option by.
  readonly keywords?: readonly string[];
  readonly disabled?: boolean;
}

interface Common {
  readonly label: string;
  readonly options: readonly ComboOption[];
  // A line below the field that says what it takes, or why the user's value
  // does not do, as an error.
  readonly hint?: string;
  readonly error?: string;
  readonly placeholder?: string;
  // What the list says where the search finds no option.
  readonly empty?: string;
  // Whether a dialog that holds the field puts the focus on it as it opens.
  readonly autofocus?: boolean;
}

export interface ComboboxProps extends Common {
  readonly value: string | undefined;
  readonly onChange: (value: string) => void;
}

export interface MultiComboboxProps extends Common {
  readonly values: readonly string[];
  readonly onChange: (values: readonly string[]) => void;
}

// A field that picks one option.
export function Combobox(props: ComboboxProps): ReactElement {
  const { value, onChange } = props;
  return <Field {...props} picked={value === undefined ? [] : [value]} pick={onChange} multiple={false} />;
}

// A field that picks any number of options, each shown as a chip.
export function MultiCombobox(props: MultiComboboxProps): ReactElement {
  const { values, onChange } = props;
  return (
    <Field
      {...props}
      picked={values}
      pick={(value) => {
        onChange([...values, value]);
      }}
      unpick={(value) => {
        onChange(values.filter((v) => v !== value));
      }}
      multiple
    />
  );
}

function Field({
  label,
  options,
  hint,
  error,
  placeholder,
  empty = "No option matches.",
  autofocus = false,
  picked,
  pick,
  unpick,
  multiple,
}: Common & {
  readonly picked: readonly string[];
  readonly pick: (value: string) => void;
  readonly unpick?: (value: string) => void;
  readonly multiple: boolean;
}): ReactElement {
  const id = useId();
  const listId = `${id}-list`;
  const noteId = `${id}-note`;
  const input = useRef<HTMLInputElement>(null);
  const list = useRef<HTMLUListElement>(null);
  const byValue = useMemo(() => new Map(options.map((option) => [option.value, option])), [options]);
  const single = multiple ? undefined : byValue.get(picked[0] ?? "");

  const [open, setOpen] = useState(false);
  // What the user typed since the list opened, which filters the options. The
  // single form shows the picked option's label while the list is closed.
  const [query, setQuery] = useState("");
  const [active, setActive] = useState<string | undefined>(undefined);

  // The options that the list shows: those that the search finds, and, in the
  // multiple form, not picked yet.
  const shown = useMemo(() => {
    const needle = query.trim().toLowerCase();
    const taken = new Set(multiple ? picked : []);
    return options.filter(
      (option) =>
        !taken.has(option.value) &&
        (needle === "" ||
          [option.label, option.detail ?? "", option.group ?? "", option.badge ?? "", ...(option.keywords ?? [])].some((text) =>
            text.toLowerCase().includes(needle),
          )),
    );
  }, [options, query, picked, multiple]);
  const pickable = shown.filter((option) => option.disabled !== true);
  const marked = active !== undefined && pickable.some((option) => option.value === active) ? active : pickable[0]?.value;

  useLayoutEffect(() => {
    if (!open || marked === undefined) return;
    list.current?.ownerDocument.getElementById(optionId(listId, marked))?.scrollIntoView({ block: "nearest" });
  }, [open, marked, listId]);

  function show(): void {
    if (!open) {
      setOpen(true);
      setActive(multiple ? undefined : single?.value);
    }
  }

  function close(): void {
    setOpen(false);
    setQuery("");
    setActive(undefined);
  }

  function choose(option: ComboOption): void {
    if (option.disabled === true) return;
    pick(option.value);
    if (multiple) {
      setQuery("");
      setActive(undefined);
      input.current?.focus();
    } else {
      close();
    }
  }

  function move(by: 1 | -1): void {
    if (!open) {
      show();
      return;
    }
    if (pickable.length === 0) return;
    const at = pickable.findIndex((option) => option.value === marked);
    const next = pickable[(at + by + pickable.length) % pickable.length];
    setActive(next?.value);
  }

  function onKeyDown(event: KeyboardEvent<HTMLInputElement>): void {
    if (event.nativeEvent.isComposing) return;
    if (event.key === "ArrowDown") {
      event.preventDefault();
      move(1);
    } else if (event.key === "ArrowUp") {
      event.preventDefault();
      move(-1);
    } else if (event.key === "Enter" && open) {
      // The form that holds the field submits only on an Enter that picks
      // nothing.
      event.preventDefault();
      const option = byValue.get(marked ?? "");
      if (option !== undefined) choose(option);
    } else if (event.key === "Escape" && open) {
      event.preventDefault();
      close();
    } else if (event.key === "Backspace" && multiple && query === "" && picked.length > 0) {
      const last = picked[picked.length - 1];
      if (last !== undefined) unpick?.(last);
    }
  }

  const groups = groupsOf(shown);
  const note = error ?? hint;
  const text = open ? query : multiple ? "" : (single?.label ?? "");

  return (
    <div className={styles.field}>
      <label htmlFor={id} className={styles.label}>
        {label}
      </label>
      {multiple && picked.length > 0 ? (
        <ul aria-label={`${label}, picked`} className="bc:m-0 bc:flex bc:list-none bc:flex-wrap bc:gap-1 bc:p-0">
          {picked.map((value) => {
            const option = byValue.get(value);
            const name = option?.label ?? value;
            return (
              <li
                key={value}
                className="bc:inline-flex bc:items-center bc:gap-1 bc:rounded-full bc:border bc:border-border-default bc:bg-bg-subtle bc:py-0.5 bc:pr-0.5 bc:pl-2 bc:text-xs"
              >
                <span>{name}</span>
                <button
                  type="button"
                  className={`${classes.quiet} bc:rounded-full bc:px-1`}
                  aria-label={`Remove ${name}`}
                  onClick={() => {
                    unpick?.(value);
                    input.current?.focus();
                  }}
                >
                  <span aria-hidden="true">×</span>
                </button>
              </li>
            );
          })}
        </ul>
      ) : null}
      {/* A grid stretches the field to the width of its column, border
          included, since core's stylesheet resets no element's box sizing. */}
      <div className="bc:relative bc:grid">
        <input
          ref={input}
          id={id}
          type="text"
          role="combobox"
          autoComplete="off"
          spellCheck={false}
          aria-autocomplete="list"
          aria-expanded={open}
          aria-controls={listId}
          aria-activedescendant={open && marked !== undefined ? optionId(listId, marked) : undefined}
          aria-invalid={error === undefined ? undefined : true}
          aria-describedby={note === undefined ? undefined : noteId}
          data-autofocus={autofocus || undefined}
          className={`${styles.box} bc:pr-8`}
          value={text}
          placeholder={placeholder}
          onChange={(event) => {
            setQuery(event.target.value);
            setActive(undefined);
            setOpen(true);
          }}
          onClick={show}
          onKeyDown={onKeyDown}
          onBlur={close}
        />
        <button
          type="button"
          tabIndex={-1}
          aria-label={open ? `Close the options of ${label}` : `Show the options of ${label}`}
          className="bc:absolute bc:inset-y-0 bc:right-0 bc:flex bc:w-8 bc:cursor-pointer bc:items-center bc:justify-center bc:border-0 bc:bg-transparent bc:text-text-muted"
          // The field keeps the focus, so that leaving it does not close the
          // list before the click toggles it.
          onMouseDown={(event) => {
            event.preventDefault();
          }}
          onClick={() => {
            if (open) close();
            else {
              show();
              input.current?.focus();
            }
          }}
        >
          <span aria-hidden="true">▾</span>
        </button>
        {open ? (
          <ul
            ref={list}
            id={listId}
            role="listbox"
            aria-label={label}
            aria-multiselectable={multiple || undefined}
            className={`${classes.surface} bc:absolute bc:inset-x-0 bc:top-full bc:z-30 bc:m-0 bc:mt-1 bc:max-h-72 bc:list-none bc:overflow-y-auto bc:rounded-md bc:border bc:border-border-default bc:p-1 bc:shadow-lg`}
          >
            {shown.length === 0 ? <li className={`${styles.hint} bc:px-2 bc:py-1`}>{empty}</li> : null}
            {groups.map(([group, members]) =>
              group === undefined ? (
                members.map((option) => (
                  <Option key={option.value} option={option} id={optionId(listId, option.value)} marked={option.value === marked} picked={picked.includes(option.value)} choose={choose} />
                ))
              ) : (
                <li key={group} role="presentation">
                  <div
                    id={groupId(listId, group)}
                    className="bc:px-2 bc:pt-2 bc:pb-1 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wide bc:text-text-muted"
                  >
                    {group}
                  </div>
                  <ul role="group" aria-labelledby={groupId(listId, group)} className="bc:m-0 bc:list-none bc:p-0">
                    {members.map((option) => (
                      <Option key={option.value} option={option} id={optionId(listId, option.value)} marked={option.value === marked} picked={picked.includes(option.value)} choose={choose} />
                    ))}
                  </ul>
                </li>
              ),
            )}
          </ul>
        ) : null}
      </div>
      {note === undefined ? null : (
        <p id={noteId} className={error === undefined ? styles.hint : styles.alert}>
          {note}
        </p>
      )}
    </div>
  );
}

function Option({
  option,
  id,
  marked,
  picked,
  choose,
}: {
  option: ComboOption;
  id: string;
  marked: boolean;
  picked: boolean;
  choose: (option: ComboOption) => void;
}): ReactElement {
  return (
    <li
      id={id}
      role="option"
      data-value={option.value}
      aria-selected={picked}
      aria-disabled={option.disabled === true || undefined}
      className={`bc:grid bc:cursor-pointer bc:gap-0.5 bc:rounded bc:px-2 bc:py-1 bc:text-sm ${marked ? "bc:bg-bg-subtle bc:outline bc:outline-1 bc:outline-border-accent" : ""} ${
        option.disabled === true ? "bc:cursor-default bc:opacity-50" : ""
      }`}
      // The field keeps the focus, so that leaving it does not close the list
      // before the click picks the option.
      onMouseDown={(event) => {
        event.preventDefault();
      }}
      onClick={() => {
        choose(option);
      }}
    >
      <span className="bc:flex bc:items-baseline bc:gap-2">
        <span className={`bc:min-w-0 bc:flex-1 ${picked ? "bc:font-semibold" : "bc:font-medium"}`}>
          {picked ? <span aria-hidden="true">✓ </span> : null}
          {option.label}
        </span>
        {option.badge === undefined ? null : (
          <span className="bc:shrink-0 bc:rounded bc:border bc:border-border-default bc:px-1 bc:text-xs bc:font-normal bc:text-text-muted">
            {option.badge}
          </span>
        )}
      </span>
      {option.detail === undefined || option.detail === "" ? null : <span className="bc:text-xs bc:text-text-muted">{option.detail}</span>}
    </li>
  );
}

// The options by group, in the order in which each group first comes, the
// options without a group first.
function groupsOf(options: readonly ComboOption[]): [string | undefined, ComboOption[]][] {
  const groups = new Map<string | undefined, ComboOption[]>();
  for (const option of options) groups.set(option.group, [...(groups.get(option.group) ?? []), option]);
  const loose = groups.get(undefined);
  groups.delete(undefined);
  return [...(loose === undefined ? [] : [[undefined, loose] as [undefined, ComboOption[]]]), ...groups];
}

// The id of an option's element, which aria-activedescendant names, and that
// of a group's heading, which names the group. An id holds no blank, so each
// character but a letter, a digit, "-" and "_" goes in as its code.
function optionId(list: string, value: string): string {
  return `${list}-option-${safe(value)}`;
}

function groupId(list: string, group: string): string {
  return `${list}-group-${safe(group)}`;
}

function safe(text: string): string {
  return text.replace(/[^A-Za-z0-9_-]/g, (c) => `_${c.charCodeAt(0).toString(16)}`);
}
