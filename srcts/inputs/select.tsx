// The one select of core's React SDK, which core's own UIs, frontends, the UIs
// of plugins and those of blocks all draw with.
// It follows the select of blockr.ui's design system
// (https://github.com/BristolMyersSquibb/blockr.ui/blob/e14e8310481e98b156871bf96669b41cb88c8435/vignettes/articles/design-system.Rmd#L671-L718),
// in core's theme, as core's other inputs are drawn, but for the select of
// several, whose list keeps the choices in their order, as the list of one
// does, and whose field takes the filter, as the field of one does.
// It picks one of its choices, or several, out of a list that the user
// filters by typing.
// It comes in three forms, by its props:
// - Given `value` and `onChange`, it picks one choice, and calls `onChange`
//   with the user's pick, as a plugin's UI runs a command with it.
// - Given `multiple` as well, it picks several, and calls `onChange` once with
//   the picks, as its list closes.
// - Given `bound`, it shows and edits a block's state arg or a board option's
//   value, which useValue() binds, as the SDK's other inputs do
//   (./inputs.tsx).
//   It picks one string, or an array of strings where `multiple`.
//
// The list is a popover, which the browser shows in its top layer, so that no
// panel clips it.
// In the DOM, the list stays inside the select's element, as the contract
// asks of a module
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.4#a-plugins-panel).
// Core's stylesheet places it below the field, or above where the window has
// no room below, through CSS anchor positioning (../runtime/index.css).
// In a browser without anchor positioning, script places it so (./place.ts).
// The select shows the list in the top layer only where the browser has the
// Popover API, which happy-dom lacks, so that a module's tests need no stub.
// The popover is a manual one, which the browser closes on no event of its
// own, so the select closes it.
//
// The field is a text field that shows the pick, or the picks, and into
// which the user types to filter the list, as the combobox of the WAI-ARIA
// Authoring Practices has it
// (https://www.w3.org/WAI/ARIA/apg/patterns/combobox/).
// The field of several shows one or two picks by name, and more as a count
// with the names after it.
// While the list is open, the field shows what the user typed, and the pick
// or the picks muted until the user types.
// The list shows the choices in their order, the pick of one in weight 600,
// and each pick of several with a tick.
// A pick of several keeps the list open and keeps the filter text, and the
// list reports the picks once, as it closes.
// Where the select goes while its list is open, as where its module
// unmounts, the picks go with it, as though the user never made them.
// Given `min`, the last picks do not untick.
//
// The keys are the pattern's:
// - Down, Up, Enter and Space open a closed list, and so does typing into the
//   field, which starts the filter afresh.
// - The arrows move the row that the keyboard marks, Enter picks it, or ticks
//   it in the list of several, and Escape closes the list.
//   In the list of several, Space ticks the row too while the user has typed
//   nothing.
// - Tab closes the list, as the focus moves on from the field.
// The list of one opens on the pick.
// It marks no row while the pointer opened it until the user presses a key,
// so that no row reads as a default pick.
// The focus stays in the field, which names the row that the keyboard marks
// in aria-activedescendant.
// The list closes as the focus leaves the field, as on a click outside it.
//
// The list shows at most MAX_ROWS rows, and then says how many more there are.
// It shows each choice's group as a heading, which stays in view as the list
// scrolls.
// It says "Loading…" while its choices are undefined, `empty` where there are
// none, and "No matches" where the filter keeps none.

import { useId, useLayoutEffect, useRef, useState } from "react";
import type { KeyboardEvent, ReactElement, ReactNode, RefObject } from "react";
import { strings } from "../binding/index.ts";
import type { Choice } from "../binding/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import { missing, Refusal } from "./inputs.tsx";
import { listSpot, listWidth } from "./place.ts";

// What each form of the select takes.
interface Common {
  // The field's label, which names the select.
  readonly label: string;
  // The choices, or undefined until they are known, such as until a request
  // answers.
  readonly choices?: readonly Choice[];
  // What the field shows while nothing is picked, "None" by default for
  // several choices.
  readonly placeholder?: string;
  // What the list says where it has no choice at all, "No options" by
  // default.
  readonly empty?: string;
  // Whether the select is greyed out, out of the tab order and never opens.
  readonly disabled?: boolean;
}

// What the forms take that pick for their caller.
interface Unbound extends Common {
  readonly bound?: undefined;
  // A line below the field that says what it takes, or why its value does not
  // do, as an error.
  readonly hint?: string;
  readonly error?: string;
  // Whether a dialog that holds the select puts the focus on it as it opens.
  readonly autofocus?: boolean;
}

export interface SelectOneProps extends Unbound {
  readonly multiple?: false;
  readonly value: string | undefined;
  readonly onChange: (value: string) => void;
}

export interface SelectSeveralProps extends Unbound {
  readonly multiple: true;
  readonly value: readonly string[];
  // Called once as the list closes, where the picks changed.
  readonly onChange: (value: readonly string[]) => void;
  // The fewest picks that the caller takes, 0 by default.
  readonly min?: number;
}

// The bound form shows a value that the choices lack, such as a column that
// the data no longer has, as it is while the choices are undefined.
// Once they are known, it shows such a value as missing, and marks itself
// invalid.
// While it is `outdated`, as while the request that gives its choices asks
// again, the choices may be for older data.
// It then keeps them on screen, and shows a value that they lack as it is.
// So its choices must list every value it takes, not a part of them.
// One choice it submits as the user picks it.
// Several it submits once, as the list closes, built from the value that the
// host holds then, so that it keeps a string the assistant added in the
// meantime.
// It submits them in the order the user picked them, after those the value
// holds, or where `ordered`, in the order of the choices, as for severities.
// It says below itself why the host refused what it submitted.
export interface SelectBoundProps extends Common {
  readonly bound: Bound;
  readonly multiple?: boolean;
  readonly outdated?: boolean;
  readonly ordered?: boolean;
  readonly min?: number;
}

export type SelectProps = SelectOneProps | SelectSeveralProps | SelectBoundProps;

// How many rows the list shows at most.
export const MAX_ROWS = 200;

export function Select(props: SelectProps): ReactElement {
  if (props.bound !== undefined) {
    return props.multiple === true ? <BoundSeveral {...props} /> : <BoundOne {...props} />;
  }
  if (props.multiple === true) {
    const { onChange, ...rest } = props;
    return (
      <Picker
        {...rest}
        report={(picks) => {
          onChange(picks);
        }}
      />
    );
  }
  const { value, onChange, ...rest } = props;
  return (
    <Picker
      {...rest}
      multiple={false}
      value={value === undefined ? [] : [value]}
      report={([pick]) => {
        if (pick !== undefined) onChange(pick);
      }}
    />
  );
}

function BoundOne({ bound, choices, outdated = false, placeholder, label, empty, disabled }: SelectBoundProps): ReactElement {
  const current = typeof bound.value === "string" ? bound.value : undefined;
  const known = choices ?? [];
  const lacked = current !== undefined && !known.some((choice) => choice.value === current);
  const invalid = lacked && choices !== undefined && !outdated;
  const shown =
    choices === undefined || current === undefined || !lacked
      ? choices
      : [...known, { value: current, label: invalid ? missing(current) : current }];
  return (
    <>
      <Picker
        label={label}
        choices={shown}
        multiple={false}
        value={current === undefined ? [] : [current]}
        placeholder={placeholder}
        empty={empty}
        disabled={disabled}
        invalid={invalid}
        report={([next]) => {
          if (next !== undefined) void bound.commit(next);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

function BoundSeveral({
  bound,
  choices,
  outdated = false,
  ordered = false,
  placeholder,
  label,
  empty,
  disabled,
  min,
}: SelectBoundProps): ReactElement {
  const held = strings(bound.value);
  const known = choices ?? [];
  const marking = choices !== undefined && !outdated;
  const lacked = held.filter((value) => !known.some((choice) => choice.value === value));
  const shown =
    choices === undefined ? undefined : [...known, ...lacked.map((value) => ({ value, label: marking ? missing(value) : value }))];

  // Builds the submit from the value that the host holds now: the strings the
  // user ticked since the list opened, after those the value holds, without
  // those the user unticked.
  const report = (picks: readonly string[], opened: readonly string[]): void => {
    const added = picks.filter((value) => !opened.includes(value));
    const removed = opened.filter((value) => !picks.includes(value));
    const now = strings(bound.get());
    const next = [...now.filter((value) => !removed.includes(value)), ...added.filter((value) => !now.includes(value))];
    void bound.commit(ordered ? inOrder(next, known) : next);
  };

  return (
    <>
      <Picker
        label={label}
        choices={shown}
        multiple
        value={held}
        placeholder={placeholder}
        empty={empty}
        disabled={disabled}
        min={min}
        invalid={marking && lacked.length > 0}
        report={report}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// The values in the order of the choices, those that the choices lack after
// them.
function inOrder(values: readonly string[], choices: readonly Choice[]): string[] {
  const order = choices.map((choice) => choice.value);
  return [...order.filter((value) => values.includes(value)), ...values.filter((value) => !order.includes(value))];
}

// What the field takes in either form: the pick of one as an array of it,
// or an empty one.
type PickerProps = Omit<Unbound, "bound"> & {
  readonly multiple: boolean;
  readonly value: readonly string[];
  // Hears the picks where they changed, with the value as the list opened:
  // the pick of one as the user picks it, and the picks of several as the
  // list closes.
  readonly report: (picks: readonly string[], opened: readonly string[]) => void;
  // The fewest picks of several that the caller takes, 0 by default.
  readonly min?: number;
  // Whether the field marks itself invalid, as for a value that the choices
  // lack.
  readonly invalid?: boolean;
};

// The field, a text field that shows the pick, or the picks, and into which
// the user types to filter the list, and the list while it is open.
function Picker({
  label,
  choices,
  multiple,
  value,
  report,
  placeholder = multiple ? "None" : "",
  hint,
  error,
  empty = "No options",
  disabled = false,
  autofocus = false,
  min = 0,
  invalid = false,
}: PickerProps): ReactElement {
  const id = useId();
  const listId = `${id}-list`;
  const noteId = `${id}-note`;
  const anchor = anchorName(id);
  const box = useRef<HTMLDivElement>(null);
  const input = useRef<HTMLInputElement>(null);
  const list = useRef<HTMLDivElement>(null);

  const [open, setOpen] = useState(false);
  // What the user typed since the list opened, which filters the choices.
  const [query, setQuery] = useState("");
  // Whether the list marks the row that the keyboard is on, and which row.
  const [keyboard, setKeyboard] = useState(false);
  const [active, setActive] = useState<string | undefined>(undefined);
  // The picks of several while the list is open, which it reports as it
  // closes.
  const [picks, setPicks] = useState<readonly string[]>([]);
  // What the list of several reports as it closes: the picks, and the value
  // as the list opened, while it is open, which events read as they come.
  const pending = useRef<{ readonly picks: readonly string[]; readonly opened: readonly string[] } | null>(null);

  const now = open && multiple ? picks : value;
  const known = choices ?? [];
  const byValue = new Map(known.map((choice) => [choice.value, choice]));
  // A pick of several that the choices lack shows after them, as it is, so
  // that the user can untick it.
  const listed = multiple ? [...known, ...value.filter((picked) => !byValue.has(picked)).map((picked) => ({ value: picked, label: picked }))] : known;
  const found = matching(listed, query);
  const rows = found.slice(0, MAX_ROWS);
  const pickable = rows.filter((choice) => choice.disabled !== true);
  const marked = !keyboard ? undefined : pickable.some((choice) => choice.value === active) ? active : pickable[0]?.value;
  // Whether a pick may not be unticked, since one fewer would be below `min`.
  const locked = multiple && min > 0 && now.length <= min;
  // How the field shows the pick, or the picks, a value that the choices lack
  // as it is.
  const names = now.map((picked) => byValue.get(picked)?.label ?? picked);
  const shown = multiple ? summary(names, choices === undefined ? undefined : listed.length) : names[0];

  // The list of one opens on the pick, which may lie far down a long list.
  const shownRow = open ? (marked ?? active) : undefined;
  useLayoutEffect(() => {
    if (shownRow !== undefined) list.current?.ownerDocument.getElementById(optionId(listId, shownRow))?.scrollIntoView({ block: "nearest" });
  }, [shownRow, listId]);

  // Opens the list, filtered by what the user typed on the closed field.
  function show(byKeyboard: boolean, typed = ""): void {
    if (open || disabled) return;
    if (multiple) {
      pending.current = { picks: value, opened: value };
      setPicks(value);
    }
    setOpen(true);
    setQuery(typed);
    setKeyboard(byKeyboard);
    setActive(multiple || typed !== "" ? undefined : value[0]);
  }

  // Closes the list, and the list of several reports the picks where they
  // changed.
  function close(): void {
    setOpen(false);
    setQuery("");
    setKeyboard(false);
    setActive(undefined);
    const left = pending.current;
    pending.current = null;
    if (left !== null && !sameList(left.picks, left.opened)) report(left.picks, left.opened);
  }

  // Picks the choice of one, which closes the list, or ticks a choice of
  // several, or unticks it, and the keyboard's row follows it.
  function pick(choice: Choice): void {
    if (choice.disabled === true) return;
    if (!multiple) {
      close();
      if (choice.value !== value[0]) report([choice.value], value);
      return;
    }
    const left = pending.current;
    if (left === null) return;
    const ticked = left.picks.includes(choice.value);
    if (ticked && min > 0 && left.picks.length <= min) return;
    const next = ticked ? left.picks.filter((picked) => picked !== choice.value) : [...left.picks, choice.value];
    pending.current = { picks: next, opened: left.opened };
    setPicks(next);
    setActive(choice.value);
  }

  // Moves the row that the keyboard marks, which the first press shows where
  // the list marks none.
  function move(by: 1 | -1): void {
    if (!keyboard) {
      setKeyboard(true);
      return;
    }
    if (pickable.length === 0) return;
    const at = pickable.findIndex((choice) => choice.value === marked);
    setActive(pickable[(at + by + pickable.length) % pickable.length]?.value);
  }

  function onKeyDown(event: KeyboardEvent<HTMLInputElement>): void {
    if (event.nativeEvent.isComposing) return;
    const row = rows.find((choice) => choice.value === marked);
    if (event.key === "ArrowDown" || event.key === "ArrowUp") {
      event.preventDefault();
      if (open) move(event.key === "ArrowDown" ? 1 : -1);
      else show(true);
    } else if (event.key === "Enter") {
      // An Enter in the field never submits the form that holds it.
      event.preventDefault();
      if (!open) show(true);
      else if (!keyboard) setKeyboard(true);
      else if (row !== undefined) pick(row);
    } else if (event.key === " " && (!open || (multiple && keyboard && query === "" && row !== undefined))) {
      event.preventDefault();
      if (!open) show(true);
      else if (row !== undefined) pick(row);
    } else if (event.key === "Escape" && open) {
      // So that a dialog or a form that holds the field stays as it is.
      event.preventDefault();
      close();
    } else if (!open && event.key.length === 1 && !event.ctrlKey && !event.metaKey && !event.altKey) {
      // Typing on the closed field starts the filter, in place of what the
      // field shows.
      event.preventDefault();
      show(true, event.key);
    }
  }

  const note = error ?? hint;

  return (
    <div className={styles.field}>
      <label htmlFor={id} className={styles.label}>
        {label}
      </label>
      {/* A grid stretches the field to the width of its column, border
          included, since core's stylesheet resets no element's box sizing. */}
      <div ref={box} className="bc:relative bc:grid" style={{ anchorName: anchor }}>
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
          aria-invalid={invalid || error !== undefined ? true : undefined}
          aria-describedby={note === undefined ? undefined : noteId}
          data-autofocus={autofocus || undefined}
          disabled={disabled}
          className={`${styles.box} ${FIELD} bc:pr-8 bc:text-ellipsis`}
          value={open ? query : (shown ?? "")}
          placeholder={open ? (shown ?? placeholder) : placeholder}
          onChange={(event) => {
            if (!open) {
              show(true, event.target.value);
              return;
            }
            setQuery(event.target.value);
            setActive(undefined);
            setKeyboard(true);
          }}
          onClick={() => {
            show(false);
          }}
          onKeyDown={onKeyDown}
          onBlur={close}
        />
        <button
          type="button"
          tabIndex={-1}
          disabled={disabled}
          aria-label={open ? `Close the choices of ${label}` : `Show the choices of ${label}`}
          className="bc:absolute bc:inset-y-0 bc:right-0 bc:flex bc:w-8 bc:cursor-pointer bc:items-center bc:justify-center bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-default bc:disabled:cursor-default"
          // The field keeps the focus, so that leaving it does not close the
          // list before the click toggles it.
          onMouseDown={(event) => {
            event.preventDefault();
          }}
          onClick={() => {
            if (open) close();
            else {
              show(false);
              input.current?.focus();
            }
          }}
        >
          <Chevron open={open} />
        </button>
      </div>
      {open ? (
        <List ref={list} anchor={box} name={anchor}>
          <ul id={listId} role="listbox" aria-label={label} aria-multiselectable={multiple || undefined} className={LISTBOX}>
            {byGroup(rows).map(([group, members]) => (
              <Group key={group ?? ""} id={group === undefined ? undefined : groupId(listId, group)} title={group}>
                {members.map((choice) => {
                  const picked = now.includes(choice.value);
                  return (
                    <Row
                      key={choice.value}
                      choice={choice}
                      id={optionId(listId, choice.value)}
                      marked={choice.value === marked}
                      picked={picked}
                      ticks={multiple}
                      locked={picked && locked}
                      onPick={pick}
                    />
                  );
                })}
              </Group>
            ))}
          </ul>
          <Note choices={choices} empty={empty} found={found.length} rows={rows.length} />
        </List>
      ) : null}
      {note === undefined ? null : (
        <p id={noteId} className={error === undefined ? styles.hint : styles.alert}>
          {note}
        </p>
      )}
    </div>
  );
}

// The list, as a popover in the top layer, by the field in `anchor`, while it
// is drawn.
// A press in it keeps the focus in the field, so that leaving the field does
// not close the list before a click on a row picks the row.
function List({
  ref,
  anchor,
  name,
  children,
}: {
  ref: RefObject<HTMLDivElement | null>;
  anchor: RefObject<HTMLElement | null>;
  name: string;
  children: ReactNode;
}): ReactElement {
  useTopLayer(ref, anchor);
  return (
    <div
      ref={ref}
      popover="manual"
      className={LIST}
      style={{ positionAnchor: name }}
      onMouseDown={(event) => {
        event.preventDefault();
      }}
    >
      {children}
    </div>
  );
}

// Shows the element of `list` in the top layer, where the browser has the
// Popover API, from before the browser paints it, and hides it as it goes.
// Where the browser has no anchor positioning, script places it by the
// element of `anchor`, and follows the field as the page scrolls, as the
// window changes its size and as the list's size changes, as while the user
// filters it.
function useTopLayer(list: RefObject<HTMLElement | null>, anchor: RefObject<HTMLElement | null>): void {
  useLayoutEffect(() => {
    const el = list.current;
    const field = anchor.current;
    if (el === null || field === null) return;
    (el as Partial<Pick<HTMLElement, "showPopover">>).showPopover?.();
    const stop = anchored(el) ? undefined : follow(el, field);
    return () => {
      stop?.();
      try {
        (el as Partial<Pick<HTMLElement, "hidePopover">>).hidePopover?.();
      } catch {
        // The popover was hidden already.
      }
    };
  }, [list, anchor]);
}

// Whether the browser places the list by CSS anchor positioning.
function anchored(el: HTMLElement): boolean {
  return el.ownerDocument.defaultView?.CSS.supports("position-area", "block-end") === true;
}

// Places the list by the field through script, until the returned function
// stops it.
function follow(el: HTMLElement, field: HTMLElement): () => void {
  const doc = el.ownerDocument;
  const win = doc.defaultView ?? window;
  const place = (): void => {
    const box = field.getBoundingClientRect();
    el.style.minWidth = `${String(listWidth(box))}px`;
    const spot = listSpot(box, el.getBoundingClientRect(), { width: win.innerWidth, height: win.innerHeight });
    el.style.left = `${String(Math.round(spot.left))}px`;
    el.style.top = `${String(Math.round(spot.top))}px`;
  };
  place();
  const resizes = new win.ResizeObserver(place);
  resizes.observe(el);
  win.addEventListener("resize", place);
  doc.addEventListener("scroll", place, true);
  return () => {
    resizes.disconnect();
    win.removeEventListener("resize", place);
    doc.removeEventListener("scroll", place, true);
  };
}

// The rows under a group's heading, or without one.
function Group({ id, title, children }: { id: string | undefined; title: string | undefined; children: ReactNode }): ReactElement {
  if (id === undefined || title === undefined) return <>{children}</>;
  return (
    <li role="presentation">
      <div id={id} className={HEADING}>
        {title}
      </div>
      <ul role="group" aria-labelledby={id} className="bc:m-0 bc:list-none bc:p-0">
        {children}
      </ul>
    </li>
  );
}

// A row of the list: the label, the muted detail after it, which gives way
// first where the row is short of room, and the badge.
// The pick of one choice shows in weight 600, and each pick of several with a
// tick, in a slot that every row of the list keeps.
function Row({
  choice,
  id,
  marked,
  picked,
  ticks = false,
  locked = false,
  onPick,
}: {
  choice: Choice;
  id: string;
  marked: boolean;
  picked: boolean;
  ticks?: boolean;
  locked?: boolean;
  onPick: (choice: Choice) => void;
}): ReactElement {
  const off = choice.disabled === true || locked;
  return (
    <li
      id={id}
      role="option"
      data-value={choice.value}
      aria-selected={picked}
      aria-disabled={off || undefined}
      className={`${ROW} ${marked ? "bc:bg-bg-subtle bc:outline bc:outline-1 bc:outline-border-accent" : ""} ${
        choice.disabled === true ? "bc:cursor-default bc:opacity-50" : locked ? "bc:cursor-default" : "bc:cursor-pointer bc:hover:bg-bg-subtle"
      }`}
      onClick={() => {
        onPick(choice);
      }}
    >
      {ticks ? <span className="bc:flex bc:size-3.5 bc:shrink-0 bc:text-bg-accent">{picked ? <Tick /> : null}</span> : null}
      <span data-label="" className={`bc:min-w-0 bc:max-w-full bc:shrink-0 bc:truncate ${!ticks && picked ? "bc:font-semibold" : ""}`}>
        {choice.label}
      </span>
      {choice.detail === undefined || choice.detail === "" ? null : (
        <span className="bc:min-w-0 bc:flex-1 bc:truncate bc:text-[13px] bc:text-text-muted">{choice.detail}</span>
      )}
      {choice.badge === undefined ? null : (
        <span className="bc:ml-auto bc:shrink-0 bc:rounded bc:border bc:border-border-default bc:px-1 bc:text-xs bc:text-text-muted">
          {choice.badge}
        </span>
      )}
    </li>
  );
}

// What the list says below its rows: that its choices are loading, that it
// has none, that the filter keeps none, or how many more the filter keeps
// than the list shows.
function Note({
  choices,
  empty,
  found,
  rows,
}: {
  choices: readonly Choice[] | undefined;
  empty: string;
  found: number;
  rows: number;
}): ReactElement | null {
  const text =
    choices === undefined
      ? "Loading…"
      : choices.length === 0 && found === 0
        ? empty
        : found === 0
          ? "No matches"
          : found > rows
            ? `${count(found - rows)} more. Type to narrow the list.`
            : undefined;
  return text === undefined ? null : <p className={NOTE}>{text}</p>;
}

// The one chevron of the design system, which turns up while the list is
// open.
function Chevron({ open }: { open: boolean }): ReactElement {
  return (
    <svg
      width="12"
      height="12"
      viewBox="0 0 12 12"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.4"
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
      className={`bc:shrink-0 bc:text-text-muted bc:transition-transform ${open ? "bc:rotate-180" : ""}`}
    >
      <polyline points="3 4.5 6 7.5 9 4.5" />
    </svg>
  );
}

// The design system's tick of a pick of several.
function Tick(): ReactElement {
  return (
    <svg
      width="14"
      height="14"
      viewBox="0 0 16 16"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.5"
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
    >
      <path d="M3.5 8.5l3 3 6-7" />
    </svg>
  );
}

// The choices one of whose texts holds the query, in any case: the label, the
// detail, the value, the group, the badge or a keyword, as the design
// system's select filters.
// The query is one text, rather than words, so that "b ·" finds the block b
// by the detail "b · dataset block" alone.
export function matching(choices: readonly Choice[], query: string): Choice[] {
  const needle = query.trim().toLowerCase();
  if (needle === "") return [...choices];
  return choices.filter((choice) =>
    [choice.label, choice.detail ?? "", choice.value, choice.group ?? "", choice.badge ?? "", ...(choice.keywords ?? [])].some((text) =>
      text.toLowerCase().includes(needle),
    ),
  );
}

// The choices by group, in the order in which each group first comes, the
// choices without a group first.
function byGroup(choices: readonly Choice[]): [string | undefined, Choice[]][] {
  const groups = new Map<string | undefined, Choice[]>();
  for (const choice of choices) groups.set(choice.group, [...(groups.get(choice.group) ?? []), choice]);
  const loose = groups.get(undefined);
  groups.delete(undefined);
  return [...(loose === undefined ? [] : [[undefined, loose] as [undefined, Choice[]]]), ...groups];
}

// How the field of several choices shows the picks, by their labels: one or
// two by name, and more as a count with the names after it, or nothing for
// none, for which it shows its placeholder.
// It names the picks alone while the total is unknown, since a count of the
// picks themselves would read as every choice.
// It never says "All": the picks are a list of values, and a value that the
// data gets later is not among them.
export function summary(names: readonly string[], total: number | undefined): string | undefined {
  if (names.length === 0) return undefined;
  if (names.length <= 2 || total === undefined) return names.join(", ");
  return `${count(names.length)} of ${count(total)}: ${names.join(", ")}`;
}

function count(n: number): string {
  return n.toLocaleString("en-US");
}

function sameList(a: readonly string[], b: readonly string[]): boolean {
  return a.length === b.length && a.every((value, i) => value === b[i]);
}

// The anchor name of the field, by which CSS anchor positioning places the
// list, from the select's id, which is unique on the page.
function anchorName(id: string): string {
  return `--bc-select-${safe(id)}`;
}

// The id of a row's element, which aria-activedescendant names, and that of a
// group's heading, which names the group. An id holds no blank, so each
// character but a letter, a digit, "-" and "_" goes in as its code.
function optionId(list: string, value: string): string {
  return `${list}-option-${safe(value)}`;
}

function groupId(list: string, key: string): string {
  return `${list}-group-${safe(key)}`;
}

function safe(text: string): string {
  return text.replace(/[^A-Za-z0-9_-]/g, (c) => `_${c.charCodeAt(0).toString(16)}`);
}

// The classes of the field, beside those of every input, of the list and of
// its parts.
const FIELD = "bc:disabled:cursor-default bc:disabled:opacity-50";
const LIST =
  "bc:select-list bc:rounded-lg bc:border bc:border-border-default bc:bg-bg-raised bc:p-1 bc:font-sans bc:text-sm bc:text-gray-900 bc:shadow-md bc:dark:text-gray-100";
// The box of the rows takes the height of its content as its flex basis, and
// shrinks from there to fit the list's greatest height.
// A basis of 0%, which `flex: 1` gives, leaves it no height at all in the
// WebKit of Safari 26.0.
const LISTBOX = "bc:m-0 bc:min-h-0 bc:list-none bc:overflow-y-auto bc:p-0";
const HEADING =
  "bc:sticky bc:top-0 bc:bg-bg-raised bc:px-2.5 bc:pt-2 bc:pb-1 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wider bc:text-text-muted";
const ROW = "bc:flex bc:min-h-8 bc:items-center bc:gap-2 bc:rounded bc:px-2.5 bc:text-sm";
const NOTE = "bc:m-0 bc:px-2.5 bc:py-2 bc:text-center bc:text-sm bc:italic bc:text-text-muted";
