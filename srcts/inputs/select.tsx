// The select of core's React SDK that has no field to type into: React Aria's
// Select (https://react-aria.adobe.com/Select), in core's theme, for a pick
// of a few values, which a list shows whole, where the SDK's ComboBox
// (./combobox.tsx) filters a long list by what the user types.
// It picks one item, or several where its selectionMode is "multiple", out
// of a list that opens from its button.
// It is drawn after the Select of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Select.tsx),
// and takes what that takes: the props of React Aria's Select, and the label,
// the description, the error message and the items that the starter kit adds.
// Its items are SelectItem and SelectSection below, the rows of the
// ComboBox's list.
// Its button shows the pick, or the picks of several, and the placeholder
// while there is none.
// React Aria draws the list in a portal at the end of the page's body, above
// a modal of the SDK that holds the select, as every overlay of the SDK sits
// in such a portal (../overlays/index.ts).
//
// Given `bound`, it shows and edits a value that useValue() binds, a block's
// state arg, a board option's value or a value that a function reads from a
// block's state, as the SDK's other inputs do (./inputs.tsx): one string, or
// an array of strings where it picks several, by the ids of its items.
// It submits as the ComboBox does: one pick as the user picks it, and several
// once, as the list closes, built from the value that the host holds then, in
// the order the user picked them, after those the value holds, or where
// `ordered`, in the order of its items.
// It shows a value that its items lack as a row of its own, before them,
// picked and marked as missing, which the user unpicks, or picks another in
// place of, to drop the value.
// Its button then shows the value as missing too, and the select marks itself
// invalid.
// While it is `outdated`, and while it has no items yet, as `items` is
// undefined while its children draw an item, it shows such a value as it is,
// as the ComboBox does.
// It says below itself why the host refused what it submitted.

import { useContext, useLayoutEffect, useRef, useState } from "react";
import type { ReactElement, ReactNode } from "react";
import { strings } from "../binding/index.ts";
import {
  Button,
  Collection,
  composeRenderProps,
  FieldError,
  Label,
  ListBox,
  Popover,
  Select as AriaSelect,
  SelectStateContext,
  SelectValue,
  Text,
} from "../aria/index.ts";
import type { Key, ListBoxItemProps, SelectProps as AriaSelectProps, ValidationResult } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import { ComboBoxItem, ComboBoxSection, useSameValue } from "./combobox.tsx";
import type { ComboBoxSectionProps } from "./combobox.tsx";
import { Chevron } from "./icons.tsx";
import { missing, Refusal } from "./inputs.tsx";
import { inOrder, merged } from "./items.ts";

type Mode = "single" | "multiple";

export interface SelectProps<T extends object, M extends Mode = "single"> extends Omit<AriaSelectProps<T, M>, "children"> {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  readonly items?: Iterable<T>;
  readonly children: ReactNode | ((item: T) => ReactNode);
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
  readonly outdated?: boolean;
  readonly ordered?: boolean;
}

export function Select<T extends object, M extends Mode = "single">(props: SelectProps<T, M>): ReactElement {
  const { bound, outdated = false, ordered = false, ...rest } = props;
  if (bound === undefined) return <Drawn {...(rest as DrawnProps)} />;
  return <BoundSelect {...(rest as DrawnProps)} bound={bound} outdated={outdated} ordered={ordered} />;
}

// A row of the list, as a row of the ComboBox's list.
export function SelectItem(props: ListBoxItemProps): ReactElement {
  return <ComboBoxItem {...props} />;
}

export type SelectSectionProps<T> = ComboBoxSectionProps<T>;

// A group of rows under a heading.
export function SelectSection<T extends object>(props: SelectSectionProps<T>): ReactElement {
  return <ComboBoxSection {...props} />;
}

type DrawnProps = Omit<SelectProps<object, Mode>, "bound" | "outdated" | "ordered"> & {
  // The rows of the values that the items lack, each by its key, before the
  // items, and whether each shows its value as missing, and what reads the
  // ids of the items.
  readonly gone?: readonly { readonly key: string; readonly text: string; readonly marked: boolean }[];
  readonly onKnown?: (ids: readonly string[]) => void;
};

// The key of the row of a value that the items lack, which no item's id
// takes, so that the row never stands in for an item that has the value as
// its id once it comes.
const GONE = "\u0000gone\u0000";

function valueOf(key: Key): string {
  const id = String(key);
  return id.startsWith(GONE) ? id.slice(GONE.length) : id;
}

function BoundSelect({
  bound,
  outdated,
  ordered,
  onChange,
  onOpenChange,
  ...props
}: DrawnProps & { readonly bound: Bound; readonly outdated: boolean; readonly ordered: boolean }): ReactElement {
  const several = props.selectionMode === "multiple";
  const held = several ? strings(bound.value) : typeof bound.value === "string" ? [bound.value] : [];
  // The ids of the items, as React Aria holds them, null until it has, and
  // while the items are unknown, as while the select's children draw an item
  // with none given.
  const [known, setKnown] = useState<readonly string[] | null>(null);
  const unknown = props.items === undefined && typeof props.children === "function";
  const ids = unknown ? null : known;
  // The picks of several while the list is open, and the value as it opened,
  // which the list submits as it closes, and which events read as they come.
  const [picks, setPicks] = useState<readonly string[] | null>(null);
  const pending = useRef<{ readonly picks: readonly string[]; readonly opened: readonly string[] } | null>(null);
  const shown = picks ?? held;
  // The values that have a row of their own: those that the items lack, and
  // every value until the items are known, which shows as it is.
  // The rows stay while the list is open, whether the user unpicks them or
  // not.
  const lacked = ids === null ? held : held.filter((value) => !ids.includes(value));
  const marking = !outdated && ids !== null && lacked.length > 0;
  const keyOf = (value: string): string => (lacked.includes(value) ? `${GONE}${value}` : value);

  const submit = (next: readonly string[], opened: readonly string[]): void => {
    const value = merged(strings(bound.get()), opened, next);
    if (value === null) return;
    void bound.commit(ordered && ids !== null ? inOrder(value, ids) : value);
  };

  const common = {
    ...props,
    isInvalid: props.isInvalid === true || marking,
    gone: lacked.map((value) => ({ key: `${GONE}${value}`, text: marking ? missing(value) : value, marked: marking })),
    onKnown: (next: readonly string[]) => {
      setKnown((last) => (last !== null && sameIds(last, next) ? last : next));
    },
  };

  if (!several) {
    return (
      <>
        <Drawn
          {...common}
          selectionMode="single"
          value={held[0] === undefined ? null : keyOf(held[0])}
          onChange={(key) => {
            if (key !== null && !Array.isArray(key)) void bound.commit(valueOf(key));
            onChange?.(key);
          }}
          onOpenChange={onOpenChange}
        />
        <Refusal error={bound.error} />
      </>
    );
  }
  return (
    <>
      <Drawn
        {...common}
        selectionMode="multiple"
        value={shown.map(keyOf)}
        onOpenChange={(isOpen) => {
          if (isOpen) {
            pending.current ??= { picks: held, opened: held };
            setPicks(pending.current.picks);
          } else {
            const left = pending.current;
            pending.current = null;
            setPicks(null);
            if (left !== null) submit(left.picks, left.opened);
          }
          onOpenChange?.(isOpen);
        }}
        onChange={(keys) => {
          if (Array.isArray(keys)) {
            const next = (keys as readonly Key[]).map(valueOf);
            if (pending.current === null) {
              submit(next, held);
            } else {
              pending.current = { ...pending.current, picks: next };
              setPicks(next);
            }
          }
          onChange?.(keys);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

function sameIds(a: readonly string[], b: readonly string[]): boolean {
  return a.length === b.length && a.every((id, i) => id === b[i]);
}

// The label, the button, the description, the error message and the list.
function Drawn({ label, description, errorMessage, items, children, gone = [], onKnown, className, ...props }: DrawnProps): ReactElement {
  const value = useSameValue(props.value);
  return (
    <AriaSelect
      {...props}
      value={value}
      className={composeRenderProps(className, (given) => `bc:group ${styles.field} ${given ?? ""}`)}
    >
      {label === undefined ? null : <Label className={styles.label}>{label}</Label>}
      <Button className={TRIGGER}>
        <SelectValue className={VALUE}>
          {({ selectedText, defaultChildren, isPlaceholder }) => (isPlaceholder ? defaultChildren : selectedText || defaultChildren)}
        </SelectValue>
        <Chevron />
      </Button>
      {/* React Aria names the description of the select only where the
          element is there from the start, so it is there while empty too. */}
      <Text slot="description" className={`${styles.hint} bc:empty:hidden`}>
        {description}
      </Text>
      <FieldError className={styles.alert}>{errorMessage}</FieldError>
      <Popover offset={4} className={LIST}>
        <ListBox renderEmptyState={() => <p className={NOTE}>No options</p>} className={LISTBOX}>
          {gone.map(({ key, text, marked }) => (
            <SelectItem key={key} id={key} textValue={text} data-missing={marked || undefined}>
              {text}
            </SelectItem>
          ))}
          {items === undefined ? (typeof children === "function" ? null : children) : <Collection items={items}>{children}</Collection>}
        </ListBox>
      </Popover>
      {onKnown === undefined ? null : <Known onKnown={onKnown} />}
    </AriaSelect>
  );
}

// Hands `onKnown` the ids of the items in the select's collection, in their
// order, without the rows of values that the items lack, each time the select
// has drawn.
// React Aria draws the select's children a second time, hidden, as it builds
// the collection, where no state of the select is there to read, and that
// draw hands nothing.
function Known({ onKnown }: { onKnown: (ids: readonly string[]) => void }): null {
  const state = useContext(SelectStateContext);
  useLayoutEffect(() => {
    if (state === null) return;
    const { collection } = state;
    const ids = [...collection.getKeys()].flatMap((key) =>
      collection.getItem(key)?.type === "item" && !String(key).startsWith(GONE) ? [String(key)] : [],
    );
    onKnown(ids);
  });
  return null;
}

// The classes of the button, of the pick it shows and of the list.
// The button is a box like core's other inputs, which shows the pick and the
// chevron, and takes the colour of danger where the select is invalid, as
// where it shows a value that its items lack.
// A row of such a value takes that colour too.
const TRIGGER =
  "bc:flex bc:min-h-[30px] bc:w-full bc:min-w-0 bc:cursor-pointer bc:items-center bc:gap-2 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:py-1 bc:pr-1.5 bc:pl-2 bc:text-left bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-1 bc:data-focus-visible:outline-border-accent bc:group-data-invalid:border-border-danger bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
const VALUE =
  "bc:min-w-0 bc:flex-1 bc:truncate bc:data-placeholder:text-text-muted bc:group-data-invalid:text-text-danger";
const LIST =
  "bc:box-border bc:min-w-(--trigger-width) bc:overflow-hidden bc:rounded-lg bc:bg-bg-raised bc:font-sans bc:text-sm bc:text-gray-900 bc:shadow-md bc:outline-none bc:dark:text-gray-100";
const LISTBOX =
  "bc:box-border bc:max-h-[inherit] bc:overflow-y-auto bc:rounded-lg bc:border bc:border-border-default bc:p-1 bc:outline-none bc:[&_[data-missing]]:text-text-danger";
const NOTE = "bc:m-0 bc:px-2.5 bc:py-2 bc:text-center bc:text-sm bc:italic bc:text-text-muted";
