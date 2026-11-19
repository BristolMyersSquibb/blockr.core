// The list of core's React SDK in place: React Aria's ListBox
// (https://react-aria.adobe.com/ListBox), in core's theme.
// The list is the control itself, always open, where the select opens its
// list from a field (./combobox.tsx).
// It is drawn after the ListBox of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/ListBox.tsx),
// and takes what that takes: the props of React Aria's ListBox, but its
// layout and its orientation.
// Its rows are ListBoxItem below, which are the rows of the select's list.
// So a list in place reads as the select's list does: a tick before each pick
// of several, and the pick of one in weight 600.
// The list is a box as wide as the space it stands in, and its rows scroll
// within a height of 18rem.
// It draws its rows through React Aria's Virtualizer, as the select's list
// does.
// The Virtualizer puts on the page only the rows in view.
// React Aria's Autocomplete around it, with the SDK's SearchField
// (./searchfield.tsx), narrows its rows to those whose text holds what the
// user types into the field.
// The Autocomplete filters by the function it takes, such as the `contains`
// of React Aria's useFilter().
// The list says "Loading…" while it has no items yet, or while its items are
// `outdated` and there are none, "No matches" where the filter keeps none of
// them, and otherwise what `renderEmptyState` says, "No options" by default.
//
// Given `bound`, it shows and edits a block's state arg or a board option's
// value, which useValue() binds, as the SDK's other inputs do (./inputs.tsx):
// one string, or an array of strings where its selectionMode is "multiple",
// by the ids of its rows.
// One pick it submits as the user picks it.
// As the user unpicks it, the list unsets the state arg, as the button of the
// select that clears the pick does.
// Several it hands to the binding's type() as the user picks them, as a text
// field hands it what the user types.
// So the binding submits them once the user pauses, as one edit and one undo
// step, and at once where the list goes first, as where its module unmounts.
// The list hands them as a function of the value that the host holds, which
// the binding calls as it submits, as the select builds its value as its list
// closes.
// So the list keeps a string that the assistant added meanwhile.
// It submits the picks in the order the user picked them, after those the
// value holds, or where `ordered`, in the order of its items.
// It shows the picks of the user until the binding shows the value again.
// A value that its rows lack, such as a column that the data no longer has,
// it shows as a row of its own before them, picked, which the user unpicks to
// drop the value.
// Such a row says that the value is not available, and the list marks itself
// invalid, but not while it is `outdated`.
// It knows its rows by the ids of its items, or of the rows among its
// children, so a bound list draws no sections.
// While it has no items yet, as `items` is undefined while its children draw
// an item, it shows no value.
// It says below itself why the host refused what it submitted.

import { Children, isValidElement, useState } from "react";
import type { ReactElement } from "react";
import { strings } from "../binding/index.ts";
import type { Bound } from "../react/index.ts";
import type { Json } from "../types/json.ts";
import { Collection, composeRenderProps, ListBox as AriaListBox, ListLayout, Virtualizer } from "../aria/index.ts";
import type { ListBoxItemProps, ListBoxProps as AriaListBoxProps } from "../aria/index.ts";
import { ComboBoxItem, edited, emptyNote, ids, inOrder, LAYOUT } from "./combobox.tsx";
import { missing, Refusal } from "./inputs.tsx";

export interface ListBoxProps<T extends object> extends Omit<AriaListBoxProps<T>, "layout" | "orientation"> {
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
  readonly outdated?: boolean;
  readonly ordered?: boolean;
}

export function ListBox<T extends object>(props: ListBoxProps<T>): ReactElement {
  const { bound, ordered = false, ...rest } = props;
  if (bound === undefined) return <Drawn {...(rest as DrawnProps)} />;
  return props.selectionMode === "multiple" ? (
    <BoundSeveral {...(rest as DrawnProps)} bound={bound} ordered={ordered} />
  ) : (
    <BoundOne {...(rest as DrawnProps)} bound={bound} />
  );
}

// What the list draws with, of either mode: the values that its rows lack,
// which it draws as rows of their own before them, and whether it marks them
// as missing.
type DrawnProps = Omit<ListBoxProps<object>, "bound" | "ordered"> & {
  readonly lacked?: readonly string[];
  readonly marking?: boolean;
};

function BoundOne({ bound, outdated = false, onSelectionChange, ...props }: DrawnProps & { readonly bound: Bound }): ReactElement {
  const rows = rowIds(props);
  const picks = rows !== undefined && typeof bound.value === "string" ? [bound.value] : [];
  return (
    <>
      <Drawn
        {...props}
        selectionMode="single"
        outdated={outdated}
        marking={!outdated}
        lacked={lacking(picks, rows)}
        selectedKeys={picks}
        onSelectionChange={(keys) => {
          const [key] = keys === "all" ? [] : [...keys];
          void (key === undefined ? bound.unset() : bound.commit(String(key)));
          onSelectionChange?.(keys);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// The picks that the binding has yet to submit, and the value as the user
// began them.
interface Burst {
  readonly picks: readonly string[];
  readonly began: readonly string[];
}

function BoundSeveral({
  bound,
  outdated = false,
  ordered,
  onSelectionChange,
  ...props
}: DrawnProps & { readonly bound: Bound; readonly ordered: boolean }): ReactElement {
  const rows = rowIds(props);
  const held = strings(bound.value);
  // The binding shows nothing while it waits to submit, so a value that it
  // shows ends the burst, as the host took it or announced another.
  const [burst, setBurst] = useState<Burst | null>(null);
  const [seen, setSeen] = useState<Json | undefined>(bound.value);
  if (!Object.is(seen, bound.value)) {
    setSeen(bound.value);
    setBurst(null);
  }
  const picks = rows === undefined ? [] : (burst?.picks ?? held);
  return (
    <>
      <Drawn
        {...props}
        selectionMode="multiple"
        outdated={outdated}
        marking={!outdated}
        lacked={lacking(picks, rows)}
        selectedKeys={picks}
        onSelectionChange={(keys) => {
          // React Aria picks every row, as on Ctrl+A, as "all".
          const next = keys === "all" ? [...picks, ...(rows ?? []).filter((id) => !picks.includes(id))] : [...keys].map(String);
          const began = burst?.began ?? held;
          setBurst({ picks: next, began });
          // Where the user unpicked what they picked, the function gives
          // undefined, so that the binding submits nothing.
          bound.type((now) => {
            const value = edited(strings(now), began, next);
            return value === null ? undefined : ordered && rows !== undefined ? inOrder(value, rows) : value;
          });
          onSelectionChange?.(keys);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// The ids of the rows that the list draws: those of its items, as React Aria
// keys an item by its id, or those of the rows among its children.
// It gives none while the list has no items yet, as `items` is undefined
// while its children draw an item.
function rowIds({ items, children }: DrawnProps): readonly string[] | undefined {
  if (items !== undefined) return ids(items);
  if (typeof children === "function") return undefined;
  return Children.toArray(children).flatMap((child) => {
    const id = isValidElement<{ id?: unknown }>(child) ? child.props.id : undefined;
    return typeof id === "string" || typeof id === "number" ? [String(id)] : [];
  });
}

// The picks that the rows lack, where the list knows its rows.
function lacking(picks: readonly string[], rows: readonly string[] | undefined): readonly string[] {
  return rows === undefined ? [] : picks.filter((pick) => !rows.includes(pick));
}

// The rows of the values that the items lack, then those of the items, and
// what the list says where it shows no row.
function Drawn({
  children,
  items,
  dependencies,
  renderEmptyState,
  outdated = false,
  marking = false,
  lacked = [],
  className,
  ...props
}: DrawnProps): ReactElement {
  // Whether it has no items yet, as its children draw an item with none given.
  const unknown = items === undefined && typeof children === "function";
  return (
    <Virtualizer layout={ListLayout} layoutOptions={LAYOUT}>
      <AriaListBox
        {...props}
        data-invalid={(marking && lacked.length > 0) || undefined}
        className={composeRenderProps(className, (given) => `${LIST} ${given ?? ""}`)}
        renderEmptyState={(values) => emptyNote(items, children, outdated, () => renderEmptyState?.(values))}
      >
        {lacked.map((value) => (
          <ComboBoxItem key={value} id={value} textValue={marking ? missing(value) : value} data-missing={marking || undefined}>
            {marking ? <span className={MISSING}>{missing(value)}</span> : value}
          </ComboBoxItem>
        ))}
        <Collection items={unknown ? [] : items} dependencies={dependencies}>
          {children}
        </Collection>
      </AriaListBox>
    </Virtualizer>
  );
}

// A row of the list: a row of the select's list, after a tick where the list
// picks several, in weight 600 where it picks one and the row is the pick.
// A description, which a child Text in the slot "description" holds, shows
// muted after it.
export function ListBoxItem(props: ListBoxItemProps): ReactElement {
  return <ComboBoxItem {...props} />;
}

// The classes of the list and of the row of a value that the items lack.
// The list is a box like core's other inputs, which shows in the colour of
// danger where it marks a value that the items lack, through the attribute
// that marks it, whose class wins over that of its default colour.
// The Virtualizer lays the rows out inside a padding of 4 pixels, as in the
// select's list, so the box holds none of it in its CSS.
const LIST =
  "bc:box-border bc:max-h-72 bc:min-w-0 bc:justify-self-stretch bc:overflow-y-auto bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-invalid:border-border-danger";
const MISSING = "bc:text-text-danger";
