// The one select of core's React SDK, which core's own UIs, frontends, the UIs
// of plugins and those of blocks all draw with: React Aria's ComboBox
// (https://react-aria.adobe.com/ComboBox), in core's theme.
// It picks one item, or several where its selectionMode is "multiple", out of
// a list that the user filters by typing into its field.
// It is drawn after the ComboBox of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/ComboBox.tsx),
// and takes what that takes: the props of React Aria's ComboBox, and the
// label, the description, the error message and the placeholder that the
// starter kit adds.
// It hands its items to React Aria's ComboBox, as the starter kit of React
// Aria's docs does, so that React Aria filters `defaultItems` by what the user
// types, and leaves `items` to its caller to filter.
// It takes the renderEmptyState of React Aria's ListBox too, for what its
// list says where it has no item at all.
// Its items are ComboBoxItem and ComboBoxSection below, as in the starter kit.
//
// The field of several shows each pick as a chip, whose button removes it,
// as Delete or Backspace on the chip does, and Backspace in the empty field
// removes the last one.
// The chips drag into order, as the items of React Aria's GridList do
// (https://react-aria.adobe.com/GridList#drag-and-drop), but where the select
// is `ordered`, or not `reorderable`.
// A pointer drags a chip by its grip or its text.
// A keyboard user moves a chip in four steps:
// 1. The user tabs into the chips, and moves to the chip with the arrow keys.
// 2. Tab takes the focus to the grip at the chip's start, where Enter starts
//    the drag.
// 3. The arrow keys move the point where the chip would go.
// 4. Enter puts the chip there, and Escape leaves it where it was.
// The select then calls onChange with the picks in their new order.
// A button at the field's right, before the one that opens the list, clears
// the pick, or every pick, where there is one, but where the select
// `isRequired`.
// Bound, a select of one unsets its state arg so, as a block's new select of
// an arg without a default starts.
// A press in the field opens the list, as typing into it does.
// The list hangs below the field, or above it where the window has no room
// below, as wide as the field.
// React Aria draws it in a portal at the end of the page's body.
// In a dialog or a popover, which sit in the browser's top layer, above the
// body, and of which a modal dialog leaves the rest of the page inert, it
// draws it inside the select instead, in a layer fixed to the window, which
// the dialog's own scroll does not clip.
// The list says "Loading…" while it has no items yet, or while its items are
// `outdated` and there are none, "No matches" where React Aria's filter keeps none of `defaultItems`,
// and otherwise what `renderEmptyState` says, "No options" by default, as
// wherever the `items` that its caller filters are none.
//
// Given `bound`, it shows and edits a block's state arg or a board option's
// value, which useValue() binds, as the SDK's other inputs do
// (./inputs.tsx): one string, or an array of strings where it picks several,
// by the ids of its items.
// It shows a value that its items lack, such as a column that the data no
// longer has, as missing, and marks itself invalid.
// While it has no items yet, as `items` and `defaultItems` are undefined while
// its children draw an item, it shows a value as it is.
// While it is `outdated`, as while the request that gives its items asks
// again, the items may be for older data.
// It then shows a value that they lack as it is too.
// So its items must hold every value it takes, not a part of them.
// One pick it submits as the user picks it.
// Several it submits once, as the list closes, built from the value that the
// host holds then, so that it keeps a string that the assistant added in the
// meantime.
// It submits them in the order the user picked them, after those the value
// holds, or where `ordered`, in the order of its items, as for severities.
// The strings of the value that the user kept take the order that the user
// gave their chips.
// Where the select goes while its list is open, as where its module
// unmounts, the picks go with it, as though the user never made them.
// A chip that the user removes or drags while the list is closed it submits
// at once.
// A press on a chip takes the focus from the text field, which closes the
// list, so a chip only ever moves while the list is closed.
// It says below itself why the host refused what it submitted.

import { useContext, useRef, useState } from "react";
import type { KeyboardEvent, ReactElement, ReactNode, RefObject } from "react";
import { strings } from "../binding/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import {
  Button,
  Collection,
  ComboBox as AriaComboBox,
  ComboBoxStateContext,
  composeRenderProps,
  DropIndicator,
  FieldError,
  GridList,
  GridListItem,
  Group,
  Header,
  Input,
  Label,
  ListBox,
  ListBoxItem,
  ListBoxSection,
  Popover,
  Text,
  UNSAFE_PortalProvider,
  useDragAndDrop,
} from "../aria/index.ts";
import type {
  ComboBoxProps as AriaComboBoxProps,
  Key,
  ListBoxItemProps,
  ListBoxSectionProps,
  ValidationResult,
} from "../aria/index.ts";
import { missing, Refusal } from "./inputs.tsx";

type Mode = "single" | "multiple";

export interface ComboBoxProps<T extends object, M extends Mode = "single">
  extends Omit<AriaComboBoxProps<T, M>, "children"> {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string | null;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  readonly placeholder?: string;
  readonly children: ReactNode | ((item: T) => ReactNode);
  // What the list says where it has no item at all, as in React Aria's
  // ListBox.
  readonly renderEmptyState?: () => ReactNode;
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
  readonly outdated?: boolean;
  readonly ordered?: boolean;
  // Where it picks several, whether the user drags the chips into order, as
  // by default.
  // A select that is `ordered` keeps the order of its items instead.
  readonly reorderable?: boolean;
}

export function ComboBox<T extends object, M extends Mode = "single">(props: ComboBoxProps<T, M>): ReactElement {
  const { bound, ordered = false, reorderable = true, ...rest } = props;
  const drawn = { ...(rest as DrawnProps), reorderable: reorderable && !ordered };
  if (bound === undefined) return <Drawn {...drawn} />;
  return props.selectionMode === "multiple" ? (
    <BoundSeveral {...drawn} bound={bound} ordered={ordered} />
  ) : (
    <BoundOne {...drawn} bound={bound} />
  );
}

// What the select draws with, of either mode, whether it marks a value that
// its items lack as missing, and whether its field may offer to clear the
// pick.
type DrawnProps = Omit<ComboBoxProps<object, Mode>, "bound" | "ordered"> & {
  readonly marking?: boolean;
  // What clears the pick in place of the select's own clearing, as where a
  // binding unsets the state arg.
  readonly onClear?: () => void;
};

function BoundOne({ bound, outdated = false, ...props }: DrawnProps & { readonly bound: Bound }): ReactElement {
  const current = typeof bound.value === "string" ? bound.value : null;
  return (
    <>
      <Drawn
        {...props}
        selectionMode="single"
        outdated={outdated}
        marking={!outdated && known(props)}
        onClear={() => {
          void bound.unset();
        }}
        value={current}
        onChange={(key) => {
          if (key !== null && !Array.isArray(key)) void bound.commit(String(key));
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

function BoundSeveral({
  bound,
  outdated = false,
  ordered,
  onOpenChange,
  ...props
}: DrawnProps & { readonly bound: Bound; readonly ordered: boolean }): ReactElement {
  const items = props.items ?? props.defaultItems;
  const held = strings(bound.value);
  // The picks while the list is open, and the value as it opened, which the
  // list submits as it closes, and which events read as they come.
  const [picks, setPicks] = useState<readonly string[] | null>(null);
  const pending = useRef<{ readonly picks: readonly string[]; readonly opened: readonly string[] } | null>(null);

  // Builds the submit from the value that the host holds now: the strings the
  // user picked since the list opened, after those the value holds, without
  // those the user removed.
  // The strings that the user kept take the places that they hold in the
  // value now, in the order that the user gave them, while a string that the
  // assistant added in the meantime keeps its place.
  const submit = (next: readonly string[], opened: readonly string[]): void => {
    if (sameKeys(next, opened)) return;
    const now = strings(bound.get());
    const removed = opened.filter((value) => !next.includes(value));
    const added = next.filter((value) => !opened.includes(value) && !now.includes(value));
    const kept = next.filter((value) => opened.includes(value) && now.includes(value));
    const order = [...kept];
    const held = now.filter((value) => !removed.includes(value)).map((value) => (kept.includes(value) ? (order.shift() ?? value) : value));
    const value = ordered && items !== undefined ? inOrder([...held, ...added], ids(items)) : [...held, ...added];
    if (!sameKeys(value, now)) void bound.commit(value);
  };

  return (
    <>
      <Drawn
        {...props}
        selectionMode="multiple"
        outdated={outdated}
        marking={!outdated && known(props)}
        value={picks ?? held}
        onOpenChange={(isOpen, trigger) => {
          if (isOpen) {
            pending.current ??= { picks: held, opened: held };
            setPicks(pending.current.picks);
          } else {
            const left = pending.current;
            pending.current = null;
            setPicks(null);
            if (left !== null) submit(left.picks, left.opened);
          }
          onOpenChange?.(isOpen, trigger);
        }}
        onChange={(keys) => {
          if (!Array.isArray(keys)) return;
          const next = keys.map(String);
          if (pending.current === null) {
            submit(next, held);
            return;
          }
          pending.current = { ...pending.current, picks: next };
          setPicks(next);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// Whether the select has its items, rather than a function that draws an item
// with no items to draw yet.
function known(props: DrawnProps): boolean {
  return (props.items ?? props.defaultItems) !== undefined || typeof props.children !== "function";
}

// The ids of the items, as React Aria keys an item by its id.
function ids(items: Iterable<object>): string[] {
  return [...items].flatMap((item) => {
    const id = (item as { id?: unknown }).id;
    return typeof id === "string" || typeof id === "number" ? [String(id)] : [];
  });
}

// The values in the order of the ids, those that the ids lack after them.
function inOrder(values: readonly string[], order: readonly string[]): string[] {
  return [...order.filter((id) => values.includes(id)), ...values.filter((value) => !order.includes(value))];
}

// The label, the field, the description, the error message and the list.
function Drawn({
  label,
  description,
  errorMessage,
  placeholder,
  children,
  renderEmptyState,
  outdated = false,
  marking = false,
  onClear,
  reorderable = true,
  className,
  allowsEmptyCollection = true,
  ...props
}: DrawnProps): ReactElement {
  const multiple = props.selectionMode === "multiple";
  const editable = props.isDisabled !== true && props.isReadOnly !== true;
  const given = props.items ?? props.defaultItems;
  // Whether it has no items yet, as its children draw an item with none given.
  const unknown = given === undefined && typeof children === "function";
  const none = given === undefined ? typeof children === "function" || !hasChildren(children) : [...given].length === 0;
  const empty = none && (outdated || unknown) ? "Loading…" : none ? (renderEmptyState?.() ?? "No options") : "No matches";
  const field = useRef<HTMLDivElement>(null);
  const layer = useRef<HTMLDivElement>(null);
  const value = useSameValue(props.value);
  return (
    <AriaComboBox
      {...props}
      value={value}
      defaultItems={unknown ? [] : props.defaultItems}
      allowsEmptyCollection={allowsEmptyCollection}
      className={composeRenderProps(className, (given) => `bc:group ${styles.field} ${given ?? ""}`)}
    >
      {label === undefined ? null : <Label className={styles.label}>{label}</Label>}
      <Field
        ref={field}
        placeholder={placeholder}
        multiple={multiple}
        marking={marking}
        clearable={props.isRequired !== true && editable}
        reorderable={multiple && reorderable}
        isDisabled={!editable}
        onClear={onClear}
        label={label}
        autoFocus={props.autoFocus}
      />
      {/* React Aria names the description of the field only where the
          element is there from the start, so it is there while empty too. */}
      <Text slot="description" className={`${styles.hint} bc:empty:hidden`}>
        {description}
      </Text>
      <FieldError className={styles.alert}>{errorMessage}</FieldError>
      <div ref={layer} className={LAYER} />
      <UNSAFE_PortalProvider getContainer={() => container(field.current, layer.current)}>
        <Popover offset={4} className={LIST}>
          <ListBox renderEmptyState={() => <p className={NOTE}>{empty}</p>} className={LISTBOX}>
            {children}
          </ListBox>
        </Popover>
      </UNSAFE_PortalProvider>
    </AriaComboBox>
  );
}

// The value given, or the array of the keys that the select drew before where
// it holds the same keys.
// React Aria takes a new array of picks for a change of the value, and then
// empties the field, so that a caller that builds the picks afresh as it draws
// would empty the field as the user types into it.
function useSameValue<V>(value: V): V {
  const [drawn, setDrawn] = useState(value);
  const same = drawn === value || (Array.isArray(drawn) && Array.isArray(value) && sameKeys(drawn, value));
  if (!same) setDrawn(value);
  return same ? drawn : value;
}

function sameKeys(a: readonly unknown[], b: readonly unknown[]): boolean {
  return a.length === b.length && a.every((key, i) => key === b[i]);
}

// Where React Aria draws the list of the select whose field is given: in the
// select's layer where a dialog or a popover holds the field, else at the end
// of the body.
function container(field: HTMLElement | null, layer: HTMLElement | null): HTMLElement | null {
  return field?.closest("dialog, [popover]") ? layer : (field?.ownerDocument.body ?? null);
}

function hasChildren(children: ReactNode): boolean {
  return Array.isArray(children) ? children.some((child) => hasChildren(child as ReactNode)) : children !== null && children !== undefined && children !== false;
}

// The field: the chips of the picks of several, the text field, which takes
// the filter, and the button that opens and closes the list.
// It shows a pick of one that the items lack in place of the placeholder, as
// missing where `marking`, and marks itself invalid.
// The text field of one shows the pick's text while the user is not in it,
// as React draws the pick, where React Aria would set it a step later, from
// an effect, so that the controls show a state as the host announces it, with
// no wait for React (../controls/controls.tsx).
function Field({
  ref,
  placeholder,
  multiple,
  marking,
  clearable,
  reorderable,
  isDisabled,
  onClear,
  label,
  autoFocus,
}: {
  readonly ref: RefObject<HTMLDivElement | null>;
  readonly placeholder: string | undefined;
  readonly multiple: boolean;
  readonly marking: boolean;
  readonly clearable: boolean;
  // Whether the chips drag into order, and whether no chip drags for now, as
  // while the select is disabled or read-only.
  readonly reorderable: boolean;
  readonly isDisabled: boolean;
  readonly onClear: (() => void) | undefined;
  readonly label: string | undefined;
  readonly autoFocus: boolean | undefined;
}): ReactElement {
  const state = useContext(ComboBoxStateContext);
  const value = state?.value ?? null;
  const keys = multiple && Array.isArray(value) ? (value as readonly Key[]) : [];
  const one = !multiple && value !== null && !Array.isArray(value) ? (value as Key) : null;
  // The item of the pick of one, where the items hold it.
  const item = state?.selectedItems[0] ?? null;
  const lackedOne = one !== null && item === null;
  const known = new Map((state?.selectedItems ?? []).map((item) => [item.key, item.textValue]));
  const lacked = keys.some((key) => !known.has(key));
  const invalid = marking && (lackedOne || lacked);
  return (
    <Group ref={ref} isInvalid={invalid} className={FIELD}>
      <div className={VALUES}>
        {keys.length === 0 ? null : (
          <Chips
            keys={keys}
            known={known}
            marking={marking}
            reorderable={reorderable}
            isDisabled={isDisabled}
            label={label}
            onChange={(next) => {
              state?.setValue(next);
            }}
          />
        )}
        <Input
          value={multiple || state === null || state.isFocused || state.isOpen ? undefined : (item?.textValue ?? "")}
          placeholder={lackedOne ? (marking ? missing(String(one)) : String(one)) : keys.length > 0 ? undefined : placeholder}
          aria-invalid={invalid || undefined}
          data-autofocus={autoFocus === true || undefined}
          className={INPUT}
          onClick={() => {
            if (state !== null && !state.isOpen) state.open(null, "manual");
          }}
          onKeyDown={(event: KeyboardEvent<HTMLInputElement>) => {
            // An Escape that closes the list closes nothing else, such as the
            // dialog or the form that holds the select, which React Aria lets
            // it reach.
            if (event.key === "Escape" && state?.isOpen === true) event.preventDefault();
            if (event.key !== "Backspace" || event.currentTarget.value !== "" || keys.length === 0) return;
            state?.setValue(keys.slice(0, -1));
          }}
        />
      </div>
      {clearable && (keys.length > 0 || one !== null) ? (
        // A button of the field's own, not React Aria's, which the select
        // would take for its own and open the list with. It keeps the focus
        // in the text field, as a press in the list does.
        <button
          type="button"
          tabIndex={-1}
          aria-label={label === undefined ? "Clear" : `Clear ${label}`}
          title={multiple ? "Clear every pick" : "Clear the pick"}
          className={CLEAR}
          onMouseDown={(event) => {
            event.preventDefault();
          }}
          onClick={() => {
            if (onClear !== undefined) onClear();
            else state?.setValue(multiple ? [] : null);
          }}
        >
          <Cross />
        </button>
      ) : null}
      <Button className={OPENER}>
        <Chevron />
      </Button>
    </Group>
  );
}

// The chips of the picks of several, the items of React Aria's GridList,
// which lays them out as a grid, so that the arrow keys move between them
// along each line of chips and from line to line.
// Each chip shows the text of its pick, as missing where `marking` and the
// items lack it, and a button that removes it, after a grip where the chips
// drag, which React Aria's GridList needs so that a keyboard user drags a
// chip.
// A chip carries its key under a type of core's own rather than as text, so
// that a chip dropped on a text field, such as the select's own, types
// nothing into it.
// React Aria's GridList, unlike its TagGroup, removes no item on a key, so
// the chips remove the one that has the focus on Delete or Backspace, as a
// TagGroup does.
function Chips({
  keys,
  known,
  marking,
  reorderable,
  isDisabled,
  label,
  onChange,
}: {
  readonly keys: readonly Key[];
  // The text of each pick that the items hold, by its key.
  readonly known: ReadonlyMap<Key, string>;
  readonly marking: boolean;
  readonly reorderable: boolean;
  readonly isDisabled: boolean;
  readonly label: string | undefined;
  readonly onChange: (keys: Key[]) => void;
}): ReactElement {
  const { dragAndDropHooks } = useDragAndDrop({
    isDisabled,
    getItems: (dragged) => [...dragged].map((key) => ({ [PICK]: String(key) })),
    onReorder: (event) => {
      const next = moved(keys, event.keys, event.target);
      if (!sameKeys(next, keys)) onChange(next);
    },
    renderDropIndicator: (target) => <DropIndicator target={target} className={DROP} />,
  });
  const remove = (gone: Key): void => {
    onChange(keys.filter((key) => key !== gone));
  };
  return (
    <div
      className="bc:flex bc:min-w-0 bc:max-w-full"
      onKeyDown={(event) => {
        if (event.key !== "Delete" && event.key !== "Backspace") return;
        const chip = event.target instanceof HTMLElement && event.target.getAttribute("role") === "row" ? event.target.dataset.key : undefined;
        const gone = keys.find((key) => String(key) === chip);
        if (gone === undefined) return;
        event.preventDefault();
        remove(gone);
      }}
    >
      <GridList
        aria-label={label === undefined ? "Picks" : `Picks of ${label}`}
        layout="grid"
        dragAndDropHooks={reorderable ? dragAndDropHooks : undefined}
        className="bc:flex bc:min-w-0 bc:flex-wrap bc:gap-1"
      >
        {keys.map((key) => {
          const text = known.get(key) ?? String(key);
          const gone = marking && !known.has(key);
          return (
            <GridListItem key={key} id={key} textValue={text} data-missing={gone || undefined} className={CHIP}>
              {reorderable ? (
                <Button slot="drag" className={GRIP}>
                  <Grip />
                </Button>
              ) : null}
              <span className="bc:truncate">{gone ? missing(text) : text}</span>
              <Button
                aria-label={`Remove ${text}`}
                className={CHIP_REMOVE}
                onPress={() => {
                  remove(key);
                }}
              >
                <Cross />
              </Button>
            </GridListItem>
          );
        })}
      </GridList>
    </div>
  );
}

// The type under which a chip carries the key of its pick as it drags.
const PICK = "application/x-blockr-pick";

// The keys, with those dragged moved before or after the key of the target,
// where the target is not one of them.
function moved(keys: readonly Key[], dragged: ReadonlySet<Key>, target: { readonly key: Key; readonly dropPosition: string }): Key[] {
  const rest = keys.filter((key) => !dragged.has(key));
  const at = rest.indexOf(target.key);
  if (at < 0) return [...keys];
  const place = target.dropPosition === "after" ? at + 1 : at;
  return [...rest.slice(0, place), ...keys.filter((key) => dragged.has(key)), ...rest.slice(place)];
}

// A row of the list: the item, after a tick where the select picks several,
// in weight 600 where the select picks one and it is the pick.
// A description, which a child Text in the slot "description" holds, shows
// muted after it.
export function ComboBoxItem(props: ListBoxItemProps): ReactElement {
  const textValue = props.textValue ?? (typeof props.children === "string" ? props.children : undefined);
  return (
    <ListBoxItem {...props} textValue={textValue} className={ROW}>
      {composeRenderProps(props.children, (children, { isSelected, selectionMode }) => (
        <>
          {selectionMode === "multiple" ? (
            <span className="bc:flex bc:size-3.5 bc:shrink-0 bc:text-bg-accent">{isSelected ? <Tick /> : null}</span>
          ) : null}
          <span
            className={`bc:flex bc:min-w-0 bc:flex-1 bc:items-center bc:gap-2 bc:truncate ${
              selectionMode === "single" && isSelected ? "bc:font-semibold" : ""
            }`}
          >
            {children}
          </span>
        </>
      ))}
    </ListBoxItem>
  );
}

export interface ComboBoxSectionProps<T> extends ListBoxSectionProps<T> {
  readonly title?: string;
  readonly items?: Iterable<T>;
}

// A group of rows under a heading, which stays in view as the list scrolls.
export function ComboBoxSection<T extends object>({ title, items, children, ...props }: ComboBoxSectionProps<T>): ReactElement {
  return (
    <ListBoxSection {...props}>
      <Header className={HEADING}>{title}</Header>
      <Collection items={items}>{children}</Collection>
    </ListBoxSection>
  );
}

// The one chevron of the design system, which turns up while the list is
// open.
function Chevron(): ReactElement {
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
      className="bc:shrink-0 bc:text-text-muted bc:transition-transform bc:group-data-open:rotate-180"
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

// The cross of a chip's button.
function Cross(): ReactElement {
  return (
    <svg
      width="10"
      height="10"
      viewBox="0 0 10 10"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.4"
      strokeLinecap="round"
      aria-hidden="true"
    >
      <path d="M2.5 2.5l5 5M7.5 2.5l-5 5" />
    </svg>
  );
}

// The design system's grip of a chip that drags: six small dots.
function Grip(): ReactElement {
  return (
    <svg width="8" height="12" viewBox="0 0 8 12" fill="currentColor" aria-hidden="true">
      <circle cx="2" cy="2" r="1" />
      <circle cx="6" cy="2" r="1" />
      <circle cx="2" cy="6" r="1" />
      <circle cx="6" cy="6" r="1" />
      <circle cx="2" cy="10" r="1" />
      <circle cx="6" cy="10" r="1" />
    </svg>
  );
}

// The classes of the field and its parts, of the list and of its parts.
// The field is a box like core's other inputs, which holds the chips, the text
// field and the button, and draws the outline of the focus while the focus is
// in it.
const FIELD =
  "bc:flex bc:min-h-[30px] bc:min-w-0 bc:items-center bc:gap-1 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:py-0.5 bc:pr-0.5 bc:pl-2 bc:font-sans bc:text-sm bc:text-text-default bc:data-focus-within:outline-2 bc:data-focus-within:outline-offset-1 bc:data-focus-within:outline-border-accent bc:data-invalid:border-border-danger bc:data-disabled:opacity-50";
// The box of the chips and the text field, which wraps the chips onto more
// lines, while the buttons stay at the field's right.
const VALUES = "bc:flex bc:min-w-0 bc:flex-1 bc:flex-wrap bc:items-center bc:gap-1";
const INPUT =
  "bc:min-w-16 bc:flex-1 bc:border-0 bc:bg-transparent bc:p-0 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:placeholder:text-text-muted bc:disabled:cursor-default";
const CLEAR =
  "bc:flex bc:size-5 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:hover:bg-bg-subtle bc:hover:text-text-default";
const OPENER =
  "bc:ml-auto bc:flex bc:size-6 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-default bc:data-disabled:cursor-default";
// A chip of a pick that the items lack shows in the colour of danger, as the
// field then does, through the attributes that mark them, whose classes win
// over those of their default colours.
// A chip that drags shows the cursor of a grab, its grip at its start, and
// fades while it drags.
const CHIP =
  "bc:inline-flex bc:max-w-full bc:min-w-0 bc:items-center bc:gap-0.5 bc:rounded bc:border bc:border-border-default bc:bg-bg-subtle bc:py-0 bc:pr-0.5 bc:pl-1.5 bc:text-xs bc:leading-5 bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-allows-dragging:cursor-grab bc:data-allows-dragging:pl-0.5 bc:data-dragging:opacity-50 bc:data-missing:border-border-danger bc:data-missing:text-text-danger";
const CHIP_REMOVE =
  "bc:flex bc:size-4 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:data-hovered:bg-bg-subtle bc:data-hovered:text-text-default";
// React Aria lets a pointer press through the grip to the chip, which drags.
const GRIP =
  "bc:flex bc:h-4 bc:w-3 bc:shrink-0 bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";
// The point where a dragged chip would go: a bar in the gap between two
// chips, which moves no chip, since its margins take back the gap that it
// adds.
const DROP = "bc:-mx-[3px] bc:w-0.5 bc:shrink-0 bc:rounded-full bc:data-drop-target:bg-border-accent";
// The list takes the field's width, and the box of the rows scrolls within
// the height that React Aria leaves the list in the window, its border
// included.
// The box fits the list to the pixel: where it stood out of the list by its
// border, React Aria's scroll to the pick on opening moved the list by the
// border, took that for a list out of view and scrolled the page, which
// closes the list, or moves a row under a pointer still pressed, which picks
// it as the pointer goes up.
// The layer that holds the list in a dialog or a popover covers the window,
// above the dialog's content, and takes no clicks but on the list.
const LAYER = "bc:pointer-events-none bc:fixed bc:inset-0 bc:z-[100000]";
const LIST =
  "bc:pointer-events-auto bc:box-border bc:w-(--trigger-width) bc:overflow-hidden bc:rounded-lg bc:bg-bg-raised bc:font-sans bc:text-sm bc:text-gray-900 bc:shadow-md bc:outline-none bc:dark:text-gray-100";
const LISTBOX =
  "bc:box-border bc:max-h-[inherit] bc:overflow-y-auto bc:rounded-lg bc:border bc:border-border-default bc:p-1 bc:outline-none";
const HEADING =
  "bc:sticky bc:-top-1 bc:z-10 bc:bg-bg-raised bc:px-2.5 bc:pt-2 bc:pb-1 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wider bc:text-text-muted";
const ROW =
  "bc:flex bc:min-h-8 bc:cursor-pointer bc:items-center bc:gap-2 bc:rounded bc:px-2.5 bc:text-sm bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-focused:bg-bg-subtle bc:data-focus-visible:outline bc:data-focus-visible:outline-1 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50 bc:[&_[slot=description]]:min-w-0 bc:[&_[slot=description]]:flex-1 bc:[&_[slot=description]]:truncate bc:[&_[slot=description]]:text-[13px] bc:[&_[slot=description]]:text-text-muted";
const NOTE = "bc:m-0 bc:px-2.5 bc:py-2 bc:text-center bc:text-sm bc:italic bc:text-text-muted";
