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
// as Backspace in the empty field removes the last one.
// A FieldButton at the field's right, before the button that opens the list,
// clears the pick, or every pick, where there is one, but where the select
// `isRequired`.
// It is outside the order of Tab, and keeps the focus in the field.
// Bound, a select of one unsets its state arg so, as a block's new select of
// an arg without a default starts.
// A press in the field opens the list, as typing into it does.
// Where the select is disabled or read-only, the field changes no pick, as
// React Aria's ComboBox changes none then.
// Its chips show no button and remove nothing on a key, Backspace removes
// nothing, no button clears, and a press in the field opens no list.
// The chips of a disabled select take no focus either, as its text field
// takes none.
// The list hangs below the field, or above it where the window has no room
// below, as wide as the field.
// React Aria draws it in a portal at the end of the page's body, above a modal
// of the SDK that holds the select, as every overlay of the SDK sits in such a
// portal (../overlays/index.ts).
// The list draws its rows through React Aria's Virtualizer, which puts on the
// page only the rows in view, however many items the select holds.
// The list says "Loading…" while it has no items yet, or while its items are
// `outdated` and there are none, "No matches" where React Aria's filter keeps none of `defaultItems`,
// and otherwise what `renderEmptyState` says, "No options" by default, as
// wherever the `items` that its caller filters are none.
//
// Given `bound`, it shows and edits a value that useValue() binds, a block's
// state arg, a board option's value or a value that a function reads from a
// block's state, as the SDK's other inputs do (./inputs.tsx): one string, or
// an array of strings where it picks several, by the ids of its items.
// It shows a value that its items lack, such as a column that the data no
// longer has, by its id, as it shows every value, marks itself invalid, draws
// the value's chip in the colour of danger, and says below itself which values
// are not available, as "Not available: wt" (./inputs.tsx).
// While it has no items yet, as `items` and `defaultItems` are undefined while
// its children draw an item, it marks no value.
// While it is `outdated`, as while the request that gives its items asks
// again, the items may be for older data, so it marks none either.
// So its items must hold every value it takes, not a part of them.
// One pick it submits as the user picks it.
// Several it submits once, as the list closes, built from the value that the
// host holds then, so that it keeps a string that the assistant added in the
// meantime.
// It submits them in the order the user picked them, after those the value
// holds, or where `ordered`, in the order of its items, as for severities.
// Where the select goes while its list is open, as where its module
// unmounts, the picks go with it, as though the user never made them.
// A chip that the user removes while the list is closed it submits at once.
// It says below itself why the host refused what it submitted.

import { useContext, useRef, useState } from "react";
import type { ContextType, KeyboardEvent, ReactElement, ReactNode } from "react";
import { strings } from "../binding/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import {
  Button,
  Collection,
  ComboBox as AriaComboBox,
  ComboBoxStateContext,
  composeRenderProps,
  FieldError,
  Group,
  Header,
  Input,
  Label,
  ListBox,
  ListBoxItem,
  ListBoxSection,
  ListLayout,
  Popover,
  Tag,
  TagGroup,
  TagList,
  Text,
  Virtualizer,
} from "../aria/index.ts";
import type {
  ComboBoxProps as AriaComboBoxProps,
  Key,
  ListBoxItemProps,
  ListBoxSectionProps,
  ValidationResult,
} from "../aria/index.ts";
import { FieldButton } from "./button.tsx";
import { Chevron, Cross, Tick } from "./icons.tsx";
import { NotAvailable, Refusal } from "./inputs.tsx";
import { inOrder, merged } from "./items.ts";

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
}

export function ComboBox<T extends object, M extends Mode = "single">(props: ComboBoxProps<T, M>): ReactElement {
  const { bound, ordered = false, ...rest } = props;
  if (bound === undefined) return <Drawn {...(rest as DrawnProps)} />;
  return props.selectionMode === "multiple" ? (
    <BoundSeveral {...(rest as DrawnProps)} bound={bound} ordered={ordered} />
  ) : (
    <BoundOne {...(rest as DrawnProps)} bound={bound} />
  );
}

// What the select draws with, of either mode, whether it marks a value that
// its items lack, and whether its field may offer to clear the pick.
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
  const submit = (next: readonly string[], opened: readonly string[]): void => {
    const value = merged(strings(bound.get()), opened, next);
    if (value === null) return;
    void bound.commit(ordered && items !== undefined ? inOrder(value, ids(items)) : value);
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
  className,
  allowsEmptyCollection = true,
  ...props
}: DrawnProps): ReactElement {
  const multiple = props.selectionMode === "multiple";
  const isDisabled = props.isDisabled === true;
  const editable = !isDisabled && props.isReadOnly !== true;
  const given = props.items ?? props.defaultItems;
  // Whether it has no items yet, as its children draw an item with none given.
  const unknown = given === undefined && typeof children === "function";
  const none = given === undefined ? typeof children === "function" || !hasChildren(children) : [...given].length === 0;
  const empty = none && (outdated || unknown) ? "Loading…" : none ? (renderEmptyState?.() ?? "No options") : "No matches";
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
        placeholder={placeholder}
        multiple={multiple}
        marking={marking}
        isDisabled={isDisabled}
        editable={editable}
        clearable={props.isRequired !== true && editable}
        onClear={onClear}
        label={label}
      />
      {/* React Aria names the description of the field only where the
          element is there from the start, so it is there while empty too. */}
      <Text slot="description" className={`${styles.hint} bc:empty:hidden`}>
        {description}
      </Text>
      <FieldError className={styles.alert}>{errorMessage}</FieldError>
      <Lacking multiple={multiple} marking={marking} />
      <Popover offset={4} className={LIST}>
        <Virtualizer layout={ListLayout} layoutOptions={LAYOUT}>
          <ListBox renderEmptyState={() => <p className={NOTE}>{empty}</p>} className={LISTBOX}>
            {children}
          </ListBox>
        </Virtualizer>
      </Popover>
    </AriaComboBox>
  );
}

// The value given, or the array of the keys that the select drew before where
// it holds the same keys.
// React Aria takes a new array of picks for a change of the value, and then
// empties the field, so that a caller that builds the picks afresh as it draws
// would empty the field as the user types into it.
// The Select takes its picks so too (./select.tsx).
export function useSameValue<V>(value: V): V {
  const [drawn, setDrawn] = useState(value);
  const same = drawn === value || (Array.isArray(drawn) && Array.isArray(value) && sameKeys(drawn, value));
  if (!same) setDrawn(value);
  return same ? drawn : value;
}

function sameKeys(a: readonly unknown[], b: readonly unknown[]): boolean {
  return a.length === b.length && a.every((key, i) => key === b[i]);
}

function hasChildren(children: ReactNode): boolean {
  return Array.isArray(children) ? children.some((child) => hasChildren(child as ReactNode)) : children !== null && children !== undefined && children !== false;
}

// The field: the chips of the picks of several, the text field, which takes
// the filter, and the button that opens and closes the list.
// It shows a pick that the items lack by its id, as the text of its chip, or
// as the text of the field of one.
// Where `marking`, it marks itself invalid while it holds such a pick, and
// draws the pick's chip in the colour of danger.
// The text field of one shows the pick's text while the user is not in it,
// as React draws the pick, where React Aria would set it a step later, from
// an effect, so that the controls show a state as the host announces it, with
// no wait for React (../controls/controls.tsx).
function Field({
  placeholder,
  multiple,
  marking,
  isDisabled,
  editable,
  clearable,
  onClear,
  label,
}: {
  readonly placeholder: string | undefined;
  readonly multiple: boolean;
  readonly marking: boolean;
  readonly isDisabled: boolean;
  // Whether the field changes the pick, as not where the select is disabled
  // or read-only.
  readonly editable: boolean;
  readonly clearable: boolean;
  readonly onClear: (() => void) | undefined;
  readonly label: string | undefined;
}): ReactElement {
  const { state, keys, one, known, lacked } = usePicks(multiple);
  // The text of the pick of one, where the items hold it.
  const item = one === null ? undefined : known.get(one);
  const invalid = marking && lacked.length > 0;
  const remove = (gone: ReadonlySet<Key>): void => {
    state?.setValue(keys.filter((key) => !gone.has(key)));
  };
  // The chip of each pick: its key, its text, and whether the items lack it.
  const chips = keys.map((key) => ({ key, text: known.get(key) ?? String(key), gone: marking && !known.has(key) }));
  const picks = label === undefined ? "Picks" : `Picks of ${label}`;
  return (
    <Group isInvalid={invalid} className={FIELD}>
      <div className={VALUES}>
        {keys.length === 0 ? null : isDisabled ? (
          // The chips of a disabled select take no focus, as its text field
          // takes none.
          // React Aria's TagGroup keeps a tab stop on its list even where
          // every tag is disabled, so a disabled select draws its chips as
          // a plain list.
          <div role="list" aria-label={picks} className="bc:flex bc:min-w-0 bc:max-w-full bc:flex-wrap bc:gap-1">
            {chips.map(({ key, text, gone }) => (
              <div key={key} role="listitem" data-missing={gone || undefined} className={CHIP}>
                <span className="bc:truncate">{text}</span>
              </div>
            ))}
          </div>
        ) : (
          <TagGroup aria-label={picks} onRemove={editable ? remove : undefined} className="bc:flex bc:min-w-0 bc:max-w-full">
            <TagList className="bc:flex bc:min-w-0 bc:flex-wrap bc:gap-1">
              {chips.map(({ key, text, gone }) => (
                <Tag key={key} id={key} textValue={text} data-missing={gone || undefined} className={CHIP}>
                  <span className="bc:truncate">{text}</span>
                  {editable ? (
                    <Button slot="remove" aria-label={`Remove ${text}`} className={CHIP_REMOVE}>
                      <Cross />
                    </Button>
                  ) : null}
                </Tag>
              ))}
            </TagList>
          </TagGroup>
        )}
        <Input
          value={multiple || state === null || state.isFocused || state.isOpen ? undefined : (item ?? (one === null ? "" : String(one)))}
          placeholder={one !== null && item === undefined ? String(one) : keys.length > 0 ? undefined : placeholder}
          aria-invalid={invalid || undefined}
          className={INPUT}
          onClick={() => {
            if (editable && state !== null && !state.isOpen) state.open(null, "manual");
          }}
          onKeyDown={(event: KeyboardEvent<HTMLInputElement>) => {
            // An Escape that closes the list closes nothing else, such as the
            // dialog or the form that holds the select, which React Aria lets
            // it reach.
            if (event.key === "Escape" && state?.isOpen === true) event.preventDefault();
            if (!editable || event.key !== "Backspace" || event.currentTarget.value !== "" || keys.length === 0) return;
            state?.setValue(keys.slice(0, -1));
          }}
        />
      </div>
      {clearable && (keys.length > 0 || one !== null) ? (
        // A FieldButton whose slot is null, which React Aria's ComboBox then
        // leaves alone, rather than take it for the button that opens the
        // list. It is outside the order of Tab, and keeps the focus in the
        // text field, as a press in the list does.
        // React Aria's Button takes no title, so the cross carries it.
        <FieldButton
          slot={null}
          excludeFromTabOrder
          preventFocusOnPress
          aria-label={label === undefined ? "Clear" : `Clear ${label}`}
          onPress={() => {
            if (onClear !== undefined) onClear();
            else state?.setValue(multiple ? [] : null);
          }}
        >
          <span title={multiple ? "Clear every pick" : "Clear the pick"} className="bc:flex bc:size-full bc:items-center bc:justify-center">
            <Cross />
          </span>
        </FieldButton>
      ) : null}
      <Button className={OPENER}>
        <Chevron />
      </Button>
    </Group>
  );
}

// The picks that the select holds, as React Aria's state of the ComboBox has
// them: the keys of several, or the key of one, the text of each pick that
// the items hold, and the picks that they lack.
function usePicks(multiple: boolean): {
  readonly state: ContextType<typeof ComboBoxStateContext>;
  readonly keys: readonly Key[];
  readonly one: Key | null;
  readonly known: ReadonlyMap<Key, string>;
  readonly lacked: readonly Key[];
} {
  const state = useContext(ComboBoxStateContext);
  const value = state?.value ?? null;
  const keys = multiple && Array.isArray(value) ? (value as readonly Key[]) : [];
  const one = !multiple && value !== null && !Array.isArray(value) ? (value as Key) : null;
  const known = new Map((state?.selectedItems ?? []).map((item) => [item.key, item.textValue]));
  const lacked = (one === null ? keys : [one]).filter((key) => !known.has(key));
  return { state, keys, one, known, lacked };
}

// What the select says below itself, where `marking`, of the picks that its
// items lack. The user removes such a pick by its chip, or picks another in
// place of a pick of one.
function Lacking({ multiple, marking }: { readonly multiple: boolean; readonly marking: boolean }): ReactElement | null {
  const { lacked } = usePicks(multiple);
  return marking ? <NotAvailable values={lacked.map(String)} /> : null;
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

// A group of rows under a heading.
// The Virtualizer lays the heading out as it lays out a row, so the heading
// scrolls away with the rows of its group.
export function ComboBoxSection<T extends object>({ title, items, children, ...props }: ComboBoxSectionProps<T>): ReactElement {
  return (
    <ListBoxSection {...props}>
      <Header className={HEADING}>{title}</Header>
      <Collection items={items}>{children}</Collection>
    </ListBoxSection>
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
const OPENER =
  "bc:ml-auto bc:flex bc:size-6 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-default bc:data-disabled:cursor-default";
// A chip of a pick that the items lack shows in the colour of danger, as the
// field then does, through the attributes that mark them, whose classes win
// over those of their default colours.
// A chip with no button, as where the select is disabled or read-only, pads
// its right as it pads its left.
const CHIP =
  "bc:inline-flex bc:max-w-full bc:min-w-0 bc:items-center bc:gap-0.5 bc:rounded bc:border bc:border-border-default bc:bg-bg-subtle bc:py-0 bc:pr-1.5 bc:pl-1.5 bc:data-allows-removing:pr-0.5 bc:text-xs bc:leading-5 bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-missing:border-border-danger bc:data-missing:text-text-danger";
const CHIP_REMOVE =
  "bc:flex bc:size-4 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:data-hovered:bg-bg-subtle bc:data-hovered:text-text-default";
// The list takes the field's width, and the box of the rows scrolls within
// the height that React Aria leaves the list in the window, its border
// included.
// The box fits the list to the pixel: where it stood out of the list by its
// border, React Aria's scroll to the pick on opening moved the list by the
// border, took that for a list out of view and scrolled the page, which
// closes the list, or moves a row under a pointer still pressed, which picks
// it as the pointer goes up.
// The Virtualizer lays the rows out one below the other, inside a padding of
// 4 pixels, which the box of the rows thus holds none of in its CSS.
// A row is one line, as ROW truncates what it holds, so the Virtualizer takes
// each row to be 32 pixels high and each heading 28, the heights that ROW and
// HEADING give them, rather than measure each as it draws it.
const LAYOUT = { rowHeight: 32, headingHeight: 28, padding: 4 } as const;
const LIST =
  "bc:box-border bc:w-(--trigger-width) bc:overflow-hidden bc:rounded-lg bc:bg-bg-raised bc:font-sans bc:text-sm bc:text-gray-900 bc:shadow-md bc:outline-none bc:dark:text-gray-100";
const LISTBOX =
  "bc:box-border bc:max-h-[inherit] bc:overflow-y-auto bc:rounded-lg bc:border bc:border-border-default bc:outline-none";
const HEADING =
  "bc:box-border bc:h-[28px] bc:bg-bg-raised bc:px-2.5 bc:pt-2 bc:pb-1 bc:text-xs bc:font-semibold bc:uppercase bc:tracking-wider bc:text-text-muted";
const ROW =
  "bc:box-border bc:flex bc:h-[32px] bc:cursor-pointer bc:items-center bc:gap-2 bc:rounded bc:px-2.5 bc:text-sm bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-focused:bg-bg-subtle bc:data-focus-visible:outline bc:data-focus-visible:outline-1 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50 bc:[&_[slot=description]]:min-w-0 bc:[&_[slot=description]]:flex-1 bc:[&_[slot=description]]:truncate bc:[&_[slot=description]]:text-[13px] bc:[&_[slot=description]]:text-text-muted";
const NOTE = "bc:m-0 bc:px-2.5 bc:py-2 bc:text-center bc:text-sm bc:italic bc:text-text-muted";
