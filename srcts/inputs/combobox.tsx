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
// The chips wrap onto more lines, but each cuts its own text off where the
// field is too narrow for it. A chip whose text is cut off shows it whole in
// a tooltip of the SDK, as the pointer rests on the chip, and at once as the
// chip takes the focus from the keyboard.
// A button at the field's right, before the one that opens the list, clears
// the pick, or every pick, where there is one, but where the select
// `isRequired`. It says so in a tooltip as the pointer rests on it, and
// takes no place in the order of the keyboard's focus, as the button that
// opens the list takes none, since the keyboard clears the field itself.
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
// portal, at the z-index of the overlays of core's theme
// (../overlays/index.ts).
// The list draws its rows through React Aria's Virtualizer, which puts on the
// page only the rows in view, however many items the select holds.
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
// Where the select goes while its list is open, as where its module
// unmounts, the picks go with it, as though the user never made them.
// A chip that the user removes while the list is closed it submits at once.
// It says below itself why the host refused what it submitted.

import { useContext, useEffect, useEffectEvent, useRef, useState } from "react";
import type { KeyboardEvent, ReactElement, ReactNode } from "react";
import { strings } from "../binding/index.ts";
import { Focusable, OVERLAY_Z_INDEX, Tooltip, TooltipTrigger } from "../overlays/index.ts";
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
  TooltipTriggerStateContext,
  Virtualizer,
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
  const submit = (next: readonly string[], opened: readonly string[]): void => {
    const added = next.filter((value) => !opened.includes(value));
    const removed = opened.filter((value) => !next.includes(value));
    if (added.length === 0 && removed.length === 0) return;
    const now = strings(bound.get());
    const value = [...now.filter((x) => !removed.includes(x)), ...added.filter((x) => !now.includes(x))];
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
      <Popover offset={4} className={LIST} style={{ zIndex: OVERLAY_Z_INDEX }}>
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
function useSameValue<V>(value: V): V {
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
// It shows a pick of one that the items lack in place of the placeholder, as
// missing where `marking`, and marks itself invalid.
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
  const remove = (gone: ReadonlySet<Key>): void => {
    state?.setValue(keys.filter((key) => !gone.has(key)));
  };
  // The chip of each pick: its key, its text, and whether it shows the pick
  // as missing.
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
              <PlainChip key={key} text={gone ? missing(text) : text} gone={gone} />
            ))}
          </div>
        ) : (
          <TagGroup aria-label={picks} onRemove={editable ? remove : undefined} className="bc:flex bc:min-w-0 bc:max-w-full">
            <TagList className="bc:flex bc:min-w-0 bc:flex-wrap bc:gap-1">
              {chips.map(({ key, text, gone }) => (
                <Tag key={key} id={key} textValue={text} data-missing={gone || undefined} className={CHIP}>
                  {({ isFocusVisible }) => (
                    <>
                      <ChipText text={gone ? missing(text) : text} focused={isFocusVisible} />
                      {editable ? (
                        <Button slot="remove" aria-label={`Remove ${text}`} className={CHIP_REMOVE}>
                          <Cross />
                        </Button>
                      ) : null}
                    </>
                  )}
                </Tag>
              ))}
            </TagList>
          </TagGroup>
        )}
        <Input
          value={multiple || state === null || state.isFocused || state.isOpen ? undefined : (item?.textValue ?? "")}
          placeholder={lackedOne ? (marking ? missing(String(one)) : String(one)) : keys.length > 0 ? undefined : placeholder}
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
        // A button of the field's own, not React Aria's, which the select
        // would take for its own and open the list with. It keeps the focus
        // in the text field, as a press in the list does. React Aria's
        // Focusable makes it the trigger of its tooltip.
        <TooltipTrigger>
          <Focusable>
            <button
              type="button"
              tabIndex={-1}
              aria-label={label === undefined ? "Clear" : `Clear ${label}`}
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
          </Focusable>
          <Tooltip>{multiple ? "Clear every pick" : "Clear the pick"}</Tooltip>
        </TooltipTrigger>
      ) : null}
      <Button className={OPENER}>
        <Chevron />
      </Button>
    </Group>
  );
}

// A chip of a disabled select, which takes no focus.
function PlainChip({ text, gone }: { text: string; gone: boolean }): ReactElement {
  return (
    <div role="listitem" data-missing={gone || undefined} className={CHIP}>
      <ChipText text={text} focused={false} />
    </div>
  );
}

// The text of a chip, which the chip cuts off where it is too narrow for it.
// It then shows the text whole in a tooltip while the pointer rests on the
// text, and while the chip has the focus from the keyboard, as `focused`
// says.
// The text follows the pointer itself, since React Aria counts a chip as
// hovered only where the select's chips can be picked or act.
// The tooltip opens and closes as the state of a TooltipTrigger has it, with
// React Aria's delays: after the pointer rests a while, at once while another
// tooltip shows, and at once on the focus.
// The chip's own name holds the text whole for assistive technology, so the
// tooltip names nothing to it.
function ChipText({ text, focused }: { text: string; focused: boolean }): ReactElement {
  return (
    <TooltipTrigger>
      <CutText text={text} focused={focused} />
    </TooltipTrigger>
  );
}

// The tooltip closes at once as the chip loses the focus, and after React
// Aria's delay as the pointer leaves it.
function CutText({ text, focused }: { text: string; focused: boolean }): ReactElement {
  const tip = useContext(TooltipTriggerStateContext);
  const ref = useRef<HTMLSpanElement>(null);
  const [pointed, setPointed] = useState(false);
  const shown = useRef({ pointed: false, focused: false });
  const follow = useEffectEvent((now: { pointed: boolean; focused: boolean }) => {
    const before = shown.current;
    shown.current = now;
    const el = ref.current;
    const cut = el !== null && el.scrollWidth > el.clientWidth;
    if (tip === null) return;
    if (cut && now.focused) tip.open(true);
    else if (cut && now.pointed) tip.open();
    else if (before.pointed || before.focused) tip.close(before.focused);
  });
  useEffect(() => {
    follow({ pointed, focused });
  }, [pointed, focused]);
  return (
    <>
      <span
        ref={ref}
        className="bc:truncate"
        onPointerEnter={(event) => {
          if (event.pointerType !== "touch") setPointed(true);
        }}
        onPointerLeave={() => {
          setPointed(false);
        }}
      >
        {text}
      </span>
      <Tooltip triggerRef={ref}>{text}</Tooltip>
    </>
  );
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
