// The toggle button and the toggle button group of core's React SDK: React
// Aria's ToggleButton and ToggleButtonGroup
// (https://react-aria.adobe.com/ToggleButton, https://react-aria.adobe.com/ToggleButtonGroup),
// in core's theme, after those of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/ToggleButton.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/ToggleButtonGroup.tsx).
// Each takes the props of its part of React Aria.
// A group draws its buttons side by side in one box, as a field's box, and
// takes the label and the description of the SDK's fields, which a group
// needs to name what it picks, as a select names it.
// A group holds its buttons, each with the id that it picks.
//
// Given `bound`, a toggle button shows and edits a boolean that useValue()
// binds, a block's state arg, a board option's value or a value that a
// function reads from a block's state, as the SDK's other inputs do
// (./inputs.tsx), and a group a string, or an array of strings where its
// selectionMode is "multiple", by the ids of its buttons.
// Each submits the user's pick at once, a group of several built from the
// value that the host holds then, so that it keeps a string that the
// assistant added in the meantime.
// A group of one unsets its state arg as the user unpicks its pick, unless it
// sets disallowEmptySelection.
// A group of several submits its picks in the order the user picked them,
// after those the value holds, or where `ordered`, in the order of its
// buttons.
// A group shows a value that none of its buttons holds as a button of its own,
// before them, picked and marked as missing, and marks itself invalid, but not
// while it is `outdated`, as the select does (./combobox.tsx).
// Each says below itself why the host refused what it submitted.

import { createContext, useContext, useId, useRef } from "react";
import type { ReactElement, ReactNode } from "react";
import { strings } from "../binding/index.ts";
import { composeRenderProps, ToggleButton as AriaToggleButton, ToggleButtonGroup as AriaToggleButtonGroup } from "../aria/index.ts";
import type { ToggleButtonGroupProps as AriaToggleButtonGroupProps, ToggleButtonProps as AriaToggleButtonProps } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import { missing, Refusal } from "./inputs.tsx";
import { inOrder, ItemsContext, lacking, merged, useItem, useItems } from "./items.ts";

export interface ToggleButtonProps extends AriaToggleButtonProps {
  // The binding of a block's state arg or of a board option's value, for a
  // button outside a group.
  readonly bound?: Bound;
}

export function ToggleButton({ bound, onChange, ...props }: ToggleButtonProps): ReactElement {
  if (bound === undefined) return <DrawnButton {...props} onChange={onChange} />;
  return (
    <>
      <DrawnButton
        {...props}
        isSelected={bound.value === true}
        onChange={(selected) => {
          void bound.commit(selected);
          onChange?.(selected);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// Whether a button sits in a group, whose box holds it.
const InGroup = createContext(false);

// A button that tells its group its id, or that shows a value the buttons
// lack.
// Outside a group, it has a border of its own.
function DrawnButton({ gone = false, className, ...props }: AriaToggleButtonProps & { readonly gone?: boolean }): ReactElement {
  const ref = useRef<HTMLButtonElement>(null);
  const inGroup = useContext(InGroup);
  useItem(props.id === undefined ? undefined : String(props.id), ref);
  return (
    <AriaToggleButton
      {...props}
      ref={ref}
      data-missing={gone || undefined}
      className={composeRenderProps(className, (given) => `${TOGGLE} ${inGroup ? "" : ALONE} ${given ?? ""}`)}
    />
  );
}

export interface ToggleButtonGroupProps extends Omit<AriaToggleButtonGroupProps, "children"> {
  // What the SDK's fields take.
  readonly label?: string;
  readonly description?: string;
  // Its buttons.
  readonly children?: ReactNode;
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
  readonly outdated?: boolean;
  readonly ordered?: boolean;
}

export function ToggleButtonGroup({ bound, outdated = false, ordered = false, ...props }: ToggleButtonGroupProps): ReactElement {
  return bound === undefined ? (
    <DrawnGroup {...props} />
  ) : (
    <BoundGroup {...props} bound={bound} outdated={outdated} ordered={ordered} />
  );
}

type DrawnGroupProps = Omit<ToggleButtonGroupProps, "bound" | "outdated" | "ordered"> & {
  // Whether it shows a value that its buttons lack.
  readonly invalid?: boolean;
};

function BoundGroup({
  bound,
  outdated,
  ordered,
  onSelectionChange,
  children,
  ...props
}: DrawnGroupProps & { readonly bound: Bound; readonly outdated: boolean; readonly ordered: boolean }): ReactElement {
  const { known, items } = useItems();
  const several = props.selectionMode === "multiple";
  const held = several ? strings(bound.value) : typeof bound.value === "string" ? [bound.value] : [];
  const gone = lacking(held, known);
  return (
    <>
      <DrawnGroup
        {...props}
        selectedKeys={held}
        invalid={!outdated && gone.length > 0}
        onSelectionChange={(keys) => {
          const picks = [...keys].map(String);
          if (several) {
            const value = merged(strings(bound.get()), held, picks);
            if (value !== null) void bound.commit(ordered && known !== null ? inOrder(value, known) : value);
          } else {
            const [pick] = picks;
            void (pick === undefined ? bound.unset() : bound.commit(pick));
          }
          onSelectionChange?.(keys);
        }}
      >
        {/* A value that the buttons lack shows as a button of its own, which
            tells the group no id. */}
        <ItemsContext.Provider value={null}>
          {gone.map((value) => (
            <DrawnButton key={value} id={value} gone={!outdated}>
              {outdated ? value : missing(value)}
            </DrawnButton>
          ))}
        </ItemsContext.Provider>
        <ItemsContext.Provider value={items}>{children}</ItemsContext.Provider>
      </DrawnGroup>
      <Refusal error={bound.error} />
    </>
  );
}

// The label, the box of the buttons and the description.
// React Aria's group has no label of its own, so the label names the group
// through aria-labelledby, and the description through aria-describedby.
function DrawnGroup({ label, description, invalid = false, children, className, ...props }: DrawnGroupProps): ReactElement {
  const labelId = useId();
  const descriptionId = useId();
  const described = description !== undefined && description !== "";
  const group = (
    <AriaToggleButtonGroup
      {...props}
      aria-labelledby={props["aria-labelledby"] ?? (label === undefined ? undefined : labelId)}
      aria-describedby={described ? descriptionId : undefined}
      data-invalid={invalid || undefined}
      className={composeRenderProps(className, (given) => `${GROUP} ${given ?? ""}`)}
    >
      <InGroup.Provider value={true}>{children}</InGroup.Provider>
    </AriaToggleButtonGroup>
  );
  if (label === undefined && !described) return group;
  return (
    <div className={styles.field}>
      {label === undefined ? null : (
        <span id={labelId} className={styles.label}>
          {label}
        </span>
      )}
      {group}
      {described ? (
        <p id={descriptionId} className={styles.hint}>
          {description}
        </p>
      ) : null}
    </div>
  );
}

// The classes of the box of a group's buttons, and of a button, which takes
// the accent colour where it is picked.
// Outside a group, a button has the border of the box.
// A button that shows a value the buttons lack takes the colour of danger,
// and so does the border of its group.
const GROUP =
  "bc:flex bc:w-fit bc:max-w-full bc:flex-wrap bc:gap-0.5 bc:rounded-md bc:border bc:border-border-default bc:p-0.5 bc:font-sans bc:data-[orientation=vertical]:flex-col bc:data-invalid:border-border-danger";
const TOGGLE =
  "bc:inline-flex bc:min-h-6 bc:cursor-pointer bc:items-center bc:justify-center bc:gap-1 bc:rounded bc:border-0 bc:bg-transparent bc:px-2 bc:py-0.5 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-1 bc:data-focus-visible:outline-border-accent bc:data-selected:bg-bg-accent bc:data-selected:text-text-on-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50 bc:data-missing:text-text-danger bc:data-selected:data-missing:bg-border-danger bc:data-selected:data-missing:text-text-on-accent";
const ALONE = "bc:rounded-md bc:border bc:border-solid bc:border-border-default bc:data-selected:border-border-accent";
