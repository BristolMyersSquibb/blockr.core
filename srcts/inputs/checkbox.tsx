// The checkbox and the checkbox group of core's React SDK: React Aria's
// CheckboxField and CheckboxButton, drawn as one, and its CheckboxGroup
// (https://react-aria.adobe.com/Checkbox, https://react-aria.adobe.com/CheckboxGroup),
// in core's theme.
// They are drawn after the Checkbox and the CheckboxGroup of React Aria's
// Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Checkbox.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/CheckboxGroup.tsx),
// and take what those take: the props of React Aria's parts, and the
// description and the error message that the starter kit adds, and the label
// of the group.
// A checkbox holds its label as its children, and a group its checkboxes,
// each with the value that it picks.
//
// Given `bound`, a checkbox shows and edits a boolean, a block's state arg or
// a board option's value, which useValue() binds, as the SDK's other inputs do
// (./inputs.tsx), and a group an array of strings, by the values of its
// checkboxes.
// Each submits the user's pick at once, a group built from the value that the
// host holds then, so that it keeps a string that the assistant added in the
// meantime.
// A group submits its picks in the order the user picked them, after those
// the value holds, or where `ordered`, in the order of its checkboxes, as for
// severities.
// A group shows a value that none of its checkboxes holds as a checkbox of its
// own, before them, checked and marked as missing, which the user clears to
// drop the value, and marks itself invalid, but not while it is `outdated`,
// as the select does (./combobox.tsx).
// Each says below itself why the host refused what it submitted.

import { useRef } from "react";
import type { ReactElement, ReactNode } from "react";
import { strings } from "../binding/index.ts";
import {
  CheckboxButton,
  CheckboxField,
  CheckboxGroup as AriaCheckboxGroup,
  composeRenderProps,
  FieldError,
  Label,
  Text,
} from "../aria/index.ts";
import type { CheckboxFieldProps, CheckboxGroupProps as AriaCheckboxGroupProps, ValidationResult } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import { Dash, Tick } from "./icons.tsx";
import { missing, Refusal } from "./inputs.tsx";
import { inOrder, ItemsContext, lacking, merged, useItem, useItems } from "./items.ts";

export interface CheckboxProps extends Omit<CheckboxFieldProps, "children"> {
  // Its label.
  readonly children?: ReactNode;
  // What the starter kit adds.
  readonly description?: string;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
}

export function Checkbox({ bound, ...props }: CheckboxProps): ReactElement {
  return bound === undefined ? <DrawnCheckbox {...props} /> : <BoundCheckbox {...props} bound={bound} />;
}

type DrawnCheckboxProps = Omit<CheckboxProps, "bound"> & {
  // Whether it shows a value that the items of its group lack.
  readonly gone?: boolean;
};

function BoundCheckbox({ bound, onChange, ...props }: DrawnCheckboxProps & { readonly bound: Bound }): ReactElement {
  return (
    <>
      <DrawnCheckbox
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

// The box and the label, the description and the error message.
// Inside a group, the checkbox tells the group its value.
function DrawnCheckbox({ children, description, errorMessage, gone = false, className, ...props }: DrawnCheckboxProps): ReactElement {
  const ref = useRef<HTMLDivElement>(null);
  useItem(props.value, ref);
  return (
    <CheckboxField {...props} ref={ref} className="bc:grid bc:gap-1 bc:font-sans bc:text-text-default">
      <CheckboxButton
        data-missing={gone || undefined}
        className={composeRenderProps(className, (given) => `bc:group ${CHECK} ${given ?? ""}`)}
      >
        {composeRenderProps(children, (children, { isSelected, isIndeterminate }) => (
          <>
            <span className={BOX}>{isIndeterminate ? <Dash /> : isSelected ? <Tick size={12} /> : null}</span>
            {children}
          </>
        ))}
      </CheckboxButton>
      {/* React Aria names the description of the checkbox only where the
          element is there from the start, so it is there while empty too. */}
      <Text slot="description" className={`${styles.hint} bc:ml-6 bc:empty:hidden`}>
        {description}
      </Text>
      <FieldError className={`${styles.alert} bc:ml-6`}>{errorMessage}</FieldError>
    </CheckboxField>
  );
}

export interface CheckboxGroupProps extends Omit<AriaCheckboxGroupProps, "children"> {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  // Its checkboxes.
  readonly children?: ReactNode;
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
  readonly outdated?: boolean;
  readonly ordered?: boolean;
}

export function CheckboxGroup({ bound, outdated = false, ordered = false, ...props }: CheckboxGroupProps): ReactElement {
  return bound === undefined ? (
    <DrawnGroup {...props} />
  ) : (
    <BoundGroup {...props} bound={bound} outdated={outdated} ordered={ordered} />
  );
}

type DrawnGroupProps = Omit<CheckboxGroupProps, "bound" | "outdated" | "ordered">;

function BoundGroup({
  bound,
  outdated,
  ordered,
  onChange,
  children,
  ...props
}: DrawnGroupProps & { readonly bound: Bound; readonly outdated: boolean; readonly ordered: boolean }): ReactElement {
  const { known, items } = useItems();
  const held = strings(bound.value);
  const gone = lacking(held, known);
  const marking = !outdated && gone.length > 0;
  return (
    <>
      <DrawnGroup
        {...props}
        value={[...held]}
        isInvalid={props.isInvalid === true || marking}
        onChange={(picks) => {
          const value = merged(strings(bound.get()), held, picks);
          if (value !== null) void bound.commit(ordered && known !== null ? inOrder(value, known) : value);
          onChange?.(picks);
        }}
      >
        {/* A value that the checkboxes lack shows as a checkbox of its own,
            which tells the group no value. */}
        <ItemsContext.Provider value={null}>
          {gone.map((value) => (
            <DrawnCheckbox key={value} value={value} gone={!outdated}>
              {outdated ? value : missing(value)}
            </DrawnCheckbox>
          ))}
        </ItemsContext.Provider>
        <ItemsContext.Provider value={items}>{children}</ItemsContext.Provider>
      </DrawnGroup>
      <Refusal error={bound.error} />
    </>
  );
}

// The label, the checkboxes, the description and the error message.
function DrawnGroup({ label, description, errorMessage, children, className, ...props }: DrawnGroupProps): ReactElement {
  return (
    <AriaCheckboxGroup {...props} className={composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`)}>
      {label === undefined ? null : <Label className={styles.label}>{label}</Label>}
      <div className="bc:grid bc:gap-1.5">{children}</div>
      <Text slot="description" className={`${styles.hint} bc:empty:hidden`}>
        {description}
      </Text>
      <FieldError className={styles.alert}>{errorMessage}</FieldError>
    </AriaCheckboxGroup>
  );
}

// The classes of the checkbox and of its box.
// The box takes the accent colour where the checkbox is checked, and the
// colour of danger where it is invalid, as where it shows a value that the
// items of its group lack, whose label takes that colour too.
// The box takes the colour on the accent, for its tick or its dash, only
// where it is checked or neither, since its border is the text's colour
// mixed with transparency otherwise, which the colour on the accent, white,
// would make invisible on a light page.
const CHECK =
  "bc:flex bc:w-fit bc:cursor-pointer bc:items-center bc:gap-2 bc:text-sm bc:text-text-default bc:data-disabled:cursor-default bc:data-disabled:opacity-50 bc:data-missing:text-text-danger";
const BOX =
  "bc:box-border bc:flex bc:size-4 bc:shrink-0 bc:items-center bc:justify-center bc:rounded-sm bc:border bc:border-border-default bc:bg-transparent bc:outline-offset-2 bc:outline-border-accent bc:group-data-focus-visible:outline-2 bc:group-data-selected:border-border-accent bc:group-data-selected:bg-bg-accent bc:group-data-selected:text-text-on-accent bc:group-data-indeterminate:border-border-accent bc:group-data-indeterminate:bg-bg-accent bc:group-data-indeterminate:text-text-on-accent bc:group-data-invalid:border-border-danger bc:group-data-selected:group-data-invalid:bg-border-danger";
