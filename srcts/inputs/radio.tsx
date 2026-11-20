// The radio group of core's React SDK: React Aria's RadioGroup, and its
// RadioField and RadioButton drawn as one radio
// (https://react-aria.adobe.com/RadioGroup), in core's theme, after the
// RadioGroup and the Radio of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/RadioGroup.tsx).
// They take what those take: the props of React Aria's parts, and the label,
// the description and the error message that the starter kit adds.
// A group holds its radios, each with the value that it picks and its label
// as its children, one below the other, or side by side where its
// orientation is "horizontal".
//
// Given `bound`, a group shows and edits a string that useValue() binds, a
// block's state arg, a board option's value or a value that a function reads
// from a block's state, as the SDK's other inputs do (./inputs.tsx), by the
// values of its radios.
// It submits the user's pick at once.
// It shows a value that none of its radios holds as a radio of its own,
// before them, picked and marked as missing, and marks itself invalid, but not
// while it is `outdated`, as the select does (./combobox.tsx).
// It says below itself why the host refused what it submitted.

import { useRef } from "react";
import type { ReactElement, ReactNode } from "react";
import { composeRenderProps, FieldError, Label, RadioButton, RadioField, RadioGroup as AriaRadioGroup, Text } from "../aria/index.ts";
import type { RadioFieldProps, RadioGroupProps as AriaRadioGroupProps, ValidationResult } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import { missing, Refusal } from "./inputs.tsx";
import { ItemsContext, lacking, useItem, useItems } from "./items.ts";

export interface RadioGroupProps extends Omit<AriaRadioGroupProps, "children"> {
  // What the starter kit adds.
  readonly label?: string;
  readonly description?: string;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  // Its radios.
  readonly children?: ReactNode;
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
  readonly outdated?: boolean;
}

export function RadioGroup({ bound, outdated = false, ...props }: RadioGroupProps): ReactElement {
  return bound === undefined ? <DrawnGroup {...props} /> : <BoundGroup {...props} bound={bound} outdated={outdated} />;
}

type DrawnGroupProps = Omit<RadioGroupProps, "bound" | "outdated">;

function BoundGroup({
  bound,
  outdated,
  onChange,
  children,
  ...props
}: DrawnGroupProps & { readonly bound: Bound; readonly outdated: boolean }): ReactElement {
  const { known, items } = useItems();
  const held = typeof bound.value === "string" ? bound.value : null;
  const [gone] = lacking(held === null ? [] : [held], known);
  const marking = !outdated && gone !== undefined;
  return (
    <>
      <DrawnGroup
        {...props}
        value={held}
        isInvalid={props.isInvalid === true || marking}
        onChange={(value) => {
          void bound.commit(value);
          onChange?.(value);
        }}
      >
        {/* A value that the radios lack shows as a radio of its own, which
            tells the group no value. */}
        <ItemsContext.Provider value={null}>
          {gone === undefined ? null : (
            <DrawnRadio value={gone} gone={!outdated}>
              {outdated ? gone : missing(gone)}
            </DrawnRadio>
          )}
        </ItemsContext.Provider>
        <ItemsContext.Provider value={items}>{children}</ItemsContext.Provider>
      </DrawnGroup>
      <Refusal error={bound.error} />
    </>
  );
}

// The label, the radios, the description and the error message.
function DrawnGroup({ label, description, errorMessage, children, className, ...props }: DrawnGroupProps): ReactElement {
  return (
    <AriaRadioGroup {...props} className={composeRenderProps(className, (given) => `bc:group ${styles.field} ${given ?? ""}`)}>
      {label === undefined ? null : <Label className={styles.label}>{label}</Label>}
      <div className="bc:flex bc:gap-1.5 bc:group-data-[orientation=horizontal]:flex-row bc:group-data-[orientation=horizontal]:flex-wrap bc:group-data-[orientation=horizontal]:gap-4 bc:group-data-[orientation=vertical]:flex-col">
        {children}
      </div>
      <Text slot="description" className={`${styles.hint} bc:empty:hidden`}>
        {description}
      </Text>
      <FieldError className={styles.alert}>{errorMessage}</FieldError>
    </AriaRadioGroup>
  );
}

export interface RadioProps extends Omit<RadioFieldProps, "children"> {
  // Its label.
  readonly children?: ReactNode;
  // What the starter kit adds.
  readonly description?: string;
}

export function Radio(props: RadioProps): ReactElement {
  return <DrawnRadio {...props} />;
}

// The circle and the label, and the description, of a radio that tells its
// group its value, or that shows a value the radios lack.
function DrawnRadio({ children, description, gone = false, className, ...props }: RadioProps & { readonly gone?: boolean }): ReactElement {
  const ref = useRef<HTMLDivElement>(null);
  useItem(props.value, ref);
  return (
    <RadioField {...props} ref={ref} className="bc:grid bc:gap-1 bc:font-sans bc:text-text-default">
      <RadioButton
        data-missing={gone || undefined}
        className={composeRenderProps(className, (given) => `bc:group ${RADIO} ${given ?? ""}`)}
      >
        <span className={CIRCLE} />
        {children}
      </RadioButton>
      <Text slot="description" className={`${styles.hint} bc:ml-6 bc:empty:hidden`}>
        {description}
      </Text>
    </RadioField>
  );
}

// The classes of the radio and of its circle, whose border thickens in the
// accent colour where the radio is picked, and takes the colour of danger
// where the group is invalid, as where it shows a value that the radios lack,
// whose label takes that colour too.
const RADIO =
  "bc:flex bc:w-fit bc:cursor-pointer bc:items-center bc:gap-2 bc:text-sm bc:text-text-default bc:data-disabled:cursor-default bc:data-disabled:opacity-50 bc:data-missing:text-text-danger";
const CIRCLE =
  "bc:box-border bc:size-4 bc:shrink-0 bc:rounded-full bc:border bc:border-border-default bc:bg-transparent bc:outline-offset-2 bc:outline-border-accent bc:transition-[border-width] bc:group-data-focus-visible:outline-2 bc:group-data-selected:border-[5px] bc:group-data-selected:border-bg-accent bc:group-data-invalid:border-border-danger";
