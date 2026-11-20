// The switch of core's React SDK: React Aria's SwitchField and SwitchButton,
// drawn as one (https://react-aria.adobe.com/Switch), in core's theme, after
// the Switch of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Switch.tsx).
// It takes what that takes: the props of React Aria's parts, and the
// description and the error message that the starter kit adds.
// It holds its label as its children.
//
// Given `bound`, it shows and edits a boolean that useValue() binds, a block's
// state arg, a board option's value or a value that a function reads from a
// block's state, as the SDK's other inputs do (./inputs.tsx).
// It submits the user's pick at once, and says below itself why the host
// refused what it submitted.

import type { ReactElement, ReactNode } from "react";
import { composeRenderProps, FieldError, SwitchButton, SwitchField, Text } from "../aria/index.ts";
import type { SwitchFieldProps, ValidationResult } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import { Refusal } from "./inputs.tsx";

export interface SwitchProps extends Omit<SwitchFieldProps, "children"> {
  // Its label.
  readonly children?: ReactNode;
  // What the starter kit adds.
  readonly description?: string;
  readonly errorMessage?: string | ((validation: ValidationResult) => string);
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
}

export function Switch({ bound, onChange, ...props }: SwitchProps): ReactElement {
  if (bound === undefined) return <Drawn {...props} onChange={onChange} />;
  return (
    <>
      <Drawn
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

// The track with its handle and the label, the description and the error
// message.
function Drawn({ children, description, errorMessage, className, ...props }: Omit<SwitchProps, "bound">): ReactElement {
  return (
    <SwitchField {...props} className="bc:grid bc:gap-1 bc:font-sans bc:text-text-default">
      <SwitchButton className={composeRenderProps(className, (given) => `bc:group ${SWITCH} ${given ?? ""}`)}>
        <span className={TRACK}>
          <span className={HANDLE} />
        </span>
        {children}
      </SwitchButton>
      {/* React Aria names the description of the switch only where the
          element is there from the start, so it is there while empty too. */}
      <Text slot="description" className={`${styles.hint} bc:empty:hidden`}>
        {description}
      </Text>
      <FieldError className={styles.alert}>{errorMessage}</FieldError>
    </SwitchField>
  );
}

// The classes of the switch, of its track and of the handle, which slides to
// the right and takes the colour on the accent where the switch is on.
const SWITCH =
  "bc:flex bc:w-fit bc:cursor-pointer bc:items-center bc:gap-2 bc:text-sm bc:text-text-default bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
const TRACK =
  "bc:box-border bc:flex bc:h-5 bc:w-9 bc:shrink-0 bc:items-center bc:rounded-full bc:border bc:border-border-default bc:bg-bg-subtle bc:px-px bc:outline-offset-2 bc:outline-border-accent bc:transition-colors bc:group-data-focus-visible:outline-2 bc:group-data-selected:border-border-accent bc:group-data-selected:bg-bg-accent";
const HANDLE =
  "bc:size-4 bc:rounded-full bc:bg-text-muted bc:shadow-xs bc:transition-transform bc:group-data-selected:translate-x-full bc:group-data-selected:bg-text-on-accent";
