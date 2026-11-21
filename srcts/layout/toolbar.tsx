// The toolbar of core's React SDK: React Aria's Toolbar and Separator
// (https://react-aria.adobe.com/Toolbar), in core's theme, after the Toolbar
// and the Separator of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Toolbar.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Separator.tsx).
// Each takes the props of its part of React Aria.
// The toolbar lays out the controls that it holds in a row, which wraps, or
// in a column where its `orientation` is "vertical".
// Tab moves the focus into the toolbar and out of it again, and the arrow
// keys move it between the controls.
// A toggle button group in the toolbar lays out its buttons as the toolbar
// lays out its controls, as the starter kit has it.
// The separator is a line between them, across where its `orientation` is
// "horizontal", the default, and upright, as tall as the row, where it is
// "vertical", as between the groups of a toolbar's row.

import type { ReactElement } from "react";
import {
  composeRenderProps,
  Separator as AriaSeparator,
  ToggleButtonGroupContext,
  Toolbar as AriaToolbar,
} from "../aria/index.ts";
import type { SeparatorProps, ToolbarProps } from "../aria/index.ts";

export function Toolbar({ className, ...props }: ToolbarProps): ReactElement {
  return (
    <ToggleButtonGroupContext value={{ orientation: props.orientation }}>
      <AriaToolbar
        {...props}
        className={composeRenderProps(className, (given, { orientation }) => `${TOOLBAR} ${ORIENTED[orientation]} ${given ?? ""}`)}
      />
    </ToggleButtonGroupContext>
  );
}

export function Separator({ className, ...props }: SeparatorProps): ReactElement {
  return <AriaSeparator {...props} className={`${SEPARATOR} ${ACROSS[props.orientation ?? "horizontal"]} ${className ?? ""}`} />;
}

const TOOLBAR = "bc:flex bc:flex-wrap bc:gap-2";
const ORIENTED = { horizontal: "bc:flex-row bc:items-center", vertical: "bc:flex-col bc:items-start" } as const;
const SEPARATOR = "bc:m-0 bc:border-none bc:bg-border-default bc:forced-colors:bg-[ButtonBorder]";
const ACROSS = { horizontal: "bc:h-px bc:w-full", vertical: "bc:w-px bc:self-stretch" } as const;
