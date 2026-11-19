// The popover of core's React SDK: React Aria's Popover
// (https://react-aria.adobe.com/Popover), in core's theme, after the Popover
// of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Popover.tsx).
// It takes the props of React Aria's Popover.
// React Aria draws it in a portal at the end of the page's body, beside the
// element that it hangs from, and above a modal that is open.
// It draws on the background that core's theme gives what sits over the
// board, bg-raised.

import type { ReactElement } from "react";
import { composeRenderProps, Popover as AriaPopover } from "../aria/index.ts";
import type { PopoverProps } from "../aria/index.ts";

export function Popover({ className, offset = 4, ...props }: PopoverProps): ReactElement {
  return <AriaPopover {...props} offset={offset} className={composeRenderProps(className, (given) => `${POPOVER} ${given ?? ""}`)} />;
}

const POPOVER =
  "bc:overflow-auto bc:rounded-md bc:border bc:border-border-default bc:bg-bg-raised bc:font-sans bc:text-sm bc:text-gray-900 bc:shadow-lg bc:outline-none bc:dark:text-gray-100";
