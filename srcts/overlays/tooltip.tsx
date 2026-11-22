// The tooltip of core's React SDK: React Aria's TooltipTrigger and Tooltip
// (https://react-aria.adobe.com/Tooltip), in core's theme, after the Tooltip
// of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Tooltip.tsx).
// Each takes the props of its part of React Aria.
// The trigger holds the element that the tooltip names and the tooltip.
// That element takes the focus, as a Button or a Link of the SDK does, or a
// child of React Aria's Focusable, which has a role such as "button", so
// that the user reaches the tooltip with the keyboard as well.
// React Aria shows the tooltip as the pointer rests on the element, after
// 1.5 seconds, at once while another tooltip shows or has just gone, and at
// once as the element takes the focus from the keyboard. It hides the
// tooltip as the pointer or the focus leaves, on Escape and as the user
// presses the element.
// It shows no tooltip for a disabled element, which takes neither the
// pointer nor the focus, nor for a touch, which has no hover, so a UI says
// what the user must know in another way as well.
// React Aria draws the tooltip in a portal at the end of the page's body,
// above the element where the window has room, with an arrow that points at
// the element.
// It stacks at the z-index of the overlays of core's theme, in place of the
// one that React Aria writes on it (../runtime/index.css).
// It draws on the background that core's theme gives what sits over the
// board, bg-raised, as a light card.

import type { ReactElement, ReactNode } from "react";
import { composeRenderProps, OverlayArrow, Tooltip as AriaTooltip } from "../aria/index.ts";
import type { TooltipProps as AriaTooltipProps } from "../aria/index.ts";
import { overlayStyle } from "./stacking.ts";

export interface TooltipProps extends Omit<AriaTooltipProps, "children"> {
  readonly children: ReactNode;
}

export function Tooltip({ children, className, style, offset = 8, ...props }: TooltipProps): ReactElement {
  return (
    <AriaTooltip
      {...props}
      offset={offset}
      style={overlayStyle(style)}
      className={composeRenderProps(className, (given) => `${TOOLTIP} ${given ?? ""}`)}
    >
      <OverlayArrow>
        <svg width={8} height={8} viewBox="0 0 8 8" className={ARROW} aria-hidden="true">
          <path d="M0 0 L4 4 L8 0" />
        </svg>
      </OverlayArrow>
      {children}
    </AriaTooltip>
  );
}

const TOOLTIP =
  "bc:group bc:box-border bc:max-w-xs bc:rounded-md bc:border bc:border-border-default bc:bg-bg-raised bc:px-2 bc:py-1 bc:font-sans bc:text-xs bc:text-gray-900 bc:shadow-md bc:outline-none bc:will-change-transform bc:dark:text-gray-100";
// The arrow points down from a tooltip above its element, and turns to point
// at the element wherever else the tooltip sits.
const ARROW =
  "bc:block bc:fill-bg-raised bc:stroke-border-default bc:forced-colors:fill-[Canvas] bc:forced-colors:stroke-[ButtonBorder] bc:group-data-[placement=bottom]:rotate-180 bc:group-data-[placement=left]:-rotate-90 bc:group-data-[placement=right]:rotate-90";
