// The buttons of core's React SDK, React Aria's Button
// (https://react-aria.adobe.com/Button) in core's theme: the button, and the
// button inside a field below.
// The button is drawn after the Button of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Button.tsx).
// It takes the props of React Aria's Button, and the starter kit's `variant`,
// one of core's three looks of a button: "primary", in the accent colour,
// "secondary", with a border, and "quiet", with none.
// The menus and dialogs of the SDK open from it, since React Aria opens them
// only from a button of its own, of the one copy of React Aria on the page.
// The button also takes a ref, which holds the button's element.
// React hands a ref to a component as one of its props, so the ref reaches
// React Aria's Button with the other props.

import type { ReactElement, RefAttributes } from "react";
import { Button as AriaButton, composeRenderProps } from "../aria/index.ts";
import type { ButtonProps as AriaButtonProps } from "../aria/index.ts";
import { buttons } from "../styles/index.ts";

export interface ButtonProps extends AriaButtonProps, RefAttributes<HTMLButtonElement> {
  readonly variant?: keyof typeof buttons;
}

export function Button({ variant = "primary", className, ...props }: ButtonProps): ReactElement {
  return <AriaButton {...props} className={composeRenderProps(className, (given) => `${buttons[variant]} ${given ?? ""}`)} />;
}

// A button inside a field, such as the button of the select's field that
// clears the pick, or of the search field that clears the search: React Aria's
// Button in core's theme, after the FieldButton of the starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/FieldButton.tsx).
// It takes the props of React Aria's Button.
// A field of React Aria takes a button inside it for its own, as the select
// opens its list with it, unless the button sets `slot={null}`.
export function FieldButton({ className, ...props }: AriaButtonProps): ReactElement {
  return <AriaButton {...props} className={composeRenderProps(className, (given) => `${FIELD_BUTTON} ${given ?? ""}`)} />;
}

const FIELD_BUTTON =
  "bc:flex bc:size-5 bc:shrink-0 bc:cursor-pointer bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-muted bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-hovered:text-text-default bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
