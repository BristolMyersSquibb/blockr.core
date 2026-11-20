// The button of core's React SDK: React Aria's Button
// (https://react-aria.adobe.com/Button), in core's theme, after the Button of
// React Aria's Tailwind starter kit
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
