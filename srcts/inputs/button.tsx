// The button of core's React SDK: React Aria's Button
// (https://react-aria.adobe.com/Button), in core's theme, after the Button of
// React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Button.tsx).
// It takes the props of React Aria's Button, and the starter kit's `variant`,
// one of core's four looks of a button: "primary", in the accent colour,
// "secondary", with a border, "destructive", in the danger colour, and
// "quiet", with no border.
// It takes `aria-keyshortcuts` too, which names the button's shortcut to
// assistive technology, and which React Aria's Button passes on to no
// element.
// The menus and dialogs of the SDK open from it, since React Aria opens them
// only from a button of its own, of the one copy of React Aria on the page.

import type { ReactElement } from "react";
import { Button as AriaButton, composeRenderProps } from "../aria/index.ts";
import type { ButtonProps as AriaButtonProps } from "../aria/index.ts";
import { buttons } from "../styles/index.ts";

export interface ButtonProps extends AriaButtonProps {
  readonly variant?: keyof typeof buttons;
  readonly "aria-keyshortcuts"?: string;
}

export function Button({ variant = "primary", className, "aria-keyshortcuts": shortcuts, ...props }: ButtonProps): ReactElement {
  return (
    <AriaButton
      {...props}
      render={
        shortcuts === undefined || props.render !== undefined
          ? props.render
          : (dom) => <button {...dom} aria-keyshortcuts={shortcuts} />
      }
      className={composeRenderProps(className, (given) => `${buttons[variant]} ${given ?? ""}`)}
    />
  );
}
