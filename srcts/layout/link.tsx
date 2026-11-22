// The link of core's React SDK: React Aria's Link
// (https://react-aria.adobe.com/Link), in core's theme, after the Link of
// React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Link.tsx).
// It takes the props of React Aria's Link, and the starter kit's `variant`:
// "primary", the default, in the blue of a link, or "secondary", in the
// colour of the text around it.
// It is underlined, more faintly until the pointer rests on it.
// Given an `href`, it is an <a> that goes there, and otherwise an element of
// the role link, whose `onPress` acts.
// A disabled link is not underlined, and does nothing.

import type { ReactElement } from "react";
import { composeRenderProps, Link as AriaLink } from "../aria/index.ts";
import type { LinkProps as AriaLinkProps } from "../aria/index.ts";

export interface LinkProps extends AriaLinkProps {
  readonly variant?: "primary" | "secondary";
}

export function Link({ variant = "primary", className, ...props }: LinkProps): ReactElement {
  return <AriaLink {...props} className={composeRenderProps(className, (given) => `${LINK} ${VARIANTS[variant]} ${given ?? ""}`)} />;
}

const LINK =
  "bc:cursor-pointer bc:rounded-xs bc:underline bc:outline-none bc:transition bc:[-webkit-tap-highlight-color:transparent] bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:no-underline bc:forced-colors:data-disabled:text-[GrayText]";
// The blue of a primary link is blue-700 under the light theme, as that of a
// message's mark is, which reaches the 4.5:1 that WCAG's level AA asks of
// text (../card/footer.tsx).
const VARIANTS = {
  primary:
    "bc:text-blue-700 bc:decoration-blue-700/60 bc:data-hovered:decoration-blue-700 bc:dark:text-blue-400 bc:dark:decoration-blue-400/60 bc:dark:data-hovered:decoration-blue-400",
  secondary: "bc:text-text-default bc:decoration-current/50 bc:data-hovered:decoration-current",
} as const;
