// The disclosure of core's React SDK: React Aria's Disclosure, DisclosureGroup
// and DisclosurePanel (https://react-aria.adobe.com/Disclosure), with the
// starter kit's DisclosureHeader, in core's theme, after the Disclosure and
// the DisclosureGroup of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Disclosure.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/DisclosureGroup.tsx).
// Each takes the props of its part of React Aria, such as `defaultExpanded`,
// or `isExpanded` and `onExpandedChange` for a caller that keeps whether the
// disclosure is expanded.
// A disclosure holds its header, whose button expands and collapses it, and
// its panel, which shows while it is expanded, and may hold more between
// them, such as what shows whether it is expanded or not.
// The header is a heading, of the `level` given, 3 by default, which holds
// the button: its title after a triangle, which points at the title while the
// disclosure is collapsed, and down while it is expanded, as a browser draws
// one before the summary of a <details>. The button takes the heading's face,
// in semibold at text-base, or in what the header's `className` gives the
// heading in place of that.
// The panel's height moves as it expands and collapses, but not while the
// user prefers reduced motion. A collapsed panel keeps what it holds on the
// page, hidden, where the browser's search in the page still finds it, and a
// match there expands the disclosure.
// The panel sets no padding of its own, since its height moves to 0: what it
// holds sets its own.
// A group of disclosures keeps one expanded at a time, unless it
// `allowsMultipleExpanded`.

import { useContext } from "react";
import type { ReactElement, ReactNode } from "react";
import {
  Button,
  composeRenderProps,
  Disclosure as AriaDisclosure,
  DisclosurePanel as AriaDisclosurePanel,
  DisclosureStateContext,
  Heading,
} from "../aria/index.ts";
import type { DisclosurePanelProps, DisclosureProps } from "../aria/index.ts";

export function Disclosure({ className, ...props }: DisclosureProps): ReactElement {
  return <AriaDisclosure {...props} className={composeRenderProps(className, (given) => `${DISCLOSURE} ${given ?? ""}`)} />;
}

export interface DisclosureHeaderProps {
  readonly children: ReactNode;
  readonly level?: number;
  readonly className?: string;
}

export function DisclosureHeader({ children, level = 3, className }: DisclosureHeaderProps): ReactElement {
  const expanded = useContext(DisclosureStateContext)?.isExpanded ?? false;
  return (
    <Heading level={level} className={`${HEADING} ${className ?? FACE}`}>
      <Button slot="trigger" className={TRIGGER}>
        <Marker expanded={expanded} />
        {children}
      </Button>
    </Heading>
  );
}

export function DisclosurePanel({ className, ...props }: DisclosurePanelProps): ReactElement {
  return <AriaDisclosurePanel {...props} className={composeRenderProps(className, (given) => `${PANEL} ${given ?? ""}`)} />;
}

// The triangle before the header's title.
function Marker({ expanded }: { expanded: boolean }): ReactElement {
  return (
    <svg
      aria-hidden="true"
      viewBox="0 0 16 16"
      fill="currentColor"
      className={`bc:size-3 bc:shrink-0 bc:transition-transform bc:motion-reduce:transition-none ${expanded ? "bc:rotate-90" : ""}`}
    >
      <path d="M5.5 3.5 11 8l-5.5 4.5Z" />
    </svg>
  );
}

const DISCLOSURE = "bc:font-sans bc:text-text-default";
const HEADING = "bc:m-0 bc:font-sans";
const FACE = "bc:text-base bc:font-semibold";
// The button takes the face of the heading, and fills its width.
const TRIGGER =
  "bc:flex bc:w-full bc:cursor-pointer bc:items-center bc:gap-1.5 bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-left bc:text-inherit bc:[font:inherit] bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
const PANEL = "bc:h-(--disclosure-panel-height) bc:overflow-clip bc:motion-safe:transition-[height]";
