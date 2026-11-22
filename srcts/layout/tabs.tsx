// The tabs of core's React SDK: React Aria's Tabs, TabList, Tab and TabPanel
// (https://react-aria.adobe.com/Tabs), in core's theme, after the Tabs of
// React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Tabs.tsx).
// Each takes the props of its part of React Aria.
// The tabs hold the list of tabs and a panel for each tab, of the tab's `id`,
// which shows while its tab is the one picked.
// The list lays the tabs out in a row, or in a column where the tabs'
// `orientation` is "vertical", and scrolls where they outgrow it.
// The picked tab shows on a pill in the accent colour, which slides to the
// tab that the user picks, but not while the user prefers reduced motion.
// The arrow keys move between the tabs, and pick the tab that they move to,
// unless the tabs' `keyboardActivation` is "manual".

import type { ReactElement } from "react";
import {
  composeRenderProps,
  SelectionIndicator,
  Tab as AriaTab,
  TabList as AriaTabList,
  TabPanel as AriaTabPanel,
  Tabs as AriaTabs,
} from "../aria/index.ts";
import type { TabListProps, TabPanelProps, TabProps, TabsProps } from "../aria/index.ts";

export function Tabs({ className, ...props }: TabsProps): ReactElement {
  return (
    <AriaTabs
      {...props}
      className={composeRenderProps(className, (given, { orientation }) => `${TABS} ${ORIENTED[orientation]} ${given ?? ""}`)}
    />
  );
}

export function TabList<T extends object>({ className, ...props }: TabListProps<T>): ReactElement {
  return (
    <AriaTabList
      {...props}
      className={composeRenderProps(className, (given, { orientation }) => `${LIST} ${LISTED[orientation]} ${given ?? ""}`)}
    />
  );
}

export function Tab({ className, children, ...props }: TabProps): ReactElement {
  return (
    <AriaTab {...props} className={composeRenderProps(className, (given) => `${TAB} ${given ?? ""}`)}>
      {composeRenderProps(children, (children) => (
        <>
          {children}
          <SelectionIndicator className={PILL} />
        </>
      ))}
    </AriaTab>
  );
}

export function TabPanel({ className, ...props }: TabPanelProps): ReactElement {
  return <AriaTabPanel {...props} className={composeRenderProps(className, (given) => `${PANEL} ${given ?? ""}`)} />;
}

const TABS = "bc:flex bc:max-w-full bc:gap-4 bc:font-sans bc:text-text-default";
const ORIENTED = { horizontal: "bc:flex-col", vertical: "bc:flex-row" } as const;
const LIST = "bc:-m-1 bc:flex bc:max-w-full bc:overflow-x-auto bc:overflow-y-clip bc:p-1 bc:[scrollbar-width:none]";
const LISTED = { horizontal: "bc:flex-row", vertical: "bc:flex-col bc:items-start" } as const;
// A tab sets up a stacking context of its own, so that the pill sits behind
// its text and above what the tab sits on.
const TAB =
  "bc:relative bc:isolate bc:flex bc:cursor-default bc:items-center bc:rounded-full bc:px-3 bc:py-1.5 bc:font-sans bc:text-sm bc:font-medium bc:text-text-muted bc:outline-none bc:transition-colors bc:forced-color-adjust-none bc:[-webkit-tap-highlight-color:transparent] bc:data-hovered:text-text-default bc:data-selected:text-text-on-accent bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50 bc:forced-colors:data-selected:text-[HighlightText]";
const PILL =
  "bc:absolute bc:inset-0 bc:-z-1 bc:rounded-full bc:bg-bg-accent bc:motion-safe:transition-[translate,width,height] bc:forced-colors:bg-[Highlight]";
const PANEL =
  "bc:box-border bc:flex-1 bc:rounded bc:p-4 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";
