// The navigation tree of core's React SDK: React Aria's NavigationTree,
// NavigationTreeItem, NavigationTreeItemContent, NavigationTreeSection and
// NavigationTreeHeader (https://react-aria.adobe.com/NavigationTree), in
// core's theme, after the NavigationTree of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/NavigationTree.tsx),
// with its NavigationTreeItemLink.
// Each takes the props of its part of React Aria.
// The tree also takes the starter kit's `renderLink`, and the link takes the
// props of React Aria's Link.
// It is a tree of links, such as to the pages of an app.
// It draws no frame, so that it adds no frame inside the panel or the popover
// that holds it.
// It marks the item of the current page.
// An item holds its content and the items below it.
// The content holds the item's link.
// The item shows the items below it while it is expanded, each indented by
// its level.
// An item with items below it ends with a button that expands it and
// collapses it again.
// A line at the left edge of the current item takes the accent colour.
// A collapsed item above the current one shows a dot there, and the item
// under the pointer shows a muted line.
// A section groups items under its header.
// A tree whose links lead within the page, as a router's links do, draws each
// link through `renderLink`, which React Aria's Link takes as its `render`.

import { createContext, useContext } from "react";
import type { ReactElement, ReactNode } from "react";
import {
  Button,
  composeRenderProps,
  Link,
  NavigationTree as AriaNavigationTree,
  NavigationTreeHeader as AriaNavigationTreeHeader,
  NavigationTreeItem as AriaNavigationTreeItem,
  NavigationTreeItemContent as AriaNavigationTreeItemContent,
  NavigationTreeSection as AriaNavigationTreeSection,
} from "../aria/index.ts";
import type {
  LinkProps,
  NavigationTreeHeaderProps,
  NavigationTreeItemProps,
  NavigationTreeProps as AriaNavigationTreeProps,
  NavigationTreeSectionProps,
} from "../aria/index.ts";
import { Chevron } from "./tree.tsx";

export interface NavigationTreeProps<T> extends AriaNavigationTreeProps<T> {
  readonly renderLink?: LinkProps["render"];
}

// What an item's link shows at its left edge: the current item, or a
// collapsed item above it.
type Indicator = "current" | "ancestor" | undefined;

const IndicatorContext = createContext<Indicator>(undefined);
const RenderLinkContext = createContext<LinkProps["render"]>(undefined);

export function NavigationTree<T extends object>({ renderLink, ...props }: NavigationTreeProps<T>): ReactElement {
  return (
    <RenderLinkContext.Provider value={renderLink}>
      <AriaNavigationTree {...props} className={composeRenderProps(props.className, (given) => `${TREE} ${given ?? ""}`)} />
    </RenderLinkContext.Provider>
  );
}

export function NavigationTreeItem(props: NavigationTreeItemProps): ReactElement {
  return <AriaNavigationTreeItem {...props} className={composeRenderProps(props.className, (given) => `${ITEM} ${given ?? ""}`)} />;
}

export function NavigationTreeItemContent({ children }: { children?: ReactNode }): ReactElement {
  return (
    <AriaNavigationTreeItemContent>
      {({ level, hasChildItems, isExpanded, isCurrent, isCurrentAncestor }) => (
        <>
          {level > 1 && <div className="bc:shrink-0" style={{ width: `calc((${String(level)} - 1) * var(--bc-spacing) * 4)` }} />}
          <IndicatorContext.Provider value={isCurrent ? "current" : isCurrentAncestor && !isExpanded ? "ancestor" : undefined}>
            {children}
          </IndicatorContext.Provider>
          <Button slot="chevron" isDisabled={!hasChildItems} className={`${EXPAND} ${hasChildItems ? "" : "bc:invisible"}`}>
            <Chevron expanded={isExpanded} />
          </Button>
        </>
      )}
    </AriaNavigationTreeItemContent>
  );
}

export function NavigationTreeSection<T extends object>(props: NavigationTreeSectionProps<T>): ReactElement {
  return <AriaNavigationTreeSection {...props} className="bc:not-first:mt-4" />;
}

export function NavigationTreeHeader(props: NavigationTreeHeaderProps): ReactElement {
  return <AriaNavigationTreeHeader {...props} className="bc:px-2 bc:py-1 bc:text-sm bc:font-semibold bc:text-text-muted" />;
}

export interface NavigationTreeItemLinkProps extends Omit<LinkProps, "children"> {
  readonly children?: ReactNode;
}

export function NavigationTreeItemLink({ render, children, ...props }: NavigationTreeItemLinkProps): ReactElement {
  const indicator = useContext(IndicatorContext);
  const given = useContext(RenderLinkContext);
  return (
    <Link render={render ?? given} {...props} className={LINK}>
      {({ isHovered }) => (
        <>
          <span aria-hidden="true" data-indicator={indicator ?? (isHovered ? "hover" : undefined)} className={MARK} />
          {children}
        </>
      )}
    </Link>
  );
}

// The classes of the tree and its parts.
const TREE = "bc:flex bc:flex-col bc:gap-0.5 bc:overflow-auto bc:font-sans bc:text-text-default bc:outline-none";
// The item draws the outline of the focus across the whole row, since its
// link takes the focus.
const ITEM =
  "bc:group bc:relative bc:flex bc:cursor-default bc:items-center bc:rounded-md bc:font-sans bc:text-text-default bc:select-none bc:outline-none bc:-outline-offset-2 bc:[-webkit-tap-highlight-color:transparent] bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:opacity-50";
// An item without a page has no link, so its title shows no pointer.
const LINK =
  "bc:relative bc:flex bc:min-w-0 bc:flex-1 bc:cursor-pointer bc:items-center bc:gap-2 bc:px-2 bc:py-1.5 bc:text-sm bc:text-current bc:no-underline bc:outline-none bc:group-[:not([data-href])]:cursor-default bc:data-disabled:cursor-default";
// The line at the left edge of an item's link: the accent colour on the
// current item, even under the pointer, a dot on a collapsed item above it,
// and a muted line on an item with a page under the pointer.
const MARK =
  "bc:absolute bc:start-0.5 bc:top-1/2 bc:h-[1lh] bc:w-0.5 bc:-translate-y-1/2 bc:rounded-full bc:forced-color-adjust-none bc:group-data-href:data-[indicator=hover]:bg-border-default bc:data-[indicator=current]:bg-bg-accent bc:data-[indicator=ancestor]:size-1 bc:data-[indicator=ancestor]:bg-border-default bc:forced-colors:data-indicator:bg-[Highlight]";
const EXPAND =
  "bc:flex bc:size-6 bc:shrink-0 bc:cursor-default bc:items-center bc:justify-center bc:rounded-md bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-default bc:outline-none bc:-outline-offset-2 bc:[-webkit-tap-highlight-color:transparent] bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";
