// The tree of core's React SDK: React Aria's Tree and TreeItem
// (https://react-aria.adobe.com/Tree), in core's theme, after the Tree of
// React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Tree.tsx).
// Each takes the props of its part of React Aria.
// An item also takes the starter kit's `title`, the text that the item shows.
// An item holds the items below it as its children.
// It shows them while it is expanded, each indented by its level.
// An item with items below it starts with a button that expands it and
// collapses it again, as the right and left arrow keys do.
// Where the tree's `selectionMode` is "single" or "multiple", each item
// starts with a checkbox that selects it.
// Where the tree's `dragAndDropHooks` let its items drag, each item starts
// with a button that drags it.
// A keyboard user drags the item from that button too.
// The starter kit's GridList and Table draw that button, while its Tree does
// not.

import type { ReactElement } from "react";
import {
  Button,
  composeRenderProps,
  Tree as AriaTree,
  TreeItem as AriaTreeItem,
  TreeItemContent,
} from "../aria/index.ts";
import type { TreeItemProps as AriaTreeItemProps, TreeProps } from "../aria/index.ts";
import { DragButton } from "../dnd/index.ts";
import { SelectionCheckbox } from "../inputs/index.ts";

export function Tree<T extends object>(props: TreeProps<T>): ReactElement {
  return <AriaTree {...props} className={composeRenderProps(props.className, (given) => `${TREE} ${given ?? ""}`)} />;
}

export interface TreeItemProps extends Partial<AriaTreeItemProps> {
  readonly title: string;
}

export function TreeItem({ title, children, ...props }: TreeItemProps): ReactElement {
  return (
    <AriaTreeItem textValue={title} {...props} className={composeRenderProps(props.className, (given) => `${ITEM} ${given ?? ""}`)}>
      <TreeItemContent>
        {({ selectionMode, selectionBehavior, hasChildItems, isExpanded, allowsDragging }) => (
          <div className="bc:flex bc:min-w-0 bc:flex-1 bc:items-center bc:gap-1">
            {allowsDragging && <DragButton />}
            {selectionMode !== "none" && selectionBehavior === "toggle" && <SelectionCheckbox />}
            <div className={INDENT} />
            {hasChildItems ? (
              <Button slot="chevron" className={EXPAND}>
                <Chevron expanded={isExpanded} />
              </Button>
            ) : (
              <div className="bc:size-6 bc:shrink-0" />
            )}
            <span className="bc:min-w-0 bc:truncate">{title}</span>
          </div>
        )}
      </TreeItemContent>
      {children}
    </AriaTreeItem>
  );
}

// The chevron of an item that holds other items.
// It points at the item's title while the item is collapsed, and down while
// the item is expanded.
export function Chevron({ expanded }: { expanded: boolean }): ReactElement {
  return (
    <svg
      width="14"
      height="14"
      viewBox="0 0 16 16"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.5"
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
      className={`bc:text-text-muted bc:transition-transform bc:motion-reduce:transition-none ${expanded ? "bc:rotate-90" : ""}`}
    >
      <path d="M6 3.5 10.5 8 6 12.5" />
    </svg>
  );
}

// The classes of the tree and its parts.
const TREE =
  "bc:relative bc:box-border bc:max-w-full bc:overflow-auto bc:rounded-lg bc:border bc:border-border-default bc:font-sans bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";
// An item that the user selects shows in a tint of the accent colour.
// A press on an item selects it or moves the focus to it, so the item's text
// cannot be selected.
const ITEM =
  "bc:group bc:relative bc:flex bc:cursor-default bc:items-center bc:border-t bc:border-transparent bc:px-2 bc:py-0.5 bc:font-sans bc:text-sm bc:text-text-default bc:select-none bc:outline-none bc:-outline-offset-2 bc:first:rounded-t-lg bc:first:border-t-0 bc:last:rounded-b-lg bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-hovered:bg-bg-subtle bc:data-pressed:bg-bg-subtle bc:data-selected:z-20 bc:data-selected:bg-bg-accent/10 bc:data-disabled:z-10 bc:data-disabled:opacity-50";
// The space that indents an item by its level, which React Aria sets in
// --tree-item-level, from 1 at the top.
const INDENT = "bc:w-[calc((var(--tree-item-level)_-_1)_*_var(--bc-spacing)_*_3)] bc:shrink-0";
const EXPAND =
  "bc:flex bc:size-6 bc:shrink-0 bc:cursor-default bc:items-center bc:justify-center bc:rounded bc:border-0 bc:bg-transparent bc:p-0 bc:text-text-default bc:outline-none bc:[-webkit-tap-highlight-color:transparent] bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:opacity-50";
