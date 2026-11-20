// The grid list of core's React SDK: React Aria's GridList, GridListItem,
// GridListSection and GridListHeader (https://react-aria.adobe.com/GridList),
// in core's theme, after the GridList of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/GridList.tsx).
// Each takes the props of its part of React Aria.
// A grid list shows its items one below the other, inside a border of its
// own.
// Where its `orientation` is "horizontal", it shows them side by side.
// Where it has no item, it shows what `renderEmptyState` gives.
// The user moves the focus through its items by the arrow keys.
// A press or Enter runs the item's `onAction`, or the list's.
// Where the list's `selectionMode` is "single" or "multiple", each item
// starts with a checkbox that selects it.
// Where the list's `dragAndDropHooks` let its items drag, each item starts
// with a button that drags it.
// A keyboard user drags the item from that button too.
// A section groups items under the header that it holds.

import type { HTMLAttributes, ReactElement } from "react";
import {
  composeRenderProps,
  GridList as AriaGridList,
  GridListHeader as AriaGridListHeader,
  GridListItem as AriaGridListItem,
  GridListSection,
} from "../aria/index.ts";
import type { GridListItemProps, GridListProps } from "../aria/index.ts";
import { DragButton } from "../dnd/index.ts";
import { SelectionCheckbox } from "../inputs/index.ts";

export { GridListSection };

export function GridList<T extends object>(props: GridListProps<T>): ReactElement {
  return <AriaGridList {...props} className={composeRenderProps(props.className, (given) => `${LIST} ${given ?? ""}`)} />;
}

export function GridListItem({ children, ...props }: GridListItemProps): ReactElement {
  const textValue = props.textValue ?? (typeof children === "string" ? children : undefined);
  return (
    <AriaGridListItem {...props} textValue={textValue} className={composeRenderProps(props.className, (given) => `${ITEM} ${given ?? ""}`)}>
      {composeRenderProps(children, (children, { selectionMode, selectionBehavior, allowsDragging }) => (
        <>
          {allowsDragging && <DragButton />}
          {selectionMode !== "none" && selectionBehavior === "toggle" && <SelectionCheckbox />}
          {children}
        </>
      ))}
    </AriaGridListItem>
  );
}

export function GridListHeader({ className, ...props }: HTMLAttributes<HTMLElement>): ReactElement {
  return <AriaGridListHeader {...props} className={`${HEADER} ${className ?? ""}`} />;
}

// The classes of the list and its parts.
// A list without items centres what it says instead.
const LIST =
  "bc:relative bc:box-border bc:overflow-auto bc:rounded-lg bc:border bc:border-border-default bc:font-sans bc:text-text-default bc:outline-none bc:data-[orientation=horizontal]:flex bc:data-[orientation=horizontal]:flex-row bc:data-[orientation=horizontal]:flex-nowrap bc:data-empty:flex bc:data-empty:items-center bc:data-empty:justify-center bc:data-empty:p-2 bc:data-empty:text-sm bc:data-empty:italic bc:data-empty:text-text-muted bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";
// A line divides each item from the one before it: across the list where the
// list is vertical, and down the list where it is horizontal.
// An item that the user selects shows in a tint of the accent colour.
// A press on an item selects it, runs its action or moves the focus to it, so
// the item's text cannot be selected.
const ITEM =
  "bc:relative bc:flex bc:cursor-default bc:select-none bc:items-center bc:gap-2 bc:border-transparent bc:px-2 bc:py-1 bc:text-sm bc:text-text-default bc:outline-none bc:-outline-offset-2 bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-hovered:bg-bg-subtle bc:data-pressed:bg-bg-subtle bc:data-selected:z-20 bc:data-selected:bg-bg-accent/10 bc:data-disabled:opacity-50 bc:data-disabled:z-10 bc:[[data-orientation=vertical]_&]:border-t bc:[[data-orientation=vertical]_&]:border-t-border-default/40 bc:[[data-orientation=vertical]_&]:first:border-t-0 bc:[[data-orientation=vertical]_&]:first:rounded-t-lg bc:[[data-orientation=vertical]_&]:last:rounded-b-lg bc:[[data-orientation=horizontal]_&]:shrink-0 bc:[[data-orientation=horizontal]_&]:border-l bc:[[data-orientation=horizontal]_&]:border-l-border-default/40 bc:[[data-orientation=horizontal]_&]:first:border-l-0 bc:[[data-orientation=horizontal]_&]:first:rounded-s-lg bc:[[data-orientation=horizontal]_&]:last:rounded-e-lg";
// The header of a section lies across the line above its first item.
const HEADER =
  "bc:relative bc:z-10 bc:-mt-px bc:border-y bc:border-border-default bc:bg-bg-subtle bc:px-2 bc:py-1 bc:text-sm bc:font-semibold bc:text-text-muted";
