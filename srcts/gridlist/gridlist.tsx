// The grid list of core's React SDK: React Aria's GridList, GridListItem,
// GridListSection and GridListHeader (https://react-aria.adobe.com/GridList),
// in core's theme, after the GridList of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/GridList.tsx).
// Each takes the props of its part of React Aria.
// A grid list shows its items one below the other, as core's lists of
// entries looked before React Aria drew them.
// It draws no frame, so that it adds no frame inside the card or the panel
// that holds it.
// Where its `orientation` is "horizontal", it shows its items side by side.
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
import { Checkbox } from "../inputs/index.ts";

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
          {selectionMode !== "none" && selectionBehavior === "toggle" && <Checkbox slot="selection" />}
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
// A list without items says why in muted text, where an item would stand.
const LIST =
  "bc:grid bc:content-start bc:gap-0.5 bc:overflow-auto bc:font-sans bc:text-text-default bc:outline-none bc:data-[orientation=horizontal]:flex bc:data-[orientation=horizontal]:flex-row bc:data-[orientation=horizontal]:flex-nowrap bc:data-empty:px-2 bc:data-empty:py-0.5 bc:data-empty:text-sm bc:data-empty:text-text-muted bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";
// The item under the pointer shows on a background, and an item that the
// user selects in a tint of the accent colour.
// A press on an item selects it, runs its action or moves the focus to it, so
// the item's text cannot be selected.
const ITEM =
  "bc:flex bc:shrink-0 bc:cursor-pointer bc:select-none bc:items-center bc:gap-2 bc:rounded bc:px-2 bc:py-0.5 bc:text-sm bc:text-text-default bc:outline-none bc:-outline-offset-2 bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent bc:data-hovered:bg-bg-subtle bc:data-pressed:bg-bg-subtle bc:data-selected:bg-bg-accent/10 bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
// The header of a section, in the muted heading of a section of the SDK's
// menu.
const HEADER = "bc:block bc:px-2 bc:pt-1 bc:text-xs bc:font-semibold bc:text-text-muted";
