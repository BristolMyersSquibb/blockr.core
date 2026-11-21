// What core's React SDK offers to drag the items of its Table, GridList and
// Tree, and to drop onto them: React Aria's useDragAndDrop and DropIndicator
// (https://react-aria.adobe.com/dnd), the latter in core's theme.
// A caller hands what useDragAndDrop returns to the list as its
// `dragAndDropHooks`.
// The list then shows where a dragged item would land by a DropIndicator: a
// line in the accent colour.
// React Aria's own indicator has no style.
// So useDragAndDrop draws the SDK's DropIndicator, unless the caller's
// `renderDropIndicator` draws another.
// Each item that drags starts with a button that drags it.
// This module gives that button to the Table, the GridList and the Tree.
// A keyboard user drags the item from the button too.

import type { ReactElement } from "react";
import { composeRenderProps, DropIndicator as AriaDropIndicator, useDragAndDrop as useAriaDragAndDrop } from "../aria/index.ts";
import type { DragAndDropHooks, DragAndDropOptions, DropIndicatorProps } from "../aria/index.ts";
import { Button } from "../inputs/index.ts";

export type { DragAndDropHooks, DragAndDropOptions, DropIndicatorProps };

export function useDragAndDrop<T = object>(options: DragAndDropOptions<T>): { dragAndDropHooks: DragAndDropHooks<T> } {
  return useAriaDragAndDrop<T>({ renderDropIndicator: (target) => <DropIndicator target={target} />, ...options });
}

export function DropIndicator(props: DropIndicatorProps): ReactElement {
  return <AriaDropIndicator {...props} className={composeRenderProps(props.className, (given) => `${INDICATOR} ${given ?? ""}`)} />;
}

// The button that drags an item.
// React Aria labels it, and hands it its handlers through the slot "drag".
export function DragButton(): ReactElement {
  return (
    <Button slot="drag" variant="quiet">
      <Grip />
    </Button>
  );
}

// The grip of the button that drags an item.
function Grip(): ReactElement {
  return (
    <svg width="12" height="12" viewBox="0 0 12 12" fill="currentColor" aria-hidden="true">
      <circle cx="4" cy="2.5" r="1" />
      <circle cx="8" cy="2.5" r="1" />
      <circle cx="4" cy="6" r="1" />
      <circle cx="8" cy="6" r="1" />
      <circle cx="4" cy="9.5" r="1" />
      <circle cx="8" cy="9.5" r="1" />
    </svg>
  );
}

// React Aria lays the indicator out between two items, where it takes no
// height, so its outline draws a line there.
const INDICATOR = "bc:outline-none bc:data-drop-target:outline-1 bc:data-drop-target:outline-bg-accent bc:forced-colors:data-drop-target:outline-[Highlight]";
