// The drop zone of core's React SDK: React Aria's DropZone
// (https://react-aria.adobe.com/DropZone), in core's theme, after the
// DropZone of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/DropZone.tsx).
// It takes the props of React Aria's DropZone, whose onDrop hears what the
// user drops on it, such as a file, and holds what it shows, such as a
// FileTrigger with a Button that picks a file the browser's way, and a Text
// in the slot "label", which names the zone.
// It marks itself while the user drags something over it that it takes.
// A drop zone takes no `bound`: a file reaches R through the block class's
// request that receives it, as an upload does
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract).

import type { ReactElement } from "react";
import { composeRenderProps, DropZone as AriaDropZone } from "../aria/index.ts";
import type { DropZoneProps } from "../aria/index.ts";

export function DropZone({ className, ...props }: DropZoneProps): ReactElement {
  return <AriaDropZone {...props} className={composeRenderProps(className, (given) => `${ZONE} ${given ?? ""}`)} />;
}

// The zone is a box with a dashed border, which takes the accent colour while
// the user drags something over it that it takes, and while it has the focus
// of the keyboard, through which the user pastes a file onto it.
const ZONE =
  "bc:box-border bc:flex bc:min-h-20 bc:flex-col bc:items-center bc:justify-center bc:gap-2 bc:rounded-lg bc:border bc:border-dashed bc:border-border-default bc:p-4 bc:text-center bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-1 bc:data-focus-visible:outline-border-accent bc:data-drop-target:border-solid bc:data-drop-target:border-border-accent bc:data-drop-target:bg-bg-subtle bc:data-disabled:opacity-50";
