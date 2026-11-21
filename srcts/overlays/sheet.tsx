// The sheet of core's React SDK: React Aria's SheetOverlay, Sheet,
// SheetBackdrop and SheetContent (https://react-aria.adobe.com/Sheet), drawn
// as one, in core's theme, after the Sheet of React Aria's Tailwind starter
// kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Sheet.tsx).
// It takes the props of React Aria's SheetOverlay, such as `position`, the
// edge of the window that the sheet slides in from, the bottom by default,
// and `snapPoints`, and the starter kit's `overscrollPadding`, true by
// default, which makes the sheet seem to go on past that edge as the user
// drags it beyond.
// It is a modal dialog, as the SDK's Modal is, which opens from a
// DialogTrigger, holds its children in a dialog, keeps the focus inside it
// and makes the rest of the page inert while it is open.
// The user closes it by dragging it back out, by Escape, or by a press on
// the backdrop.
// Only the sheet at the bottom of a stack of sheets darkens the page, and a
// sheet that opens over another moves the other back.
// React Aria draws it in a portal at the end of the page's body.
// It stacks at the z-index of the overlays of core's theme
// (../runtime/index.css).
// It draws on the background that core's theme gives what sits over the
// board, bg-raised.

import type { ReactElement } from "react";
import {
  composeRenderProps,
  Sheet as AriaSheet,
  SheetBackdrop,
  SheetContent,
  SheetOverlay,
} from "../aria/index.ts";
import type { DialogProps, SheetOverlayProps } from "../aria/index.ts";

export interface SheetProps extends Omit<SheetOverlayProps, "children"> {
  // What the sheet holds, which it draws in a dialog.
  readonly children?: DialogProps["children"];
  readonly overscrollPadding?: boolean;
}

export function Sheet({ children, className, overscrollPadding = true, ...props }: SheetProps): ReactElement {
  const { snapPoints } = props;
  return (
    <SheetOverlay {...props} className={composeRenderProps(className, (given) => `bc:group/sheet bc:z-overlay ${given ?? ""}`)}>
      <SheetBackdrop
        className={BACKDROP}
        swipeAnimation="bc-sheet-backdrop"
        swipeAnimationRange={snapPoints === undefined ? undefined : { start: snapPoints.length - 1 }}
      />
      <AriaSheet
        overscrollPadding={overscrollPadding}
        stackAnimation="bc-sheet-scale-back"
        className={({ position }) => `${SHEET} ${POSITIONS[position]}`}
      >
        <SheetContent className={CONTENT}>{children}</SheetContent>
      </AriaSheet>
    </SheetOverlay>
  );
}

// The backdrop darkens the page under the first sheet of a stack alone.
const BACKDROP = "bc:data-[stack-index=0]:bg-black/40";
// The sheet sets up a space in three dimensions, in which a sheet that opens
// over it moves it back.
const SHEET =
  "bc:z-[1] bc:overflow-clip bc:rounded-2xl bc:border bc:border-border-default bc:bg-bg-raised bc:font-sans bc:text-gray-900 bc:shadow-2xl bc:outline-hidden bc:will-change-transform bc:[transform:perspective(1000px)] bc:forced-colors:bg-[Canvas] bc:dark:text-gray-100";
const POSITIONS: Readonly<Record<"bottom" | "top" | "center" | "left" | "right", string>> = {
  bottom: "bc:max-h-[calc(100%-32px)] bc:w-[calc(100%-1rem)] bc:max-w-[800px] bc:origin-[center_-150px]",
  top: "bc:max-h-[calc(100%-32px)] bc:w-[calc(100%-1rem)] bc:max-w-[800px] bc:origin-[center_150px]",
  center: "bc:max-h-[calc(100%-32px)] bc:w-[calc(100%-1rem)] bc:max-w-[800px] bc:origin-[center_-150px]",
  left: "bc:h-[calc(100%-1rem)] bc:w-[300px] bc:origin-[150px_center]",
  right: "bc:h-[calc(100%-1rem)] bc:w-[300px] bc:origin-[-150px_center]",
};
// The content scrolls once the sheet is open in full. Before then, a swipe on
// it moves the sheet, as on iOS.
const CONTENT =
  "bc:box-border bc:h-full bc:overflow-hidden bc:overscroll-auto bc:p-6 bc:pb-[calc(--spacing(6)+var(--sheet-scroll-padding-y))] bc:outline-hidden bc:[scrollbar-gutter:stable] bc:group-data-[expanded]/sheet:overflow-auto";
