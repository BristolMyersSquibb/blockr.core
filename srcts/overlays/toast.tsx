// The toasts of core's React SDK: React Aria's ToastQueue, ToastRegion, Toast
// and ToastContent (https://react-aria.adobe.com/Toast), in core's theme,
// after the Toast of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Toast.tsx).
// React Aria 1.22 still names them UNSTABLE_, so a minor release may change
// them, and the SDK names them without the prefix, as the starter kit does.
// Each takes the props of its part of React Aria.
//
// A UI keeps its toasts in a ToastQueue, adds one with `add()`, which takes
// what the toast shows and returns the toast's key, and closes one with
// `close()`, given the key.
// A toast added with a `timeout`, in milliseconds, closes once it is up, and
// fades in its last 300 milliseconds, but not while the user prefers reduced
// motion. One added without a timeout stays until the user or the UI closes
// it.
// Its `onClose` runs whichever way it closes.
//
// The region draws the toasts of a queue in the window's bottom right corner,
// one above the other, the latest lowest, each as the function that it is
// given draws the toast.
// It is a landmark, which F6 moves the keyboard's focus to, and it lets
// clicks through between the toasts, so that the page below takes them.
// The column scrolls where the toasts outgrow the window, and its padding
// keeps their shadows from being cut off there.
// While the pointer rests on the region or the focus is in it, React Aria
// pauses the timeout of every toast, and the fade with it.
// React Aria marks the region as a layer of its own, which a modal leaves
// within the user's reach, so that the user can still close a toast while a
// dialog is open.
// The region sits one above the overlays of core's theme
// (../runtime/index.css). React Aria draws it in a portal at the end of the
// page's body only while it holds a toast, so it can come before a modal on
// the page, and would otherwise show beneath the modal's backdrop.
//
// A toast draws its ToastContent, which holds what it says, with a title and
// a description in React Aria's Text of the slots "title" and "description",
// beside a Button of the SDK of the slot "close", which closes the toast.
// React Aria makes the content an alert, which a screen reader reads out at
// once, unless its `role` says otherwise, as "status" does.
// A toast draws on the background that core's theme gives what sits over the
// board, bg-raised.

import type { CSSProperties, ReactElement } from "react";
import { composeRenderProps, UNSTABLE_Toast as AriaToast, UNSTABLE_ToastRegion as AriaToastRegion } from "../aria/index.ts";
import type { ToastProps, ToastRegionProps } from "../aria/index.ts";

export { UNSTABLE_ToastContent as ToastContent, UNSTABLE_ToastQueue as ToastQueue } from "../aria/index.ts";
export type { QueuedToast } from "../aria/index.ts";

// How long a toast takes to fade, in milliseconds, at the end of its timeout.
const FADES_FOR = 300;

export function ToastRegion<T>({ className, ...props }: ToastRegionProps<T>): ReactElement {
  return <AriaToastRegion {...props} className={composeRenderProps(className, (given) => `${REGION} ${given ?? ""}`)} />;
}

export function Toast<T>({ className, style, ...props }: ToastProps<T>): ReactElement {
  const { timeout } = props.toast;
  const fade: CSSProperties | undefined =
    timeout === undefined ? undefined : ({ "--bc-toast-fade-at": `${String(Math.max(0, timeout - FADES_FOR))}ms` } as CSSProperties);
  return (
    <AriaToast
      {...props}
      style={composeRenderProps(style, (given: CSSProperties | undefined) => ({ ...fade, ...given }))}
      className={composeRenderProps(className, (given) => `${TOAST} ${timeout === undefined ? "" : FADE} ${given ?? ""}`)}
    />
  );
}

const REGION =
  "bc:group/toasts bc:pointer-events-none bc:fixed bc:right-2 bc:bottom-2 bc:z-[calc(var(--bc-z-index-overlay)+1)] bc:m-0 bc:box-border bc:flex bc:max-h-[calc(100dvh-1rem)] bc:w-[21rem] bc:max-w-[calc(100vw-1rem)] bc:flex-col-reverse bc:gap-2 bc:overflow-y-auto bc:p-2 bc:font-sans bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-border-accent";
const TOAST =
  "bc:pointer-events-auto bc:box-border bc:flex bc:items-start bc:gap-2 bc:rounded-md bc:border bc:border-border-default bc:bg-bg-raised bc:px-3 bc:py-2 bc:text-sm bc:text-gray-900 bc:shadow-lg bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:dark:text-gray-100";
// The fade of a toast with a timeout, in its last 300 milliseconds, which
// pauses as React Aria pauses the timeout: while the pointer rests on the
// region or the focus is in it (../runtime/index.css).
const FADE =
  "bc:toast-fade bc:group-data-hovered/toasts:[animation-play-state:paused] bc:group-focus-within/toasts:[animation-play-state:paused]";
