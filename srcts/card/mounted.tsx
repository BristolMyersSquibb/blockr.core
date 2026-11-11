// The element in which core mounts a module, for a block's UI and its output
// on both cards, and for the board options and the plugins that core's basic
// frontend shows.

import { useLayoutEffect, useRef } from "react";
import type { ReactElement } from "react";

// The element in which core mounts a module, as `mount` does it, until the
// component goes.
// Where it has a label, the element is a region of that name.
// Core mounts the module as React commits the element.
// So a card that React draws within flushSync() has mounted its block once
// flushSync() returns, as the card of a block that serve() serves alone needs.
// Core's cleanup runs once React has finished the work in which the component
// went.
// A module written in React then unmounts its own root at once, before core
// empties the element.
// Within that work, React would put the unmount off until after core had
// emptied the element, and then fail to remove what it drew.
export function Mounted({
  mount,
  label,
  className,
  data,
}: {
  mount: (el: HTMLElement) => () => void;
  label?: string;
  className?: string;
  data?: Readonly<Record<`data-${string}`, string>>;
}): ReactElement {
  const ref = useRef<HTMLDivElement>(null);
  useLayoutEffect(() => {
    const el = ref.current;
    if (el === null) return;
    const cleanup = mount(el);
    return () => {
      queueMicrotask(cleanup);
    };
  }, [mount]);
  return (
    <div ref={ref} role={label === undefined ? undefined : "region"} aria-label={label} className={className} {...data} />
  );
}
