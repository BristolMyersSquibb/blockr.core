// The element in which core mounts a module, for a block's UI and its output
// on both cards, for the board options and the plugins that core's basic
// frontend shows, and for the advanced actions that core's dialog and the Edit
// board plugin show.

import { useLayoutEffect, useRef } from "react";
import type { ReactElement } from "react";

// The element in which core mounts a module, as `mount` does it, until the
// component goes.
// Where it has a label, the element is a region of that name.
// Core mounts the module as React commits the element.
// So a card that React draws within flushSync() has mounted its block once
// flushSync() returns, as the card of a block that serve() serves alone needs.
// Where `deferred` is set, core mounts the module only once React has finished
// the work in which it committed the element, and not at all where the
// component has gone by then.
// Core's dialog and the Edit board plugin set it for an advanced action, which
// core renders within `mount`.
// A form of core's draws with flushSync() as it renders (../ui/ui.tsx).
// Within React's work, its effects included, flushSync() flushes nothing.
// React would then log an error, and draw the form only once drawForm() had
// returned.
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
  deferred = false,
}: {
  mount: (el: HTMLElement) => () => void;
  label?: string;
  className?: string;
  data?: Readonly<Record<`data-${string}`, string>>;
  deferred?: boolean;
}): ReactElement {
  const ref = useRef<HTMLDivElement>(null);
  useLayoutEffect(() => {
    const el = ref.current;
    if (el === null) return;
    let cleanup: (() => void) | undefined;
    let gone = false;
    const start = (): void => {
      if (!gone) cleanup = mount(el);
    };
    if (deferred) queueMicrotask(start);
    else start();
    return () => {
      gone = true;
      queueMicrotask(() => {
        cleanup?.();
      });
    };
  }, [mount, deferred]);
  return (
    <div ref={ref} role={label === undefined ? undefined : "region"} aria-label={label} className={className} {...data} />
  );
}
