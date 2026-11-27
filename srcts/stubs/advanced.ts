// Core's dialog under the CoreApi for tests, which shows each advanced action
// that neither the caller of menu() or trigger() nor the frontend shows, as on
// the page, and draws no modal, so that a test of a board action needs no
// browser
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#testing).
// Core renders the advanced action in an element at the end of the page's
// body, which the test reads and drives as a user would. The test closes the
// dialog as the user does.

import type { BoardActionId } from "../types/board.ts";
import type { ShowDialog, Target } from "../actions/index.ts";
import { onAbort } from "../mount/index.ts";

// An advanced action that core shows in its dialog. Each member does nothing
// once the dialog has closed.
export interface StubAdvanced {
  // The board action's id, its package and its id within the package, the
  // dialog's title and the target, as core hands a frontend's place them.
  readonly id: BoardActionId;
  readonly title: string;
  readonly target: Target;
  // The element in which core renders the advanced action, at the end of the
  // page's body until the dialog closes.
  readonly el: HTMLElement;
  // Whether the dialog has closed, as it does once done() ends the advanced
  // action.
  closed(): boolean;
  // Closes the dialog, as Escape or its Close button does.
  close(): void;
}

// What the CoreApi for tests hands core to show its dialog, which records the
// advanced action that the dialog shows in place of drawing a modal, and that
// advanced action, as a test drives it.
export function stubDialogs(): { readonly show: ShowDialog; readonly open: () => StubAdvanced | undefined } {
  let open: StubAdvanced | undefined;
  return {
    show(args, at) {
      const doc = at.el.ownerDocument;
      const el = doc.createElement("div");
      doc.body.append(el);
      let closed = false;
      // What unmounts the advanced action, once core has rendered it.
      const unmounts: (() => void)[] = [];
      const hide = (): void => {
        if (closed) return;
        closed = true;
        offAbort();
        for (const unmount of unmounts.splice(0)) unmount();
        el.remove();
        if (open === shown) open = undefined;
        args.close();
      };
      const offAbort = at.signal === undefined ? () => undefined : onAbort(at.signal, hide);
      const shown: StubAdvanced = { id: args.id, title: args.title, target: args.target, el, closed: () => closed, close: hide };
      open = shown;
      unmounts.push(args.mount(el));
      return () => {
        hide();
      };
    },
    open: () => open,
  };
}
