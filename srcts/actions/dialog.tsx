// Core's dialog, the place in which core shows an advanced action where
// neither the caller of menu() or trigger() nor the frontend shows it, as
// Board actions has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.16#the-advanced-action).
// It is a modal dialog of blockr.ui, React Aria's Modal and Dialog drawn with
// blockr.ui's theme variables, titled with the board action's label, in
// which core renders the advanced action through the mount() that core hands
// it, as a frontend's place does. It is at least 420 px wide, as the dock's
// sidebar is by default, or as wide as the window where the window is
// narrower.
// It closes in these cases:
// - The user presses Escape or its Close button.
// - The advanced action ends, as its done() does.
// - The next board opens.
// As it closes, it tells core through close(), and the keyboard's focus
// returns to the element that the user triggered from. A press outside it
// leaves it open, so that what the user filled in stays.
// It draws from the theme variables, which a frontend's theme sets to restyle
// it, as it restyles core's menu.
// Core's runtime loads this file as it first shows the dialog, through
// showDialog() (./advanced.ts), which keeps the dialog's element and closes
// it, so that the page loads blockr.ui's components only then.

import type { ReactElement } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { Button, Dialog, Heading, Modal } from "@blockr/ui";
import { Mounted } from "../card/index.ts";
import type { PlaceArgs } from "./advanced.ts";

// Draws core's dialog in the element, which showDialog() puts at the end of
// the page's body, and returns what unmounts it. The dialog calls `onClose`
// as the user closes it.
export function drawDialog(el: HTMLElement, args: PlaceArgs, onClose: () => void): () => void {
  const root = createRoot(el);
  flushSync(() => {
    root.render(<CoreDialog args={args} onClose={onClose} />);
  });
  return () => {
    root.unmount();
  };
}

// The modal dialog, with the board action's label as its title, a Close
// button, and the element in which core renders the advanced action. Core
// unmounts the advanced action once React has finished closing the dialog, so
// that an advanced action written in React unmounts its own root before core
// empties the element, as a card's module does.
function CoreDialog({ args, onClose }: { args: PlaceArgs; onClose: () => void }): ReactElement {
  const { mount } = args;
  return (
    <Modal
      isOpen
      onOpenChange={(isOpen) => {
        if (!isOpen) onClose();
      }}
      className={WIDTH}
    >
      <Dialog>
        <div className="bc:grid bc:gap-3">
          <div className="bc:flex bc:items-center bc:justify-between bc:gap-2">
            <Heading slot="title" className="bc:m-0 bc:font-sans bc:text-base bc:font-semibold">
              {args.title}
            </Heading>
            <Button variant="secondary" onPress={onClose}>
              Close
            </Button>
          </div>
          <Mounted mount={mount} className="bc:min-w-0" />
        </div>
      </Dialog>
    </Modal>
  );
}

// At least 420 px wide, or as wide as the box that holds the modal in the
// middle of the window, where that is narrower.
const WIDTH = "bc:min-w-[min(420px,100%)]";
