// The popover of core's React SDK, which React Aria's DialogTrigger opens from
// a button.
// One of its elements, such as a search field with autoFocus, can take the
// focus as the popover opens.
// The popover leaves the focus there, as in a browser, also where a pointer
// press opens it (../testing/dom.ts).

import { afterEach, describe, expect, test } from "vitest";
import { Button, SearchField } from "../inputs/index.ts";
import { find } from "../testing/inputs.ts";
import { openDialog } from "../testing/overlays.ts";
import { draw, step } from "../testing/react.ts";
import { Dialog, DialogTrigger, Popover } from "./index.ts";

afterEach(() => {
  document.body.replaceChildren();
});

// Presses the element with the mouse's main button and lets it go, as a
// browser fires the events of the pointer and then the click.
function pointerPress(el: HTMLElement): void {
  const pointer = { bubbles: true, cancelable: true, composed: true, pointerId: 1, pointerType: "mouse", isPrimary: true, button: 0 };
  el.dispatchEvent(new PointerEvent("pointerdown", { ...pointer, buttons: 1 }));
  el.dispatchEvent(new PointerEvent("pointerup", { ...pointer, buttons: 0 }));
  el.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, composed: true, button: 0, detail: 1 }));
}

describe("the popover", () => {
  test("leaves the focus with its search field, which takes the focus as a pointer press opens the popover", async () => {
    const { el, unmount } = draw(
      <DialogTrigger>
        <Button>Pick</Button>
        <Popover>
          <Dialog aria-label="Pick">
            <SearchField autoFocus aria-label="Search" />
          </Dialog>
        </Popover>
      </DialogTrigger>,
    );

    await step(() => {
      pointerPress(find(el, "button"));
    });

    expect(document.activeElement).toBe(find(openDialog(), 'input[aria-label="Search"]', HTMLInputElement));
    unmount();
  });
});
