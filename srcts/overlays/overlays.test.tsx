// The overlays of core's React SDK, React Aria's in core's theme: what core
// adds to them, where they stack, the tooltip and its trigger, the dialog that
// asks, where the popover leaves the focus, the sheet, the submenu and the
// toasts.
// The rest, such as the keys that move through a menu, is React Aria's.

import { act } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { afterEach, describe, expect, test, vi } from "vitest";
import { Button, ComboBox, ComboBoxItem, SearchField } from "../inputs/index.ts";
import { find, press } from "../testing/inputs.ts";
import { menuItem, menuItems, openDialog, openMenu, pointTo, shownTip, tipOnFocus } from "../testing/overlays.ts";
import { step } from "../testing/react.ts";
import { labelled } from "../testing/select.ts";
import {
  AlertDialog,
  Dialog,
  DialogTrigger,
  Focusable,
  Menu,
  MenuItem,
  MenuTrigger,
  Modal,
  Popover,
  Sheet,
  SubmenuTrigger,
  Toast,
  ToastContent,
  ToastQueue,
  ToastRegion,
  Tooltip,
  TooltipTrigger,
} from "./index.ts";

// What unmounts each component that a test drew, which runs as the test ends.
const drawn: (() => void)[] = [];

afterEach(() => {
  act(() => {
    for (const unmount of drawn.splice(0)) unmount();
  });
  vi.useRealTimers();
});

// Draws the component on the page, until the test ends.
async function draw(node: ReactElement): Promise<HTMLElement> {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  drawn.push(() => {
    root.unmount();
  });
  await step(() => {
    root.render(node);
  });
  return el;
}

// Clicks the element, as a user does.
async function click(el: HTMLElement): Promise<void> {
  await step(() => {
    el.click();
  });
}

// Presses the element with the mouse's main button and lets it go, as a
// browser fires the events of the pointer and then the click.
// React Aria takes click() for a virtual press and moves the focus later.
async function pointerPress(el: HTMLElement): Promise<void> {
  const pointer = { bubbles: true, cancelable: true, composed: true, pointerId: 1, pointerType: "mouse", isPrimary: true, button: 0 };
  await step(() => {
    el.dispatchEvent(new PointerEvent("pointerdown", { ...pointer, buttons: 1 }));
    el.dispatchEvent(new PointerEvent("pointerup", { ...pointer, buttons: 0 }));
    el.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, composed: true, button: 0, detail: 1 }));
  });
}

const button = (el: ParentNode, text: string): HTMLButtonElement => {
  const found = [...el.querySelectorAll("button")].find((b) => b.textContent.trim() === text);
  if (found === undefined) throw new Error(`Found no button ${text}.`);
  return found;
};

describe("the overlays", () => {
  test("stack at the z-index of the overlays of core's theme, in place of React Aria's, and the toasts one above", async () => {
    const queue = new ToastQueue<string>();
    const el = await draw(
      <>
        <MenuTrigger>
          <Button>Actions</Button>
          <Menu aria-label="Actions">
            <MenuItem id="rename">Rename</MenuItem>
          </Menu>
        </MenuTrigger>
        <TooltipTrigger>
          <Button>Save</Button>
          <Tooltip>Save to Connect</Tooltip>
        </TooltipTrigger>
        <ComboBox label="Fruit" defaultItems={[{ id: "apple" }]}>
          {(item) => <ComboBoxItem id={item.id}>{item.id}</ComboBoxItem>}
        </ComboBox>
        <DialogTrigger>
          <Button>Open</Button>
          <Modal>
            <AlertDialog title="Remove the block?" actionLabel="Remove">
              It goes with its links.
            </AlertDialog>
          </Modal>
        </DialogTrigger>
        <ToastRegion queue={queue}>
          {({ toast }) => (
            <Toast toast={toast}>
              <ToastContent>{toast.content}</ToastContent>
            </Toast>
          )}
        </ToastRegion>
      </>,
    );

    await click(button(el, "Actions"));
    expect(openMenu("Actions").closest<HTMLElement>("[data-trigger]")?.style.zIndex).toBe("var(--bc-z-index-overlay)");
    await step(() => {
      press(openMenu("Actions"), "Escape");
    });
    await tipOnFocus(button(el, "Save"));
    expect(find(document, '[role="tooltip"]').style.zIndex).toBe("var(--bc-z-index-overlay)");
    await step(() => {
      labelled(el, "Fruit").click();
    });
    expect(find(document, '[role="listbox"]').closest<HTMLElement>("[data-trigger]")?.style.zIndex).toBe("var(--bc-z-index-overlay)");
    await step(() => {
      press(labelled(el, "Fruit"), "Escape");
    });
    await click(button(el, "Open"));
    expect(find(document, '[role="alertdialog"]').closest(".bc\\:z-overlay")).not.toBeNull();
    await step(() => {
      queue.add("Saved.");
    });
    expect(find(document, '[role="region"]').className).toContain("bc:z-[calc(var(--bc-z-index-overlay)+1)]");
  });
});

describe("the tooltip", () => {
  test("shows as the keyboard's focus moves to its trigger, names it there, and hides on Escape and as the focus leaves", async () => {
    const el = await draw(
      <TooltipTrigger>
        <Button>Save</Button>
        <Tooltip>Save to Connect</Tooltip>
      </TooltipTrigger>,
    );
    const save = button(el, "Save");

    expect(await tipOnFocus(save)).toBe("Save to Connect");
    expect(document.getElementById(save.getAttribute("aria-describedby") ?? "")?.textContent).toBe("Save to Connect");
    await step(() => {
      press(save, "Escape");
    });
    expect(shownTip()).toBeNull();

    await step(() => {
      save.blur();
    });
    expect(await tipOnFocus(save)).toBe("Save to Connect");
    await step(() => {
      save.blur();
    });
    expect(shownTip()).toBeNull();
  });

  test("shows once the pointer rests on its trigger, and never for a disabled one", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    const el = await draw(
      <>
        <TooltipTrigger>
          <Button>Save</Button>
          <Tooltip>Save to Connect</Tooltip>
        </TooltipTrigger>
        <TooltipTrigger>
          <Button isDisabled>Share</Button>
          <Tooltip>Nothing to share</Tooltip>
        </TooltipTrigger>
      </>,
    );

    await step(() => {
      pointTo(button(el, "Share"));
      vi.advanceTimersByTime(5000);
    });
    expect(shownTip()).toBeNull();
    expect(await tipOnFocus(button(el, "Share"))).toBeNull();

    // React Aria shows it after 1.5 seconds, or at once where another tooltip
    // has just shown, as in the test before.
    await step(() => {
      pointTo(button(el, "Save"));
      vi.advanceTimersByTime(1500);
    });
    expect(shownTip()).toBe("Save to Connect");
  });

  test("takes an element of a UI's own for its trigger through Focusable, which keeps the element's place in the order of the focus", async () => {
    const el = await draw(
      <TooltipTrigger>
        <Focusable>
          <button type="button" tabIndex={-1}>
            Clear
          </button>
        </Focusable>
        <Tooltip>Clear the pick</Tooltip>
      </TooltipTrigger>,
    );
    const clear = button(el, "Clear");

    expect(clear.tabIndex).toBe(-1);
    expect(await tipOnFocus(clear)).toBe("Clear the pick");
  });
});

describe("the dialog that asks", () => {
  test("names itself by its title, gives the button that confirms the focus, which runs onAction and closes it, and closes on Cancel alone", async () => {
    const onAction = vi.fn();
    const el = await draw(
      <DialogTrigger>
        <Button>Remove</Button>
        <Modal>
          <AlertDialog title="Remove the block?" variant="destructive" actionLabel="Remove it" onAction={onAction}>
            It goes with its links.
          </AlertDialog>
        </Modal>
      </DialogTrigger>,
    );

    await click(button(el, "Remove"));
    const asked = find(document, '[role="alertdialog"]');
    expect(document.getElementById(asked.getAttribute("aria-labelledby") ?? "")?.textContent).toBe("Remove the block?");
    expect(asked.textContent).toContain("It goes with its links.");
    expect(document.activeElement).toBe(button(asked, "Remove it"));
    // The button that confirms what cannot be undone is in the danger colour.
    expect(button(asked, "Remove it").className).toContain("bc:bg-red-700");
    await click(button(asked, "Cancel"));
    expect(document.querySelector('[role="alertdialog"]')).toBeNull();
    expect(onAction).not.toHaveBeenCalled();

    await click(button(el, "Remove"));
    await click(button(find(document, '[role="alertdialog"]'), "Remove it"));
    expect(onAction).toHaveBeenCalledTimes(1);
    expect(document.querySelector('[role="alertdialog"]')).toBeNull();
  });
});

describe("the popover", () => {
  test("leaves the focus with its search field, which takes the focus as a pointer press opens the popover", async () => {
    const el = await draw(
      <DialogTrigger>
        <Button>Pick</Button>
        <Popover>
          <Dialog aria-label="Pick">
            <SearchField autoFocus aria-label="Search" />
          </Dialog>
        </Popover>
      </DialogTrigger>,
    );

    await pointerPress(button(el, "Pick"));
    // In happy-dom, the dialog takes the focus from the search field unless
    // ../testing/dom.ts makes the page's document an instance of the
    // window's Document.
    expect(document.activeElement).toBe(find(openDialog(), 'input[aria-label="Search"]', HTMLInputElement));
  });
});

describe("the sheet", () => {
  test("opens from a DialogTrigger as a modal dialog, at the overlays' z-index, and closes on Escape", async () => {
    const el = await draw(
      <DialogTrigger>
        <Button>Filters</Button>
        <Sheet position="right">
          <p>Filter the rows.</p>
        </Sheet>
      </DialogTrigger>,
    );

    const filters = button(el, "Filters");
    await click(filters);
    const sheet = openDialog();
    expect(sheet.textContent).toBe("Filter the rows.");
    expect(sheet.closest(".bc\\:z-overlay")).not.toBeNull();
    expect(filters.getAttribute("aria-expanded")).toBe("true");
    // React Aria leaves the sheet on the page until the browser has scrolled
    // it out, which happy-dom does not.
    await step(() => {
      press(sheet, "Escape");
    });
    expect(filters.getAttribute("aria-expanded")).toBe("false");
  });
});

describe("the submenu", () => {
  test("is an item with a chevron, which opens its menu beside its row, whose item runs and closes every menu", async () => {
    const onAction = vi.fn();
    const el = await draw(
      <MenuTrigger>
        <Button>Versions</Button>
        <Menu aria-label="Versions">
          <MenuItem id="restore">Restore</MenuItem>
          <SubmenuTrigger>
            <MenuItem id="older">Older</MenuItem>
            <Menu aria-label="Older">
              <MenuItem id="v1" onAction={onAction}>
                Version 1
              </MenuItem>
            </Menu>
          </SubmenuTrigger>
        </Menu>
      </MenuTrigger>,
    );

    await click(button(el, "Versions"));
    const older = menuItem(openMenu("Versions"), "Older");
    expect(older.getAttribute("aria-haspopup")).toBe("menu");
    expect(older.querySelectorAll("svg")).toHaveLength(1);
    expect(menuItem(openMenu("Versions"), "Restore").querySelectorAll("svg")).toHaveLength(0);
    await click(older);
    expect(menuItems(openMenu("Older"))).toEqual(["Version 1"]);

    await click(menuItem(openMenu("Older"), "Version 1"));
    expect(onAction).toHaveBeenCalledTimes(1);
    expect(document.querySelector('[role="menu"]')).toBeNull();
  });
});

describe("the toasts", () => {
  // A region of the toasts of the queue, each with its text and a button that
  // closes it.
  function region(queue: ToastQueue<string>): ReactElement {
    return (
      <ToastRegion queue={queue}>
        {({ toast }) => (
          <Toast toast={toast}>
            <ToastContent role={toast.content.startsWith("Saved") ? "status" : undefined}>{toast.content}</ToastContent>
            <Button slot="close">Close</Button>
          </Toast>
        )}
      </ToastRegion>
    );
  }

  const shown = (): string[] =>
    [...document.querySelectorAll('[role="region"] [role="alertdialog"]')].map((toast) => toast.firstElementChild?.textContent ?? "");

  test("show the latest first, which the region lays out lowest, and close by their button, which runs their onClose", async () => {
    const queue = new ToastQueue<string>();
    const onClose = vi.fn();
    await draw(region(queue));
    expect(document.querySelector('[role="region"]')).toBeNull();

    await step(() => {
      queue.add("Saved.", { onClose });
      queue.add("Not shared.");
    });
    expect(shown()).toEqual(["Not shared.", "Saved."]);
    expect(find(document, '[role="region"]').className).toContain("bc:flex-col-reverse");
    await click(button(find(document, '[role="region"]'), "Close"));
    expect(shown()).toEqual(["Saved."]);
    expect(onClose).not.toHaveBeenCalled();
    await click(button(find(document, '[role="region"]'), "Close"));
    expect(shown()).toEqual([]);
    expect(onClose).toHaveBeenCalledTimes(1);
  });

  test("say what they hold as an alert, or as what the content's role says", async () => {
    const queue = new ToastQueue<string>();
    await draw(region(queue));

    await step(() => {
      queue.add("Saved.");
      queue.add("Not shared.");
    });
    expect([...document.querySelectorAll('[aria-atomic="true"]')].map((content) => [content.textContent, content.getAttribute("role")])).toEqual([
      ["Not shared.", "alert"],
      ["Saved.", "status"],
    ]);
  });

  test("close once their timeout is up, fading at its end, which every toast waits for while the pointer rests on the region", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    const queue = new ToastQueue<string>();
    await draw(region(queue));
    await step(() => {
      queue.add("Saved.", { timeout: 5000 });
      queue.add("Not shared.");
    });
    const toasts = [...document.querySelectorAll<HTMLElement>('[role="region"] [role="alertdialog"]')];
    expect(toasts.map((toast) => [toast.className.includes("bc:toast-fade"), toast.style.getPropertyValue("--bc-toast-fade-at")])).toEqual([
      [false, ""],
      [true, "4700ms"],
    ]);

    await step(() => {
      toasts[0]?.dispatchEvent(new PointerEvent("pointerover", { bubbles: true, pointerType: "mouse" }));
      vi.advanceTimersByTime(60_000);
    });
    expect(shown()).toEqual(["Not shared.", "Saved."]);
    await step(() => {
      toasts[0]?.dispatchEvent(new PointerEvent("pointerout", { bubbles: true, pointerType: "mouse" }));
      vi.advanceTimersByTime(5000);
    });
    expect(shown()).toEqual(["Not shared."]);
  });
});
