// What rises above the page, such as a menu, a modal or a slider's thumb,
// takes its background from the colour behind the element that it rises
// from, which core measures as it mounts and again as the page switches
// theme.

import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { afterEach, describe, expect, test } from "vitest";
import { Button, ComboBox, ComboBoxItem, Select, SelectItem } from "../inputs/index.ts";
import { find } from "../testing/inputs.ts";
import { openDialog, openMenu } from "../testing/overlays.ts";
import { step } from "../testing/react.ts";
import { Dialog, Menu, MenuItem, MenuTrigger, Modal } from "./index.ts";
import { useRaised } from "./raised.ts";

afterEach(() => {
  Reflect.deleteProperty(document, "elementsFromPoint");
  document.body.replaceChildren();
  document.body.removeAttribute("style");
  document.documentElement.removeAttribute("data-bc-theme");
});

async function draw(node: ReactElement): Promise<HTMLElement> {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  await step(() => {
    root.render(node);
  });
  return el;
}

// The colour that core set on an element as the colour behind what it rises
// from.
const behind = (el: Element | null): string => (el instanceof HTMLElement ? el.style.getPropertyValue("--bc-color-behind") : "");

const FRUITS = [
  { id: "apple", name: "Apple" },
  { id: "pear", name: "Pear" },
];

function Raised(): ReactElement {
  return <span ref={useRaised("parent")}>Raised</span>;
}

describe("what rises above the page", () => {
  test("takes the colour behind its parent, and measures it again as the page switches theme", async () => {
    const el = await draw(
      <div data-surface="" style={{ backgroundColor: "rgb(22, 27, 35)" }}>
        <Raised />
      </div>,
    );
    const raised = find(el, "span");
    expect(behind(raised)).toBe("#161b23");

    find(el, "[data-surface]").style.backgroundColor = "rgb(255, 255, 255)";
    await step(() => {
      document.documentElement.setAttribute("data-bc-theme", "light");
    });
    expect(behind(raised)).toBe("#ffffff");
  });

  test("as a menu, takes the colour behind the trigger that opened it", async () => {
    const el = await draw(
      <div style={{ backgroundColor: "rgb(29, 29, 30)" }}>
        <MenuTrigger>
          <Button aria-label="Actions">…</Button>
          <Menu aria-label="Actions of the block">
            <MenuItem id="rename">Rename</MenuItem>
          </Menu>
        </MenuTrigger>
      </div>,
    );
    await step(() => {
      find(el, 'button[aria-label="Actions"]').click();
    });

    const popover = openMenu("Actions of the block").closest("[data-trigger]");
    expect(behind(popover)).toBe("#1d1d1e");
    expect(popover?.hasAttribute("data-bc-overlay")).toBe(true);
  });

  test.each([
    {
      input: "a select",
      node: (
        <Select label="Fruit" items={FRUITS}>
          {(fruit) => <SelectItem id={fruit.id}>{fruit.name}</SelectItem>}
        </Select>
      ),
    },
    {
      input: "a combo box",
      node: (
        <ComboBox label="Fruit" defaultItems={FRUITS}>
          {(fruit) => <ComboBoxItem id={fruit.id}>{fruit.name}</ComboBoxItem>}
        </ComboBox>
      ),
    },
  ])("as the list of $input, takes the colour behind what opened it", async ({ node }) => {
    const el = await draw(<div style={{ backgroundColor: "rgb(22, 27, 35)" }}>{node}</div>);

    await step(() => {
      find(el, 'button[aria-haspopup="listbox"]').click();
    });

    const list = find(document, '[role="listbox"]');
    expect(behind(list.closest("[data-trigger]"))).toBe("#161b23");
  });

  test("as a modal, takes the colour of the page in the middle of the window, below every overlay", async () => {
    const page = document.createElement("main");
    page.style.backgroundColor = "rgb(14, 18, 25)";
    document.body.append(page);
    // The page in the middle of the window lies below the modal's backdrop and
    // its box, which the browser lists first. The test runner lists nothing,
    // so the test gives the page what a browser would list.
    document.elementsFromPoint = () => [...document.querySelectorAll("[data-bc-overlay], [data-bc-overlay] *"), page];

    await draw(
      <Modal isOpen>
        <Dialog aria-label="Options">Options</Dialog>
      </Modal>,
    );

    expect(behind(openDialog().parentElement)).toBe("#0e1219");

    // As the theme switches, the modal measures again the element that it found
    // as it opened, which the browser no longer lists below the inert page.
    document.elementsFromPoint = () => [...document.querySelectorAll("[data-bc-overlay], [data-bc-overlay] *")];
    page.style.backgroundColor = "rgb(249, 250, 251)";
    await step(() => {
      document.documentElement.setAttribute("data-bc-theme", "light");
    });
    expect(behind(openDialog().parentElement)).toBe("#f9fafb");
  });
});
