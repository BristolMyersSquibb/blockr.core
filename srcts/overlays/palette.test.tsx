// The command palette of core's React SDK: a menu that a search field
// narrows, in a modal, which opens while its caller says so, closes on Escape
// once the search is empty, and keeps no listener on the page.
// The rest, such as the keys of the menu and the focus, is React Aria's.

import { useState } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import type { Root } from "react-dom/client";
import { afterEach, describe, expect, test } from "vitest";
import { find, press, type } from "../testing/inputs.ts";
import { menuItem, menuItems, openDialog } from "../testing/overlays.ts";
import { step } from "../testing/react.ts";
import { CommandPalette, MenuItem } from "./index.ts";

// The root of each palette that a test drew.
// After each test, the hook below unmounts the roots before it empties the
// page's body, so that React runs the cleanups of their effects.
// React Aria's dialog clears the timer that it sets as it opens only in such a
// cleanup.
// A timer left pending can run once Vitest has torn down the page, and throw.
const roots: Root[] = [];

afterEach(async () => {
  await step(() => {
    for (const root of roots.splice(0)) root.unmount();
  });
  document.body.replaceChildren();
});

const commands = [
  { id: "undo", name: "Undo" },
  { id: "redo", name: "Redo" },
  { id: "save", name: "Save the board" },
];

// A caller that holds whether the palette is open, opens it by a button, and
// records the commands that the user runs.
function Caller({ ran }: { ran: string[] }): ReactElement {
  const [open, setOpen] = useState(false);
  return (
    <>
      <button
        type="button"
        onClick={() => {
          setOpen(true);
        }}
      >
        Commands
      </button>
      <CommandPalette
        isOpen={open}
        onOpenChange={setOpen}
        aria-label="Commands"
        items={commands}
        onAction={(key) => {
          ran.push(String(key));
        }}
      >
        {(command) => <MenuItem id={command.id}>{command.name}</MenuItem>}
      </CommandPalette>
    </>
  );
}

async function draw(node: ReactElement): Promise<HTMLElement> {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  roots.push(root);
  await step(() => {
    root.render(node);
  });
  return el;
}

describe("the command palette", () => {
  test("opens as its caller says, narrows its commands to those that hold what the user types, and runs the one the user picks", async () => {
    const ran: string[] = [];
    const el = await draw(<Caller ran={ran} />);
    expect(document.querySelector('[role="dialog"]')).toBeNull();

    await step(() => {
      find(el, "button").click();
    });
    const search = find(openDialog(), 'input[aria-label="Search commands"]', HTMLInputElement);
    expect([document.activeElement, menuItems(openDialog())]).toEqual([search, ["Undo", "Redo", "Save the board"]]);

    await step(() => {
      type(search, "do");
    });
    expect(menuItems(openDialog())).toEqual(["Undo", "Redo"]);
    await step(() => {
      menuItem(openDialog(), "Redo").click();
    });

    expect(ran).toEqual(["redo"]);
    expect(document.querySelector('[role="dialog"]')).toBeNull();
  });

  test("clears the search on Escape, and closes on Escape once the search is empty", async () => {
    const el = await draw(<Caller ran={[]} />);
    await step(() => {
      find(el, "button").click();
    });
    const search = find(openDialog(), "input", HTMLInputElement);

    await step(() => {
      type(search, "save");
    });
    await step(() => {
      press(search, "Escape");
    });
    expect([search.value, menuItems(openDialog())]).toEqual(["", ["Undo", "Redo", "Save the board"]]);

    await step(() => {
      press(search, "Escape");
    });
    expect(document.querySelector('[role="dialog"]')).toBeNull();
  });

  test("keeps no listener on the page, so that a shortcut opens it only where its caller listens for one", async () => {
    await draw(<Caller ran={[]} />);

    await step(() => {
      press(document.body, "j", { ctrlKey: true });
      press(document.body, "j", { metaKey: true });
    });

    expect(document.querySelector('[role="dialog"]')).toBeNull();
  });
});
