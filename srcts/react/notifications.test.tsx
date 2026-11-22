// The list in which a UI shows notifications, drawn with React on
// notifications that the test gives, as useNotified() would give them.

import { createRoot } from "react-dom/client";
import type { Root } from "react-dom/client";
import { beforeEach, describe, expect, test } from "vitest";
import { step } from "../testing/react.ts";
import type { Notification, Severity } from "../types/core-api.ts";
import { NotificationList } from "./index.ts";

// A condition that R raised as it evaluated the block `b`.
function raised(id: string, severity: Severity, message: string): Notification {
  return {
    id,
    severity,
    message,
    source: { kind: "r", class: [severity, "condition"], request: { type: "evaluate", phase: "block_eval" } },
    about: { block: "b" },
  };
}

let el: HTMLElement;
let root: Root;

beforeEach(() => {
  el = document.createElement("div");
  document.body.append(el);
  root = createRoot(el);
});

function items(): HTMLLIElement[] {
  return [...el.querySelectorAll("li")];
}

describe("NotificationList", () => {
  test("draws an item for each notification, in the order given, with its severity in data-severity and its message as text", async () => {
    await step(() => {
      root.render(
        <NotificationList
          notifications={[
            raised("w", "warning", "Dropped <b>37</b> rows"),
            raised("e", "error", "Column not found:\n  Ozone2"),
            raised("m", "message", "Joined by id"),
          ]}
        />,
      );
    });

    expect(items().map((item) => item.dataset.severity)).toEqual(["warning", "error", "message"]);
    expect(items().map((item) => item.textContent)).toEqual(["Dropped <b>37</b> rows", "Column not found:\n  Ozone2", "Joined by id"]);
    // The message is text, never HTML.
    expect(el.querySelector("b")).toBeNull();
  });

  test("draws an error and a warning in their sets of core's theme, and a message in its neutral colours", async () => {
    await step(() => {
      root.render(
        <NotificationList
          notifications={[raised("e", "error", "failed"), raised("w", "warning", "dropped"), raised("m", "message", "joined")]}
        />,
      );
    });

    const [error, warning, message] = items().map((item) => item.className.split(" "));
    expect(error).toEqual(expect.arrayContaining(["bc:text-text-danger", "bc:bg-bg-danger", "bc:border-border-danger-subtle"]));
    expect(warning).toEqual(expect.arrayContaining(["bc:text-text-warning", "bc:bg-bg-warning", "bc:border-border-warning-subtle"]));
    expect(message).toEqual(expect.arrayContaining(["bc:text-text-default", "bc:bg-bg-subtle", "bc:border-border-default"]));
  });

  test("takes its accessible name from its caller", async () => {
    await step(() => {
      root.render(<NotificationList notifications={[raised("e", "error", "failed")]} aria-label="Errors" />);
    });
    expect(el.querySelector("ul")?.getAttribute("aria-label")).toBe("Errors");

    await step(() => {
      root.render(<NotificationList notifications={[raised("e", "error", "failed")]} />);
    });
    expect(el.querySelector("ul")?.hasAttribute("aria-label")).toBe(false);
  });

  test("keeps the item of each notification that holds, as others come and go", async () => {
    const [a, b, c] = [raised("a", "error", "a"), raised("b", "warning", "b"), raised("c", "message", "c")];
    await step(() => {
      root.render(<NotificationList notifications={[a, b]} />);
    });
    const [first, second] = items();

    await step(() => {
      root.render(<NotificationList notifications={[b, c]} />);
    });

    expect(items().map((item) => item.textContent)).toEqual(["b", "c"]);
    expect(items()[0]).toBe(second);
    expect(first?.isConnected).toBe(false);
  });

  test("draws nothing where no notification is given", async () => {
    await step(() => {
      root.render(<NotificationList notifications={[]} aria-label="Errors" />);
    });

    expect(el.childNodes).toHaveLength(0);
  });
});
