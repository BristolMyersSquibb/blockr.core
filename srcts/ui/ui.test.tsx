// The pieces that core's basic frontend and its Edit board plugin draw with:
// the search and the pages of a table.

import { act } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { afterEach, describe, expect, test } from "vitest";
import { find, texts, type } from "../testing/inputs.ts";
import { step } from "../testing/react.ts";
import { Pager, TableSearch, useTableView } from "./index.ts";

const unmounts: (() => void)[] = [];

afterEach(() => {
  for (const unmount of unmounts.splice(0)) unmount();
});

// Draws the element in an element on the page, as React does on the page.
function drawn(ui: ReactElement): HTMLElement {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  act(() => {
    root.render(ui);
  });
  unmounts.push(() => {
    act(() => {
      root.unmount();
    });
  });
  return el;
}

function Rows({ rows }: { rows: readonly string[] }): ReactElement {
  const view = useTableView(rows, (row) => row, 3);
  return (
    <div>
      <TableSearch label="Search rows" view={view} />
      <ul aria-label="Rows">
        {view.rows.map((row) => (
          <li key={row}>{row}</li>
        ))}
      </ul>
      <Pager label="Pages of rows" view={view} />
    </div>
  );
}

describe("a table's search and pages", () => {
  const ROWS = ["alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta"];

  test("show a page of rows at a time, which the pager moves between", async () => {
    const el = drawn(<Rows rows={ROWS} />);
    const shown = (): string[] => texts(el, 'ul[aria-label="Rows"] li');
    const pager = (): HTMLElement => find(el, 'nav[aria-label="Pages of rows"]');
    const button = (text: string): HTMLButtonElement => find(pager(), `button:${text === "Previous" ? "first-of-type" : "last-of-type"}`, HTMLButtonElement);

    expect(shown()).toEqual(["alpha", "beta", "gamma"]);
    expect(pager().querySelector("span")?.textContent).toBe("Rows 1–3 of 7");
    expect(button("Previous").disabled).toBe(true);

    await step(() => {
      button("Next").click();
    });
    await step(() => {
      button("Next").click();
    });
    expect(shown()).toEqual(["eta"]);
    expect(pager().querySelector("span")?.textContent).toBe("Rows 7–7 of 7");
    expect(button("Next").disabled).toBe(true);
  });

  test("narrow the rows by the search, from the first page, and draw no pager while one page holds them all", async () => {
    const el = drawn(<Rows rows={ROWS} />);
    await step(() => {
      find(el, 'nav[aria-label="Pages of rows"] button:last-of-type').click();
    });
    await step(() => {
      type(find(el, "search input", HTMLInputElement), "ETA");
    });
    expect(texts(el, 'ul[aria-label="Rows"] li')).toEqual(["beta", "zeta", "eta"]);
    expect(el.querySelector("nav")).toBeNull();
  });

  test("show the last page where the rows of the page shown are gone", async () => {
    const el = document.createElement("div");
    document.body.append(el);
    const root = createRoot(el);
    unmounts.push(() => {
      act(() => {
        root.unmount();
      });
    });
    act(() => {
      root.render(<Rows rows={ROWS} />);
    });
    for (let i = 0; i < 2; i++) {
      await step(() => {
        find(el, 'nav[aria-label="Pages of rows"] button:last-of-type').click();
      });
    }
    await step(() => {
      root.render(<Rows rows={ROWS.slice(0, 4)} />);
    });
    expect(texts(el, 'ul[aria-label="Rows"] li')).toEqual(["delta"]);
  });
});
