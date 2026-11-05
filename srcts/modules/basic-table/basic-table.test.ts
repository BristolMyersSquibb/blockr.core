// Core's plain renderer of table outputs, mounted with the stub of its host on
// the sample table outputs that R writes for the class.

import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, test } from "vitest";
import type { Condition } from "../../protocol/index.ts";
import { stubOutputHost } from "../../stubs/index.ts";
import { mountReact, step } from "../../testing/react.ts";
import type { StubOutputHostOptions } from "../../stubs/index.ts";
import type { Json } from "../../types/json.ts";
import type { OutputFixtures, TableOutput } from "../../types/outputs.ts";
import * as basicTable from "./index.tsx";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../../..");
const fixtures = JSON.parse(
  readFileSync(join(root, "inst", "outputs", "fixtures", "table-output.json"), "utf8"),
) as OutputFixtures<TableOutput>;

function sample(i: number) {
  const found = fixtures.outputs[i];
  if (found === undefined) throw new Error(`The fixtures hold no output ${String(i)}.`);
  return found;
}

function outputOf(i: number): Json {
  return sample(i).output as unknown as Json;
}

const error: Condition = {
  severity: "error",
  class: ["table_output_params_invalid", "blockr_error", "rlang_error", "error", "condition"],
  phase: "output",
  message: "The table has 2 pages, so it has no page 4.",
};

// The renderer, mounted on the sample output `i`, with the board option values
// and parameters it was asked with. Its host answers each change of
// parameters from the fixtures, or else, with `waiting`, records it and waits,
// as core waits for R, until the test hands it an output with setOutput().
async function mounted(i: number, options: StubOutputHostOptions & { waiting?: boolean } = {}) {
  const { params, options: values } = sample(i);
  const host = stubOutputHost({
    output: outputOf(i),
    params,
    options: values,
    fixtures: fixtures as unknown as OutputFixtures,
    ...options,
  });
  const asks: Json[] = [];
  if (options.waiting === true) {
    host.setParams = (next) => {
      asks.push(next);
    };
  }
  const mounted = await mountReact(basicTable, host);
  const { el } = mounted;

  const button = (text: string): HTMLButtonElement => {
    const found = [...el.querySelectorAll("button")].find((node) => node.textContent === text);
    if (found === undefined) throw new Error(`No button reads "${text}".`);
    return found;
  };
  const header = (name: string): HTMLTableCellElement => {
    const found = [...el.querySelectorAll("th")].find((th) => th.querySelector("span span")?.textContent === name);
    if (found === undefined) throw new Error(`No column is named "${name}".`);
    return found;
  };
  return {
    el,
    host,
    asks,
    unmount: () => {
      mounted.unmount();
    },
    button,
    header,
    texts: (selector: string) => [...el.querySelectorAll(selector)].map((node) => words(node.textContent)),
    rows: () =>
      [...el.querySelectorAll("tbody tr")].map((tr) => [...tr.querySelectorAll("td")].map((td) => td.textContent)),
    counts: () => el.querySelector("table")?.parentElement?.nextElementSibling?.firstElementChild?.textContent,
    page: () => button("Previous").nextElementSibling?.textContent,
    // Whether the pager draws its buttons, which it does where the table has
    // more than one page.
    paged: () => [...el.querySelectorAll("button")].some((node) => node.textContent === "Previous"),
    alert: () => el.querySelector<HTMLElement>('[role="alert"]'),
  };
}

// Text with its runs of white space as one space, as it reads on the page.
function words(text: string): string {
  return text.replace(/\s+/g, " ").trim();
}

test("names n_rows as the board option it requires, by which the coercion pages", () => {
  expect(basicTable.api).toBe(1);
  expect(basicTable.default.options).toEqual(["n_rows"]);
});

describe("drawing a page", () => {
  test("shows the columns with their classes, and the rows of the page in their order", async () => {
    const table = await mounted(0);

    expect(table.texts("th")).toEqual([
      "Sepal.Lengthnumeric",
      "Sepal.Widthnumeric",
      "Petal.Lengthnumeric",
      "Petal.Widthnumeric",
      "Speciesfactor",
    ]);
    expect(table.rows()).toEqual([
      ["4.6", "3.1", "1.5", "0.2", "setosa"],
      ["5", "3.6", "1.4", "0.2", "setosa"],
      ["5.4", "3.9", "1.7", "0.4", "setosa"],
    ]);
    // Numbers line up on the right, under names that do too.
    const cells = table.el.querySelectorAll("tbody tr:first-child td");
    expect(cells[0]?.className).toContain("bc:text-right");
    expect(cells[4]?.className).not.toContain("bc:text-right");
    expect(table.header("Sepal.Length").querySelector("button")?.className).toContain("bc:justify-end");
    expect(table.header("Species").querySelector("button")?.className).not.toContain("bc:justify-end");
  });

  test("shows cells of every kind as R sends them, a missing value as NA, and keeps names that repeat", async () => {
    const table = await mounted(2);

    expect(table.texts("th span span:first-child")).toEqual([
      "number",
      "count",
      "flag",
      "text",
      "level",
      "date",
      "time",
      "2019",
      "text",
      "cells",
    ]);
    expect(table.rows()).toEqual([
      ["1.5", "1", "TRUE", "a", "low", "2024-01-31", "2024-01-31 12:30:00", "1", "v", "<integer> [3]"],
      ["NA", "NA", "FALSE", "NA", "high", "NA", "2024-01-31 13:30:00", "2", "w", "one"],
      ["NA", "3", "NA", "c", "NA", "2024-02-02", "2024-01-31 14:30:00", "3", "x", "<NULL>"],
      ["NA", "4", "TRUE", "d", "low", "2024-02-03", "2024-01-31 15:30:00", "4", "y", "<data.frame> [6 × 2]"],
      ["NA", "5", "TRUE", "e", "high", "2024-02-04", "2024-01-31 16:30:00", "5", "z", "<lm>"],
    ]);
    expect(table.el.querySelector("td span")?.className).toContain("bc:italic");
    expect(table.counts()).toBe("5 rows");
    expect(table.paged()).toBe(false);
  });

  test("says that a table without rows has none, with no pager", async () => {
    const table = await mounted(3);
    expect(table.texts("th")).toHaveLength(5);
    expect(table.rows()).toEqual([]);
    expect(table.counts()).toBe("No rows");
    expect(table.paged()).toBe(false);
  });
});

describe("paging", () => {
  test("shows where the page sits, by the rows of a page that n_rows gives, and asks for the page before or after", async () => {
    const table = await mounted(0, { waiting: true });

    expect(table.counts()).toBe("Rows 4–6 of 7");
    expect(table.page()).toBe("Page 2 of 3");
    expect(table.paged()).toBe(true);

    await step(() => {
      table.button("Next").click();
    });
    await step(() => {
      table.button("Previous").click();
    });

    expect(table.asks).toEqual([{ page: 3 }, {}]);
  });

  test("keeps the sort and the rows of a page that the parameters give as it pages", async () => {
    const table = await mounted(1, { waiting: true });

    expect(table.counts()).toBe("Rows 151–153 of 153");
    expect(table.page()).toBe("Page 4 of 4");
    expect(table.button("Next").disabled).toBe(true);
    await step(() => {
      table.button("Previous").click();
    });
    expect(table.asks).toEqual([{ page: 3, sort: { column: "Ozone", desc: true } }]);

    const rows = sample(0).output.rows.slice(0, 2);
    const sized = await mounted(0, {
      output: { ...sample(0).output, rows } as unknown as Json,
      params: { page: 2, page_size: 2 },
      waiting: true,
    });
    expect(sized.counts()).toBe("Rows 3–4 of 7");
    expect(sized.page()).toBe("Page 2 of 4");
    await step(() => {
      sized.button("Next").click();
    });
    expect(sized.asks).toEqual([{ page: 3, page_size: 2 }]);
  });

  test("shows no page count where the total is unknown, and takes a page with fewer rows than a page holds for the last", async () => {
    const full = await mounted(4, { waiting: true });
    expect(full.counts()).toBe("Rows 1–3");
    expect(full.page()).toBe("Page 1");
    expect(full.button("Previous").disabled).toBe(true);
    expect(full.button("Next").disabled).toBe(false);
    await step(() => {
      full.button("Next").click();
    });
    expect(full.asks).toEqual([{ page: 2 }]);

    const rows = sample(4).output.rows.slice(0, 2);
    await step(() => {
      full.host.setOutput({ ...sample(4).output, rows } as unknown as Json, null, { page: 2 });
    });
    expect(full.counts()).toBe("Rows 4–5");
    expect(full.page()).toBe("Page 2");
    expect(full.button("Next").disabled).toBe(true);
  });
});

describe("sorting", () => {
  test("sorts by a column from the smallest value, then from the largest, then not at all, from the first page", async () => {
    const table = await mounted(0, { waiting: true });
    const species = table.header("Species");
    expect(species.getAttribute("aria-sort")).toBe("none");

    await step(() => {
      species.querySelector("button")?.click();
    });
    await step(() => {
      table.host.setOutput(outputOf(0), null, { sort: { column: "Species" } });
    });
    expect(species.getAttribute("aria-sort")).toBe("ascending");
    expect(species.textContent).toContain("↑");

    await step(() => {
      species.querySelector("button")?.click();
    });
    await step(() => {
      table.host.setOutput(outputOf(0), null, { sort: { column: "Species", desc: true } });
    });
    expect(species.getAttribute("aria-sort")).toBe("descending");
    expect(species.textContent).toContain("↓");

    await step(() => {
      species.querySelector("button")?.click();
    });

    expect(table.asks).toEqual([{ sort: { column: "Species" } }, { sort: { column: "Species", desc: true } }, {}]);
  });

  test("shows the sort that the parameters hold, and keeps each column's header while the columns stay", async () => {
    const table = await mounted(1, { waiting: true });
    const ozone = table.header("Ozone");
    expect(ozone.getAttribute("aria-sort")).toBe("descending");
    expect(table.header("Wind").getAttribute("aria-sort")).toBe("none");

    await step(() => {
      table.host.setOutput(outputOf(1), null, { page: 3, sort: { column: "Wind" } });
    });

    expect(table.header("Ozone")).toBe(ozone);
    expect(ozone.getAttribute("aria-sort")).toBe("none");
    expect(table.header("Wind").getAttribute("aria-sort")).toBe("ascending");
  });
});

describe("while core asks R for an output", () => {
  test("marks the table busy while a newer output loads", async () => {
    const table = await mounted(0);
    const busy = table.el.firstElementChild;
    expect(busy?.getAttribute("aria-busy")).toBe("false");

    await step(() => {
      table.host.setLoading(true);
    });
    expect(busy?.getAttribute("aria-busy")).toBe("true");
    expect(table.el.querySelector("table")?.classList.contains("bc:opacity-60")).toBe(true);

    await step(() => {
      table.host.setOutput(outputOf(0), null, { page: 2 });
    });
    expect(busy?.getAttribute("aria-busy")).toBe("false");
    expect(table.el.querySelector("table")?.classList.contains("bc:opacity-60")).toBe(false);
  });

  test("asks for the first page where an ask fails, then for the first page unsorted", async () => {
    // The fixtures hold no first page sorted by Ozone, so that ask fails too,
    // while they hold the first page unsorted at fifty rows to a page, the
    // sample of many kinds of cells.
    const table = await mounted(1);
    expect(table.alert()).toBeNull();

    await step(() => {
      table.host.setError(error);
    });

    expect(table.host.asked).toEqual([{ sort: { column: "Ozone", desc: true } }, {}]);
    expect(table.alert()).toBeNull();
    expect(table.rows()).toHaveLength(5);
  });

  test("asks again no further than the first page unsorted, and shows the error over the page it holds", async () => {
    // The fixtures hold no first page at five rows to a page.
    const table = await mounted(0, { options: { n_rows: 5 } });

    await step(() => {
      table.host.setError(error);
    });

    expect(table.host.asked).toEqual([{}]);
    expect(table.alert()?.textContent).toBe("The fixtures hold no output asked with the parameters {}.");
    expect(table.rows()).toHaveLength(3);

    await step(() => {
      table.host.setLoading(false);
    });
    expect(table.host.asked).toEqual([{}]);

    const first = await mounted(4);
    await step(() => {
      first.host.setError(error);
    });
    expect(first.host.asked).toEqual([]);
    expect(first.alert()?.textContent).toBe(error.message);
  });
});

test("leaves its element empty once unmounted, and draws nothing more", async () => {
  const table = await mounted(0);
  await step(() => {
    table.unmount();
  });
  await step(() => {
    table.host.setOutput(outputOf(2));
  });
  expect(table.el.childNodes).toHaveLength(0);
});
