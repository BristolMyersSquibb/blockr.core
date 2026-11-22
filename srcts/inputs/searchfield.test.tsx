// The search field of core's React SDK, React Aria's SearchField in core's
// theme: its button that clears the search, Escape, and the binding of a
// block's state arg, on the stub of a block's host, which submits as the
// SDK's TextField does.
// The rest, such as the focus, is React Aria's.

import { act } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { PAUSE } from "../binding/index.ts";
import { useValue } from "../react/index.ts";
import { stubBlockHost } from "../stubs/index.ts";
import type { StubBlockHost } from "../stubs/index.ts";
import { press, type } from "../testing/inputs.ts";
import { step } from "../testing/react.ts";
import { labelled } from "../testing/select.ts";
import type { BlockHost } from "../types/hosts.ts";
import { SearchField } from "./index.ts";

type Query = Readonly<{ query: string }>;

// A host whose fixture lists the state given and those that a test submits.
function host(state: Query, ...submits: Query[]): StubBlockHost<Query> {
  return stubBlockHost<Query>({ states: [state, ...submits], requests: [] }, { state });
}

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
});

// Draws the component on the page, and unmounts it.
async function draw(node: ReactElement): Promise<{ el: HTMLElement; unmount: () => Promise<void> }> {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  await step(() => {
    root.render(node);
  });
  return {
    el,
    unmount: () =>
      step(() => {
        root.unmount();
      }),
  };
}

async function wait(ms: number): Promise<void> {
  await act(() => vi.advanceTimersByTimeAsync(ms));
}

function Bound({ host }: { host: BlockHost<Query> }): ReactElement {
  return <SearchField bound={useValue(host, "query")} label="Search" />;
}

// The button of the field that clears the search.
function clearer(field: HTMLInputElement): HTMLButtonElement {
  const found = field.closest('[role="group"]')?.querySelector('button[aria-label="Clear search"]');
  if (!(found instanceof HTMLButtonElement)) throw new Error("Found no button that clears the search.");
  return found;
}

describe("a search field", () => {
  test("clears the search by its button, which Tab skips, and by Escape, which goes on to the elements around it where the search is empty", async () => {
    const heard: string[] = [];
    const { el } = await draw(
      <div
        onKeyDown={(event) => {
          heard.push(event.key);
        }}
      >
        <SearchField label="Search" defaultValue="iris" />
      </div>,
    );
    const field = labelled(el, "Search");
    expect([field.value, field.type, clearer(field).tabIndex]).toEqual(["iris", "search", -1]);

    await step(() => {
      clearer(field).click();
    });
    expect(field.value).toBe("");

    await step(() => {
      type(field, "head");
    });
    await step(() => {
      press(field, "Escape");
    });
    expect([field.value, heard]).toEqual(["", []]);
    await step(() => {
      press(field, "Escape");
    });
    expect(heard).toEqual(["Escape"]);
  });
});

describe("a bound search field", () => {
  test("submits what the user types once the user pauses, as one edit", async () => {
    const stub = host({ query: "" }, { query: "iris" });
    const { el } = await draw(<Bound host={stub} />);
    const field = labelled(el, "Search");

    await step(() => {
      type(field, "iris");
    });
    await wait(PAUSE - 1);
    expect(stub.submitted).toEqual([]);
    await wait(1);

    expect(stub.submitted).toEqual([{ query: "iris" }]);
  });

  test("submits at once on Enter, as the user leaves the field, and as the user clears the search", async () => {
    const stub = host({ query: "" }, { query: "a" }, { query: "ab" }, { query: "" });
    const { el } = await draw(<Bound host={stub} />);
    const field = labelled(el, "Search");

    await step(() => {
      type(field, "a");
      press(field, "Enter");
    });
    await step(() => {
      type(field, "ab");
      field.blur();
    });
    await step(() => {
      clearer(field).click();
    });

    expect(stub.submitted).toEqual([{ query: "a" }, { query: "ab" }, { query: "" }]);
  });

  test("shows a state it did not make, but not while the user types", async () => {
    const stub = host({ query: "" }, { query: "iris" });
    const { el } = await draw(<Bound host={stub} />);
    const field = labelled(el, "Search");

    await step(() => {
      stub.setState({ query: "iris" });
    });
    expect(field.value).toBe("iris");

    await step(() => {
      type(field, "head");
    });
    await step(() => {
      stub.setState({ query: "" });
    });
    expect(field.value).toBe("head");
  });
});
