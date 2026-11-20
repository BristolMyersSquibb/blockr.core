// The head block's UI, mounted on the stub of its host
// with the fixture that the head block's R tests write,
// which lists every state a test submits.

import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { PAUSE } from "../../binding/index.ts";
import { READY, stubBlockHost } from "../../stubs/index.ts";
import { fieldError, numberSaid, press, toggleButton, toggles, type } from "../../testing/inputs.ts";
import { blockFixture, mountOnPage, settle, unmount, wait } from "../../testing/modules.ts";
import { step } from "../../testing/react.ts";
import { holdRequests } from "../../testing/requests.ts";
import { labelled } from "../../testing/select.ts";
import * as head from "./index.tsx";
import type { State } from "./state.ts";

const fixture = blockFixture("transform-head", "head_block");

const six: State = { n: 6, direction: "head" };
const tenHead: State = { n: 10, direction: "head" };
const tenTail: State = { n: 10, direction: "tail" };
const twentyTail: State = { n: 20, direction: "tail" };

// What the browser's own number input says of the number in the text, with
// the head block's range of 0 to the 150 rows of its input, and its step of 1,
// as the number field says it.
const said = (text: string): string | null => numberSaid(text, { min: 0, max: 150, step: 1 });

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
  document.body.replaceChildren();
});

async function mounted(state: State = six) {
  const host = stubBlockHost<State>(fixture, { state });
  const view = await mountOnPage(head, host);
  const n = labelled(view.el, "Number of rows");
  // The end that the toggle buttons of the direction show as picked.
  const direction = (): string | undefined => toggles(view.el, "Direction").find((end) => end.picked)?.label;
  return { host, view, n, direction };
}

test("shows the state, and takes the rows of its input as the most the field takes", async () => {
  const { host, n, direction } = await mounted();
  expect([n.value, direction()]).toEqual(["6", "Head"]);

  await settle();
  expect(host.requested).toEqual([{ name: "n_max", args: {} }]);
  await step(() => {
    type(n, "150");
  });
  expect(n.getAttribute("aria-invalid")).toBeNull();
  await step(() => {
    type(n, "151");
  });
  expect(n.getAttribute("aria-invalid")).toBe("true");
  expect(host.submitted).toEqual([]);
});

test("takes a state it did not make, in the elements it drew", async () => {
  const { host, view, n, direction } = await mounted();

  await step(() => {
    host.setState(tenTail);
  });

  expect(labelled(view.el, "Number of rows")).toBe(n);
  expect([n.value, direction()]).toEqual(["10", "Tail"]);
  expect(host.submitted).toEqual([]);
});

test("submits a number once the user pauses typing, as one edit", async () => {
  const { host, n } = await mounted();

  await step(() => {
    type(n, "10");
  });
  await wait(PAUSE - 1);
  expect(host.submitted).toEqual([]);

  await wait(1);
  expect(host.submitted).toEqual([tenHead]);
  expect(host.state()).toEqual(tenHead);
});

test("submits a number at once on Enter, and as the user leaves the field", async () => {
  const { host, n } = await mounted();

  await step(() => {
    type(n, "10");
    press(n, "Enter");
  });
  expect(host.submitted).toEqual([tenHead]);

  await step(() => {
    host.setState(six);
  });
  expect(n.value).toBe("6");
  await step(() => {
    type(n, "10");
  });
  await step(() => {
    n.blur();
  });
  expect(host.submitted).toEqual([tenHead, tenHead]);

  await wait(PAUSE);
  expect(host.submitted).toHaveLength(2);
});

test("submits no number that the field does not take, and marks it and says why", async () => {
  const { host, view, n } = await mounted();
  await settle();
  const shownNow = () => [n.getAttribute("aria-invalid"), fieldError(view.el)];

  const shown: (string | null)[][] = [];
  for (const text of ["200", "-1", "2.5", ""]) {
    await step(() => {
      type(n, text);
    });
    await wait(PAUSE);
    shown.push(shownNow());
  }
  expect(shown).toEqual([
    ["true", said("200")],
    ["true", said("-1")],
    ["true", said("2.5")],
    [null, null],
  ]);
  expect(host.submitted).toEqual([]);

  await step(() => {
    type(n, "10");
  });
  expect(shownNow()).toEqual([null, null]);
  await wait(PAUSE);
  expect(host.submitted).toEqual([tenHead]);
});

test("goes back to the state as the user leaves a number above the rows, which clears what it said", async () => {
  const { host, view, n } = await mounted();
  await settle();
  const shown = () => [n.value, fieldError(view.el)];

  await step(() => {
    type(n, "200");
  });
  expect(shown()).toEqual(["200", said("200")]);
  await step(() => {
    n.blur();
  });
  expect(shown()).toEqual(["6", null]);
  await wait(PAUSE);
  expect(host.submitted).toEqual([]);
});

test("submits the direction as the user picks it, and keeps one picked", async () => {
  const { host, view, direction } = await mounted(tenHead);
  const tail = toggleButton(view.el, "Direction", "Tail");

  await step(() => {
    tail.click();
  });
  expect(host.submitted).toEqual([tenTail]);
  expect(direction()).toBe("Tail");
  expect(toggleButton(view.el, "Direction", "Tail")).toBe(tail);

  await step(() => {
    tail.click();
  });
  expect(host.submitted).toEqual([tenTail]);
  expect(direction()).toBe("Tail");
});

test("leaves the field the user is typing in as the state changes, and submits on top of the state as it is then", async () => {
  const { host, n, direction } = await mounted();

  await step(() => {
    type(n, "20");
  });
  await step(() => {
    host.setState(tenTail);
  });
  expect([n.value, direction()]).toEqual(["20", "Tail"]);

  await wait(PAUSE);
  expect(host.submitted).toEqual([twentyTail]);
});

test("submits an edit still pending as it unmounts", async () => {
  const { host, view, n } = await mounted();

  await step(() => {
    type(n, "10");
  });
  unmount(view);
  expect(host.submitted).toEqual([tenHead]);

  await wait(PAUSE);
  expect(host.submitted).toHaveLength(1);
});

test("asks for the rows of its input again once the input's revision changes", async () => {
  const { host } = await mounted();
  await settle();

  await step(() => {
    host.setContext({ status: { ...READY, status: "stale", revision: 2 } });
  });
  await settle();
  expect(host.requested).toHaveLength(1);

  await step(() => {
    host.setContext({ inputs: [{ input: "data", revision: 2 }] });
  });
  await settle();
  expect(host.requested).toHaveLength(2);
});

test("takes no most while it asks for the rows of its input again, so that it marks no number meanwhile", async () => {
  const host = stubBlockHost<State>(fixture, { state: { n: 200, direction: "head" } });
  const release = holdRequests(host);
  const view = await mountOnPage(head, host);
  const n = labelled(view.el, "Number of rows");
  const shown = () => [n.value, n.getAttribute("aria-invalid"), fieldError(view.el)];
  expect(shown()).toEqual(["200", null, null]);

  release();
  await settle();
  expect(shown()).toEqual(["200", "true", said("200")]);

  await step(() => {
    host.setContext({ inputs: [{ input: "data", revision: 2 }] });
  });
  expect(shown()).toEqual(["200", null, null]);

  release();
  await settle();
  expect(shown()).toEqual(["200", "true", said("200")]);
  expect(host.submitted).toEqual([]);
});

test("takes no most where its input has no rows to give", async () => {
  const host = stubBlockHost<State>({ ...fixture, requests: [] }, { state: { n: 200, direction: "head" } });
  const view = await mountOnPage(head, host);
  await settle();

  expect(labelled(view.el, "Number of rows").getAttribute("aria-invalid")).toBeNull();
  expect(fieldError(view.el)).toBeNull();
});
