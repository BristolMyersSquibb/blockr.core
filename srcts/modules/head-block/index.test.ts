// The head block's UI, mounted on the stub of its host
// with the fixture that the head block's R tests write,
// which lists every state a test submits.

import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { PAUSE } from "../../binding/index.ts";
import { READY, stubBlockHost } from "../../stubs/index.ts";
import { find, pick, press, type } from "../../testing/inputs.ts";
import { blockFixture, mountOnPage, settle, unmount, wait } from "../../testing/modules.ts";
import { step } from "../../testing/react.ts";
import { holdRequests } from "../../testing/requests.ts";
import * as head from "./index.tsx";
import type { State } from "./state.ts";

const fixture = blockFixture("transform-head", "head_block");

const six: State = { n: 6, direction: "head" };
const tenHead: State = { n: 10, direction: "head" };
const tenTail: State = { n: 10, direction: "tail" };
const twentyTail: State = { n: 20, direction: "tail" };

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
  const n = find(view.el, 'input[type="number"]', HTMLInputElement);
  const direction = find(view.el, "select", HTMLSelectElement);
  return { host, view, n, direction };
}

test("shows the state, and takes the rows of its input as the most the input takes", async () => {
  const { host, n, direction } = await mounted();
  expect([n.value, direction.value]).toEqual(["6", "head"]);

  await settle();
  expect(n.max).toBe("150");
  expect(host.requested).toEqual([{ name: "n_max", args: {} }]);
  expect(host.submitted).toEqual([]);
});

test("takes a state it did not make, in the elements it drew", async () => {
  const { host, view, n, direction } = await mounted();

  await step(() => {
    host.setState(tenTail);
  });

  expect(find(view.el, 'input[type="number"]')).toBe(n);
  expect([n.value, direction.value]).toEqual(["10", "tail"]);
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

test("submits a number at once on Enter, and as the user leaves the input", async () => {
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

test("submits no number that the input does not take, and marks it", async () => {
  const { host, n } = await mounted();
  await settle();

  for (const text of ["200", "-1", "2.5", ""]) {
    await step(() => {
      type(n, text);
    });
    await wait(PAUSE);
    expect(n.getAttribute("aria-invalid")).toBe("true");
  }
  expect(host.submitted).toEqual([]);

  await step(() => {
    type(n, "10");
  });
  expect(n.getAttribute("aria-invalid")).toBe("false");
  await wait(PAUSE);
  expect(host.submitted).toEqual([tenHead]);
});

test("submits the direction as the user picks it", async () => {
  const { host, direction } = await mounted(tenHead);

  await step(() => {
    pick(direction, "tail");
  });

  expect(host.submitted).toEqual([tenTail]);
});

test("leaves the input the user is typing in as the state changes, and submits on top of the state as it is then", async () => {
  const { host, n, direction } = await mounted();

  await step(() => {
    type(n, "20");
  });
  await step(() => {
    host.setState(tenTail);
  });
  expect([n.value, direction.value]).toEqual(["20", "tail"]);

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
  const n = find(view.el, 'input[type="number"]', HTMLInputElement);
  const shown = () => [n.max, n.getAttribute("aria-invalid")];
  expect(shown()).toEqual(["", "false"]);

  release();
  await settle();
  expect(shown()).toEqual(["150", "true"]);

  await step(() => {
    host.setContext({ inputs: [{ input: "data", revision: 2 }] });
  });
  expect(shown()).toEqual(["", "false"]);

  release();
  await settle();
  expect(shown()).toEqual(["150", "true"]);
  expect(host.submitted).toEqual([]);
});

test("takes no most where its input has no rows to give", async () => {
  const host = stubBlockHost<State>({ ...fixture, requests: [] }, { state: six });
  const view = await mountOnPage(head, host);
  await settle();

  expect(find(view.el, 'input[type="number"]', HTMLInputElement).max).toBe("");
  expect(view.el.querySelector('[role="alert"]')).toBeNull();
});
