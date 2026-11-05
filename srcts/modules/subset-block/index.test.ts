// The subset block's UI, mounted on the stub of its host
// with the fixture that the subset block's R tests write,
// which lists every state a test submits.

import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { PAUSE } from "../../binding/index.ts";
import { stubBlockHost } from "../../stubs/index.ts";
import { press, type } from "../../testing/inputs.ts";
import { blockFixture, mountOnPage, wait } from "../../testing/modules.ts";
import { step } from "../../testing/react.ts";
import * as subset from "./index.tsx";
import type { State } from "./state.ts";

const fixture = blockFixture("transform-subset", "subset_block");

const none: State = { subset: "", select: "" };
const rows: State = { subset: "Sepal.Width > 3", select: "" };
const columns: State = { subset: "", select: "Species" };
const all: State = { subset: "Sepal.Width > 3", select: "c(Sepal.Length, Species)" };
const both: State = { subset: "Sepal.Width > 3", select: "Species" };

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
  document.body.replaceChildren();
});

async function mounted(state: State) {
  const host = stubBlockHost<State>(fixture, { state });
  const view = await mountOnPage(subset, host);
  const [rowInput, columnInput] = view.el.querySelectorAll("input");
  if (rowInput === undefined || columnInput === undefined) throw new Error("Found no inputs.");
  return { host, view, rowInput, columnInput };
}

test("shows each expression the state holds", async () => {
  const { host, rowInput, columnInput } = await mounted(all);

  expect([rowInput.value, columnInput.value]).toEqual(["Sepal.Width > 3", "c(Sepal.Length, Species)"]);
  expect(host.requested).toEqual([]);
});

test("submits an expression once the user presses Enter, not as the user pauses, as one edit", async () => {
  const { host, rowInput } = await mounted(none);

  await step(() => {
    type(rowInput, "Sepal.Width > 3");
  });
  await wait(PAUSE * 4);
  expect(host.submitted).toEqual([]);

  await step(() => {
    press(rowInput, "Enter");
  });
  expect(host.submitted).toEqual([rows]);
});

test("leaves the input the user is typing in as the state changes, and submits on top of the state as it is then", async () => {
  const { host, rowInput, columnInput } = await mounted(none);

  await step(() => {
    type(rowInput, "Sepal.Width > 3");
  });
  await step(() => {
    host.setState(columns);
  });
  expect([rowInput.value, columnInput.value]).toEqual(["Sepal.Width > 3", "Species"]);

  await step(() => {
    rowInput.blur();
  });
  expect(host.submitted).toEqual([both]);
});

test("submits a cleared input, and nothing where the text is what the state holds", async () => {
  const { host, rowInput } = await mounted(rows);

  await step(() => {
    type(rowInput, "Sepal.Width > 3");
  });
  await step(() => {
    press(rowInput, "Enter");
  });
  expect(host.submitted).toEqual([]);

  await step(() => {
    type(rowInput, "");
  });
  await step(() => {
    press(rowInput, "Enter");
  });
  expect(host.submitted).toEqual([none]);
});
