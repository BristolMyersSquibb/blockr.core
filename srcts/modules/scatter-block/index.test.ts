// The scatter block's UI, mounted on the stub of its host
// with the fixture that the scatter block's R tests write,
// which lists every state a test submits.

import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { stubBlockHost } from "../../stubs/index.ts";
import { pick, texts } from "../../testing/inputs.ts";
import { blockFixture, mountOnPage, settle } from "../../testing/modules.ts";
import { step } from "../../testing/react.ts";
import * as scatter from "./index.tsx";
import type { State } from "./state.ts";

const fixture = blockFixture("plot-scatter", "scatter_block");

const sepals: State = { x: "Sepal.Length", y: "Sepal.Width" };
const lengths: State = { x: "Sepal.Length", y: "Petal.Length" };
const columns = ["Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species"];

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
  document.body.replaceChildren();
});

async function mounted(state: State) {
  const host = stubBlockHost<State>(fixture, { state });
  const view = await mountOnPage(scatter, host);
  const [x, y] = view.el.querySelectorAll("select");
  if (x === undefined || y === undefined) throw new Error("Found no selects.");
  return { host, view, x, y };
}

test("shows the state, with a choice of each column of its input on each axis", async () => {
  const { host, x, y } = await mounted(sepals);
  expect([x.value, y.value]).toEqual(["Sepal.Length", "Sepal.Width"]);

  await settle();
  expect(texts(x, "option")).toEqual(columns);
  expect(texts(y, "option")).toEqual(columns);
  expect(host.requested).toEqual([{ name: "columns", args: {} }]);
  expect(host.submitted).toEqual([]);
});

test("never picks a column once the columns arrive", async () => {
  const { host, x, y } = await mounted({});
  await settle();

  expect([x.value, y.value]).toEqual(["", ""]);
  expect(texts(x, "option")).toEqual(["Choose a column", ...columns]);
  expect(host.submitted).toEqual([]);
});

test("submits the column the user picks, and takes one set from outside", async () => {
  const { host, x, y } = await mounted(sepals);
  await settle();

  await step(() => {
    pick(y, "Petal.Length");
  });
  expect(host.submitted).toEqual([lengths]);

  await step(() => {
    host.setState(sepals);
  });
  expect([x.value, y.value]).toEqual(["Sepal.Length", "Sepal.Width"]);
});
