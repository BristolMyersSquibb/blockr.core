// The scatter block's UI, mounted on the stub of its host
// with the fixture that the scatter block's R tests write,
// which lists every state a test submits.

import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { stubBlockHost } from "../../stubs/index.ts";

import { blockFixture, mountOnPage, settle } from "../../testing/modules.ts";
import { step } from "../../testing/react.ts";
import { holdRequests } from "../../testing/requests.ts";
import { labelled, listed, notAvailable, picked } from "../../testing/select.ts";
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
});

async function mounted(state: State, requests = fixture.requests) {
  const host = stubBlockHost<State>({ ...fixture, requests }, { state });
  const view = await mountOnPage(scatter, host);
  return { host, view, x: labelled(view.el, "X-axis"), y: labelled(view.el, "Y-axis") };
}

// What a select shows: its field's text, the rows of its list and whether it
// marks itself invalid.
async function shown(select: HTMLInputElement): Promise<[string, string[], string | null]> {
  return [select.value || select.placeholder, await listed(select, step), select.getAttribute("aria-invalid")];
}

test("shows the state, with a choice of each column of its input on each axis", async () => {
  const { host, x, y } = await mounted(sepals);
  expect([x.value, y.value]).toEqual(["Sepal.Length", "Sepal.Width"]);

  await settle();
  expect(await shown(x)).toEqual(["Sepal.Length", columns, null]);
  expect(await shown(y)).toEqual(["Sepal.Width", columns, null]);
  expect(host.requested).toEqual([{ name: "columns", args: {} }]);
  expect(host.submitted).toEqual([]);
});

test("never picks a column once the columns arrive", async () => {
  const { host, x, y } = await mounted({});
  await settle();

  expect([x.value, y.value, x.placeholder]).toEqual(["", "", "Choose a column"]);
  expect(await shown(x)).toEqual(["Choose a column", columns, null]);
  expect(host.submitted).toEqual([]);
});

test("shows a column that its input lacks as it is, and once the columns arrive, marks it and says below the select that it is not available", async () => {
  const unanswered = await mounted({ x: "wt", y: "Sepal.Width" }, []);
  await settle();
  expect(await shown(unanswered.x)).toEqual(["wt", [], null]);
  expect(notAvailable(unanswered.view.el)).toEqual([]);

  const { host, view, x, y } = await mounted({ x: "wt", y: "Sepal.Width" });
  await settle();
  expect(await shown(x)).toEqual(["wt", columns, "true"]);
  expect(await shown(y)).toEqual(["Sepal.Width", columns, null]);
  expect(notAvailable(view.el)).toEqual(["Not available: wt"]);
  expect(host.submitted).toEqual([]);
});

test("marks no column while the columns are asked again, as once its input changed, nor once that ask failed", async () => {
  const requests = [...fixture.requests];
  const host = stubBlockHost<State>({ ...fixture, requests }, { state: { x: "wt", y: "Sepal.Width" } });
  const release = holdRequests(host);
  const view = await mountOnPage(scatter, host);
  const x = labelled(view.el, "X-axis");
  expect(await shown(x)).toEqual(["wt", [], null]);

  release();
  await settle();
  const marked = ["wt", columns, "true"];
  expect(await shown(x)).toEqual(marked);
  expect(notAvailable(view.el)).toEqual(["Not available: wt"]);

  await step(() => {
    host.setContext({ inputs: [{ input: "data", revision: 2 }] });
  });
  expect(await shown(x)).toEqual(["wt", columns, null]);
  expect(notAvailable(view.el)).toEqual([]);

  // R answers no more, as where the input failed.
  requests.splice(0);
  release();
  await settle();
  expect(await shown(x)).toEqual(["wt", columns, null]);

  requests.push(...fixture.requests);
  await step(() => {
    host.setContext({ inputs: [{ input: "data", revision: 3 }] });
  });
  release();
  await settle();
  expect(await shown(x)).toEqual(marked);
  expect(host.submitted).toEqual([]);
});

test("submits the column the user picks, and takes one set from outside", async () => {
  const { host, x, y } = await mounted(sepals);
  await settle();

  await picked(y, step, "Petal.Length");
  expect(host.submitted).toEqual([lengths]);

  await step(() => {
    host.setState(sepals);
  });
  expect([x.value, y.value]).toEqual(["Sepal.Length", "Sepal.Width"]);
});
