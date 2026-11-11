// The scatter block's UI, mounted on the stub of its host
// with the fixture that the scatter block's R tests write,
// which lists every state a test submits.

import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { stubBlockHost } from "../../stubs/index.ts";
import { find, pick, texts } from "../../testing/inputs.ts";
import { blockFixture, mountOnPage, settle } from "../../testing/modules.ts";
import { step } from "../../testing/react.ts";
import { holdRequests } from "../../testing/requests.ts";
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

async function mounted(state: State, requests = fixture.requests) {
  const host = stubBlockHost<State>({ ...fixture, requests }, { state });
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

test("shows a column that its input lacks as it is until the columns arrive, then as missing", async () => {
  const unanswered = await mounted({ x: "wt", y: "Sepal.Width" }, []);
  await settle();
  expect([texts(unanswered.x, "option"), unanswered.x.getAttribute("aria-invalid")]).toEqual([["wt"], "false"]);

  const { host, x, y } = await mounted({ x: "wt", y: "Sepal.Width" });
  await settle();
  expect([x.value, texts(x, "option"), x.getAttribute("aria-invalid")]).toEqual([
    "wt",
    [...columns, "wt (not available)"],
    "true",
  ]);
  expect([y.value, texts(y, "option"), y.getAttribute("aria-invalid")]).toEqual(["Sepal.Width", columns, "false"]);
  expect(host.submitted).toEqual([]);
});

test("marks no column while the columns are asked again, as once its input changed, nor once that ask failed", async () => {
  const requests = [...fixture.requests];
  const host = stubBlockHost<State>({ ...fixture, requests }, { state: { x: "wt", y: "Sepal.Width" } });
  const release = holdRequests(host);
  const view = await mountOnPage(scatter, host);
  const x = find(view.el, "select", HTMLSelectElement);
  const shown = () => [texts(x, "option"), x.getAttribute("aria-invalid")];
  expect(shown()).toEqual([["wt"], "false"]);

  release();
  await settle();
  const marked = [[...columns, "wt (not available)"], "true"];
  expect(shown()).toEqual(marked);

  await step(() => {
    host.setContext({ inputs: [{ input: "data", revision: 2 }] });
  });
  expect(shown()).toEqual([[...columns, "wt"], "false"]);

  // R answers no more, as where the input failed.
  requests.splice(0);
  release();
  await settle();
  expect(shown()).toEqual([[...columns, "wt"], "false"]);

  requests.push(...fixture.requests);
  await step(() => {
    host.setContext({ inputs: [{ input: "data", revision: 3 }] });
  });
  release();
  await settle();
  expect(shown()).toEqual(marked);
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
