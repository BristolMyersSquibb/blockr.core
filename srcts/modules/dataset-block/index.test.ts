// The dataset block's UI, mounted on the stub of its host
// with the fixture that the dataset block's R tests write,
// which lists every state a test submits.

import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { stubBlockHost } from "../../stubs/index.ts";
import { find, pick, texts } from "../../testing/inputs.ts";
import { blockFixture, mountOnPage, settle } from "../../testing/modules.ts";
import { step } from "../../testing/react.ts";
import * as dataset from "./index.tsx";
import type { State } from "./state.ts";

const fixture = blockFixture("data-dataset", "dataset_block");

const iris: State = { dataset: "iris", package: "datasets" };
const bod: State = { dataset: "BOD", package: "datasets" };

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
  document.body.replaceChildren();
});

async function mounted(state: State, requests = fixture.requests) {
  const host = stubBlockHost<State>({ ...fixture, requests }, { state });
  const view = await mountOnPage(dataset, host);
  return { host, view, select: find(view.el, "select", HTMLSelectElement) };
}

test("shows the state, with a choice of each data frame the package ships", async () => {
  const { host, select } = await mounted(iris);
  await settle();

  expect([select.value, texts(select, "option")]).toEqual(["iris", ["BOD", "iris", "mtcars"]]);
  expect(host.requested).toEqual([{ name: "datasets", args: {} }]);
  expect(host.submitted).toEqual([]);
});

test("shows a dataset that the choices lack as it is until they arrive, then as missing", async () => {
  const unanswered = await mounted({ dataset: "Nile", package: "datasets" }, []);
  await settle();
  expect([texts(unanswered.select, "option"), unanswered.select.getAttribute("aria-invalid")]).toEqual([
    ["Nile"],
    "false",
  ]);

  const { host, select } = await mounted({ dataset: "Nile", package: "datasets" });
  await settle();
  expect([select.value, texts(select, "option"), select.getAttribute("aria-invalid")]).toEqual([
    "Nile",
    ["BOD", "iris", "mtcars", "Nile (not available)"],
    "true",
  ]);
  expect(host.submitted).toEqual([]);
});

test("never picks a dataset once the choices arrive", async () => {
  const { host, select } = await mounted({ package: "datasets" });
  await settle();

  expect(select.value).toBe("");
  expect(texts(select, "option")).toEqual(["Choose a dataset", "BOD", "iris", "mtcars"]);
  expect(host.submitted).toEqual([]);
});

test("submits the dataset the user picks, and takes one set from outside", async () => {
  const { host, select } = await mounted(iris);
  await settle();

  await step(() => {
    pick(select, "BOD");
  });
  expect(host.submitted).toEqual([bod]);

  await step(() => {
    host.setState(iris);
  });
  expect(select.value).toBe("iris");
});

test("asks again where the package changes, and keeps its choices where that fails", async () => {
  const { host, select } = await mounted(iris);
  await settle();

  await step(() => {
    host.setState({ dataset: "iris", package: "MASS" });
  });
  await settle();

  expect(host.requested).toHaveLength(2);
  expect(texts(select, "option")).toEqual(["BOD", "iris", "mtcars"]);
});
