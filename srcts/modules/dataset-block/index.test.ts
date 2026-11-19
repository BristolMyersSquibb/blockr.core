// The dataset block's UI, mounted on the stub of its host
// with the fixture that the dataset block's R tests write,
// which lists every state a test submits.

import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { stubBlockHost } from "../../stubs/index.ts";
import { labelled, listed, picked } from "../../testing/select.ts";
import { blockFixture, mountOnPage, settle } from "../../testing/modules.ts";
import { step } from "../../testing/react.ts";
import { holdRequests } from "../../testing/requests.ts";
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
  return { host, view, select: labelled(view.el, "Dataset") };
}

// What the select shows: its field's text, the rows of its list and whether
// it marks itself invalid.
async function shown(select: HTMLInputElement): Promise<[string, string[], string | null]> {
  return [select.value || select.placeholder, await listed(select, step), select.getAttribute("aria-invalid")];
}

test("shows the state, with a choice of each data frame the package ships", async () => {
  const { host, select } = await mounted(iris);
  await settle();

  expect(await shown(select)).toEqual(["iris", ["BOD", "iris", "mtcars"], null]);
  expect(host.requested).toEqual([{ name: "datasets", args: {} }]);
  expect(host.submitted).toEqual([]);
});

test("shows a dataset that the choices lack as it is, and marks it once they arrive", async () => {
  const unanswered = await mounted({ dataset: "Nile", package: "datasets" }, []);
  await settle();
  expect(await shown(unanswered.select)).toEqual(["Nile", [], null]);

  const { host, select } = await mounted({ dataset: "Nile", package: "datasets" });
  await settle();
  expect(await shown(select)).toEqual(["Nile", ["BOD", "iris", "mtcars"], "true"]);
  expect(host.submitted).toEqual([]);
});

test("never picks a dataset once the choices arrive", async () => {
  const { host, select } = await mounted({ package: "datasets" });
  await settle();

  expect([select.value, select.placeholder]).toEqual(["", "Choose a dataset"]);
  expect(await shown(select)).toEqual(["Choose a dataset", ["BOD", "iris", "mtcars"], null]);
  expect(host.submitted).toEqual([]);
});

test("submits the dataset the user picks, and takes one set from outside", async () => {
  const { host, select } = await mounted(iris);
  await settle();

  await picked(select, step, "BOD");
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
  expect(await shown(select)).toEqual(["iris", ["BOD", "iris", "mtcars"], null]);
});

test("marks no dataset while it asks for the datasets of another package, nor once that ask failed", async () => {
  const host = stubBlockHost<State>(fixture, { state: iris });
  const release = holdRequests(host);
  const view = await mountOnPage(dataset, host);
  const select = labelled(view.el, "Dataset");
  release();
  await settle();
  expect(await shown(select)).toEqual(["iris", ["BOD", "iris", "mtcars"], null]);

  await step(() => {
    host.setState({ dataset: "Boston", package: "MASS" });
  });
  expect(await shown(select)).toEqual(["Boston", ["BOD", "iris", "mtcars"], null]);

  // The fixture records no datasets of MASS.
  release();
  await settle();
  expect(await shown(select)).toEqual(["Boston", ["BOD", "iris", "mtcars"], null]);
  expect(host.submitted).toEqual([]);
});
