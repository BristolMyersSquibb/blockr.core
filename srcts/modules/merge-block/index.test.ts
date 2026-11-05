// The merge block's UI, mounted on the stub of its host
// with the fixture that the merge block's R tests write,
// which lists every state a test submits.

import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { stubBlockHost } from "../../stubs/index.ts";
import { find, texts, toggle } from "../../testing/inputs.ts";
import { blockFixture, mountOnPage, settle } from "../../testing/modules.ts";
import { step } from "../../testing/react.ts";
import * as merge from "./index.tsx";
import type { State } from "./state.ts";

const fixture = blockFixture("transform-merge", "merge_block");

const time: State = { by: ["Time"], all_x: false, all_y: false };
const both: State = { by: ["Time", "demand"], all_x: false, all_y: false };
const demandFirst: State = { by: ["demand", "Time"], all_x: false, all_y: false };
const left: State = { by: ["Time"], all_x: true, all_y: false };

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
  document.body.replaceChildren();
});

async function mounted(state: State, requests = fixture.requests) {
  const host = stubBlockHost<State>({ ...fixture, requests }, { state });
  const view = await mountOnPage(merge, host);
  const checkbox = (label: string): HTMLInputElement => {
    const found = [...view.el.querySelectorAll("label")].find((node) => node.textContent === label);
    if (found === undefined) throw new Error(`Found no checkbox ${label}.`);
    return find(found, "input", HTMLInputElement);
  };
  const checked = (): string[] =>
    [...view.el.querySelectorAll<HTMLInputElement>("fieldset input:checked")].map((input) => input.value);
  return { host, view, checkbox, checked };
}

test("shows the state, with a checkbox for each column its inputs share", async () => {
  const { host, view, checkbox, checked } = await mounted(time);
  await settle();

  expect(texts(view.el, "fieldset label")).toEqual(["Time", "demand"]);
  expect(checked()).toEqual(["Time"]);
  expect(checkbox("Keep the rows of x that y does not match").checked).toBe(false);
  expect(host.requested).toEqual([{ name: "shared", args: {} }]);
  expect(host.submitted).toEqual([]);
});

test("never sets the columns to join by once the shared columns arrive", async () => {
  const { host, checked } = await mounted({ all_x: false, all_y: false });
  await settle();

  expect(checked()).toEqual([]);
  expect(host.submitted).toEqual([]);
});

test("submits a column the user checks after those the state holds, and one the user clears", async () => {
  const { host, checkbox } = await mounted(time);
  await settle();

  await step(() => {
    toggle(checkbox("demand"));
  });
  expect(host.submitted).toEqual([both]);

  await step(() => {
    toggle(checkbox("demand"));
  });
  expect(host.submitted).toEqual([both, time]);
});

test("builds a submit from the state as it is, keeping a column set from outside", async () => {
  const { host, checkbox, checked } = await mounted(time);
  await settle();

  await step(() => {
    host.setState({ by: ["demand"], all_x: false, all_y: false });
  });
  expect(checked()).toEqual(["demand"]);

  await step(() => {
    toggle(checkbox("Time"));
  });
  expect(host.submitted).toEqual([demandFirst]);
});

test("submits whether to keep the unmatched rows of an input", async () => {
  const { host, checkbox } = await mounted(time);

  await step(() => {
    toggle(checkbox("Keep the rows of x that y does not match"));
  });

  expect(host.submitted).toEqual([left]);
});

test("shows a column it joins by that the inputs do not share, so that the user can clear it", async () => {
  const { checked, view } = await mounted({ by: ["rate"], all_x: false, all_y: false });
  await settle();

  expect(texts(view.el, "fieldset label")).toEqual(["Time", "demand", "rate"]);
  expect(checked()).toEqual(["rate"]);
});

test("says that the inputs share no column once R says so, and not before", async () => {
  const unanswered = await mounted({ all_x: false, all_y: false }, []);
  await settle();
  expect(unanswered.view.el.textContent).not.toContain("The inputs share no column.");

  const none = [{ name: "shared", args: {}, options: {}, answer: [] }];
  const answered = await mounted({ all_x: false, all_y: false }, none);
  await settle();
  expect(answered.view.el.textContent).toContain("The inputs share no column.");
});
