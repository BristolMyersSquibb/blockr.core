// The merge block's UI, mounted on the stub of its host
// with the fixture that the merge block's R tests write,
// which lists every state a test submits.

import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { stubBlockHost } from "../../stubs/index.ts";
import { texts } from "../../testing/inputs.ts";
import { blockFixture, mountOnPage, settle } from "../../testing/modules.ts";
import { step } from "../../testing/react.ts";
import { holdRequests } from "../../testing/requests.ts";
import { chipLabels, labelled, listed, listNote, notAvailable, picked, selectButton, selectShows } from "../../testing/select.ts";
import * as merge from "./index.tsx";
import type { State } from "./state.ts";

const fixture = blockFixture("transform-merge", "merge_block");

const time: State = { by: ["Time"], all_x: false, all_y: false };
const both: State = { by: ["Time", "demand"], all_x: false, all_y: false };
const demandFirst: State = { by: ["demand", "Time"], all_x: false, all_y: false };
const left: State = { by: ["Time"], all_x: true, all_y: false };
const right: State = { by: ["Time"], all_x: false, all_y: true };
const full: State = { by: ["Time"], all_x: true, all_y: true };

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
  return { host, view, by: labelled(view.el, "By columns"), join: selectButton(view.el, "Join type") };
}

test("shows the state, with a choice of each column its inputs share", async () => {
  const { host, by, join } = await mounted(time);
  await settle();

  expect(chipLabels(by)).toEqual(["Time"]);
  expect(await listed(by, step)).toEqual(["Time", "demand"]);
  expect(selectShows(join)).toBe("Inner");
  expect(host.requested).toEqual([{ name: "shared", args: {} }]);
  expect(host.submitted).toEqual([]);
});

test("never sets the columns to join by once the shared columns arrive", async () => {
  const { host, by } = await mounted({ all_x: false, all_y: false });
  await settle();

  expect([chipLabels(by), by.value]).toEqual([[], ""]);
  expect(host.submitted).toEqual([]);
});

test("submits a column the user picks after those the state holds, and one the user unticks, as the list closes", async () => {
  const { host, by } = await mounted(time);
  await settle();

  await picked(by, step, "demand");
  expect(host.submitted).toEqual([both]);

  await picked(by, step, "demand");
  expect(host.submitted).toEqual([both, time]);
});

test("builds a submit from the state as it is, keeping a column set from outside", async () => {
  const { host, by } = await mounted(time);
  await settle();

  await step(() => {
    host.setState({ by: ["demand"], all_x: false, all_y: false });
  });
  expect(chipLabels(by)).toEqual(["demand"]);

  await picked(by, step, "Time");
  expect(host.submitted).toEqual([demandFirst]);
});

test("shows the join that all_x and all_y make, of the four it offers, and one set from outside", async () => {
  const { host, join } = await mounted(left);

  expect(selectShows(join)).toBe("Left");
  expect(await listed(join, step)).toEqual(["Inner", "Left", "Right", "Full"]);

  await step(() => {
    host.setState(full);
  });
  expect(selectShows(join)).toBe("Full");
  expect(host.submitted).toEqual([]);
});

test("submits both state args of the join the user picks, in one submit", async () => {
  const { host, join } = await mounted(left);

  await picked(join, step, "Right");
  expect(host.submitted).toEqual([right]);
  expect(selectShows(join)).toBe("Right");

  await picked(join, step, "Full");
  await picked(join, step, "Inner");
  expect(host.submitted).toEqual([right, full, time]);
});

test("submits nothing where the user picks the join the state holds", async () => {
  const { host, join } = await mounted(time);

  await picked(join, step, "Inner");
  expect(host.submitted).toEqual([]);
  expect(selectShows(join)).toBe("Inner");
});

test("says below the select why the host refused the join the user picked, and shows the join the state holds again", async () => {
  // The fixture lists no left join by these columns, so the host refuses it.
  const { host, view, join } = await mounted(demandFirst);

  await picked(join, step, "Left");
  expect(host.submitted).toEqual([{ ...demandFirst, all_x: true }]);
  expect(selectShows(join)).toBe("Inner");
  expect(texts(view.el, '[role="alert"]')).toEqual([expect.stringContaining("which the fixture does not list")]);
});

test("shows a column it joins by that the inputs do not share, as missing once R says which they share", async () => {
  const unanswered = await mounted({ by: ["rate"], all_x: false, all_y: false }, []);
  await settle();
  expect([chipLabels(unanswered.by), unanswered.by.getAttribute("aria-invalid"), notAvailable(unanswered.view.el)]).toEqual([["rate"], null, []]);
  expect(await listed(unanswered.by, step)).toEqual([]);

  const { view, by } = await mounted({ by: ["rate"], all_x: false, all_y: false });
  await settle();
  expect([chipLabels(by), by.getAttribute("aria-invalid"), notAvailable(view.el)]).toEqual([["rate"], "true", ["Not available: rate"]]);
  expect(await listed(by, step)).toEqual(["Time", "demand"]);
});

test("keeps every column the inputs shared while R says again which they share, and marks none meanwhile", async () => {
  const host = stubBlockHost<State>(fixture, { state: { by: ["rate"], all_x: false, all_y: false } });
  const release = holdRequests(host);
  const view = await mountOnPage(merge, host);
  const by = labelled(view.el, "By columns");
  const shown = async () => [await listed(by, step), chipLabels(by), by.getAttribute("aria-invalid"), notAvailable(view.el)];
  release();
  await settle();
  const marked = [["Time", "demand"], ["rate"], "true", ["Not available: rate"]];
  expect(await shown()).toEqual(marked);

  await step(() => {
    host.setContext({ inputs: [{ input: "x", revision: 2 }] });
  });
  expect(await shown()).toEqual([["Time", "demand"], ["rate"], null, []]);

  release();
  await settle();
  expect(await shown()).toEqual(marked);
  expect(host.submitted).toEqual([]);
});

test("says that the inputs share no column once R says so, and not before", async () => {
  const unanswered = await mounted({ all_x: false, all_y: false }, []);
  await settle();
  await step(() => {
    unanswered.by.click();
  });
  expect(listNote(unanswered.by)).toBe("Loading…");

  const none = [{ name: "shared", args: {}, options: {}, answer: [] }];
  const answered = await mounted({ all_x: false, all_y: false }, none);
  await settle();
  await step(() => {
    answered.by.click();
  });
  expect(listNote(answered.by)).toBe("The inputs share no column.");
});
