// The merge block's UI, mounted on the stub of its host
// with the fixture that the merge block's R tests write,
// which lists every state a test submits.

import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { stubBlockHost } from "../../stubs/index.ts";
import { find, toggle } from "../../testing/inputs.ts";
import { blockFixture, mountOnPage, settle } from "../../testing/modules.ts";
import { step } from "../../testing/react.ts";
import { holdRequests } from "../../testing/requests.ts";
import { chipLabels, labelled, listed, listNote, picked } from "../../testing/select.ts";
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
  return { host, view, by: labelled(view.el, "By columns"), checkbox };
}

test("shows the state, with a choice of each column its inputs share", async () => {
  const { host, by, checkbox } = await mounted(time);
  await settle();

  expect(chipLabels(by)).toEqual(["Time"]);
  expect(await listed(by, step)).toEqual(["Time", "demand"]);
  expect(checkbox("Keep the rows of x that y does not match").checked).toBe(false);
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

test("submits whether to keep the unmatched rows of an input", async () => {
  const { host, checkbox } = await mounted(time);

  await step(() => {
    toggle(checkbox("Keep the rows of x that y does not match"));
  });

  expect(host.submitted).toEqual([left]);
});

test("shows a column it joins by that the inputs do not share, as missing once R says which they share", async () => {
  const unanswered = await mounted({ by: ["rate"], all_x: false, all_y: false }, []);
  await settle();
  expect([chipLabels(unanswered.by), unanswered.by.getAttribute("aria-invalid")]).toEqual([["rate"], null]);
  expect(await listed(unanswered.by, step)).toEqual([]);

  const { by } = await mounted({ by: ["rate"], all_x: false, all_y: false });
  await settle();
  expect([chipLabels(by), by.getAttribute("aria-invalid")]).toEqual([["rate (not available)"], "true"]);
  expect(await listed(by, step)).toEqual(["Time", "demand"]);
});

test("keeps every column the inputs shared while R says again which they share, and marks none meanwhile", async () => {
  const host = stubBlockHost<State>(fixture, { state: { by: ["rate"], all_x: false, all_y: false } });
  const release = holdRequests(host);
  const view = await mountOnPage(merge, host);
  const by = labelled(view.el, "By columns");
  const shown = async () => [await listed(by, step), chipLabels(by), by.getAttribute("aria-invalid")];
  release();
  await settle();
  const marked = [["Time", "demand"], ["rate (not available)"], "true"];
  expect(await shown()).toEqual(marked);

  await step(() => {
    host.setContext({ inputs: [{ input: "x", revision: 2 }] });
  });
  expect(await shown()).toEqual([["Time", "demand"], ["rate"], null]);

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
