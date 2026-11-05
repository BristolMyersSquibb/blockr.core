// The glue block's UI, mounted on the stub of its host
// with the fixture that the glue block's R tests write,
// which lists every state a test submits.

import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { PAUSE } from "../../binding/index.ts";
import { stubBlockHost } from "../../stubs/index.ts";
import { find, press, type } from "../../testing/inputs.ts";
import { blockFixture, mountOnPage, wait } from "../../testing/modules.ts";
import { step } from "../../testing/react.ts";
import * as glue from "./index.tsx";
import type { State } from "./state.ts";

const fixture = blockFixture("text-glue", "glue_block");

const empty: State = { text: "" };
const rows: State = { text: "{nrow(data)} rows" };

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
  document.body.replaceChildren();
});

async function mounted(state: State) {
  const host = stubBlockHost<State>(fixture, { state });
  const view = await mountOnPage(glue, host);
  return { host, view, area: find(view.el, "textarea", HTMLTextAreaElement) };
}

test("shows the text, and takes a text set from outside", async () => {
  const { host, area } = await mounted(rows);
  expect(area.value).toBe("{nrow(data)} rows");

  await step(() => {
    host.setState(empty);
  });
  expect(area.value).toBe("");
});

test("submits the text once the user pauses typing", async () => {
  const { host, area } = await mounted(empty);

  await step(() => {
    type(area, "{nrow(data)} rows");
  });
  await wait(PAUSE);

  expect(host.submitted).toEqual([rows]);
});

test("submits at once on Ctrl or Cmd with Enter, where Enter alone starts a line", async () => {
  const { host, area } = await mounted(empty);

  await step(() => {
    type(area, "{nrow(data)} rows");
  });
  await step(() => {
    press(area, "Enter");
  });
  expect(host.submitted).toEqual([]);

  await step(() => {
    press(area, "Enter", { metaKey: true });
  });
  expect(host.submitted).toEqual([rows]);
});
