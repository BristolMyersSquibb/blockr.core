// The CSV block's UI, mounted on the stub of its host
// with the fixture that the CSV block's R tests write,
// which lists every state a test submits.

import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { PAUSE } from "../../binding/index.ts";
import { stubBlockHost } from "../../stubs/index.ts";
import { type } from "../../testing/inputs.ts";
import { blockFixture, mountOnPage, wait } from "../../testing/modules.ts";
import { step } from "../../testing/react.ts";
import * as csv from "./index.tsx";
import type { State } from "./state.ts";

const fixture = blockFixture("parser-csv", "csv_block");

const comma: State = { sep: ",", quote: '"' };
const semicolon: State = { sep: ";", quote: '"' };
const bare: State = { sep: ";", quote: "" };

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
  document.body.replaceChildren();
});

test("shows the separator and the quoting characters, and submits each as the user pauses typing", async () => {
  const host = stubBlockHost<State>(fixture, { state: comma });
  const view = await mountOnPage(csv, host);
  const [sep, quote] = view.el.querySelectorAll("input");
  if (sep === undefined || quote === undefined) throw new Error("Found no inputs.");
  expect([sep.value, quote.value]).toEqual([",", '"']);

  await step(() => {
    type(sep, ";");
  });
  await wait(PAUSE);
  expect(host.submitted).toEqual([semicolon]);

  await step(() => {
    type(quote, "");
  });
  await wait(PAUSE);
  expect(host.submitted).toEqual([semicolon, bare]);
});
