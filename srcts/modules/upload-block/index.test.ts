// The upload block's UI, mounted on the stub of its host
// with the fixture that the upload block's R tests write,
// which records what the action upload answers for the file sales.csv.

import { act } from "react";
import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { choose, drop, find, picking, step, texts } from "@blockr/ui/stubs";
import { stubBlockHost } from "../../stubs/index.ts";
import { blockFixture, mountOnPage, settle } from "../../testing/modules.ts";
import { holdRequests } from "../../testing/requests.ts";
import * as upload from "./index.tsx";
import type { State } from "./state.ts";

const fixture = blockFixture("file-upload", "upload_block");

const kept: State = { file_path: "uploads/3f9a2c/sales.csv" };

// The file the R tests upload, of 18 bytes.
const sales = (): File => new File(["Time,demand\n1,8.3\n"], "sales.csv", { type: "text/csv" });

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
});

async function mounted(state: State) {
  const host = stubBlockHost<State>(fixture, { state });
  const view = await mountOnPage(upload, host);
  return {
    host,
    view,
    // The browser's file input, which the button opens, and the button.
    input: find(view.el, 'input[type="file"]', HTMLInputElement),
    button: buttonNamed(view.el, "Choose a file"),
  };
}

// The button of the text given, in `el`.
function buttonNamed(el: HTMLElement, text: string): HTMLButtonElement {
  const found = [...el.querySelectorAll("button")].find((button) => button.textContent.trim() === text);
  if (found === undefined) throw new Error(`Found no button ${text}.`);
  return found;
}

test("says nothing below the zone, and asks nothing, until the user picks a file", async () => {
  const { host, view } = await mounted({});
  await settle();

  expect(texts(view.el, "p")).toEqual([]);
  expect(host.requested).toEqual([]);
  expect(host.submitted).toEqual([]);
});

test("opens the browser's file picker from its button", async () => {
  const { button } = await mounted({});
  const clicked = picking(undefined);

  await step(() => {
    button.click();
  });

  expect(clicked).toEqual([{ accept: "", connected: true }]);
});

test("sends the file the user picks to R, and submits the path R keeps it at", async () => {
  const { host, view, input, button } = await mounted({});
  const file = sales();

  // Acting without waiting on the upload shows the input while it is on its
  // way.
  act(() => {
    choose(input, file);
  });
  expect(button.disabled).toBe(true);
  expect(view.el.textContent).toContain("Uploading sales.csv…");

  await settle();
  expect(host.requested).toEqual([{ name: "upload", args: {}, file }]);
  expect(host.submitted).toEqual([kept]);
  expect(button.disabled).toBe(false);
  expect(view.el.textContent).toContain("Uploaded sales.csv");
});

test("sends a file that the user drops on its zone to R, and takes no other while one is on its way", async () => {
  const { host, view, button } = await mounted({});
  const zone = find(view.el, "[data-rac][class*='border-dashed']");
  const release = holdRequests(host);
  const file = sales();

  await step(() => {
    drop(zone, file);
  });
  // React Aria reads the dropped file in a promise.
  await step(() => undefined);
  expect([view.el.textContent.includes("Uploading sales.csv…"), button.disabled, zone.hasAttribute("data-disabled")]).toEqual([true, true, true]);
  await step(() => {
    drop(zone, new File(["x"], "notes.txt", { type: "text/plain" }));
  });
  await step(() => undefined);

  await act(async () => {
    release();
    await Promise.resolve();
  });
  await settle();
  expect(host.requested).toEqual([{ name: "upload", args: {}, file }]);
  expect(host.submitted).toEqual([kept]);
  expect(view.el.textContent).toContain("Uploaded sales.csv");
});

test("leaves an upload that fails to core's notification of R's error reply, drawing nothing of it, and submits nothing", async () => {
  const { host, view, input } = await mounted(kept);

  await step(() => {
    choose(input, new File(["x"], "notes.txt", { type: "text/plain" }));
  });
  await settle();

  expect(host.submitted).toEqual([]);
  expect(view.el.querySelector('[role="alert"]')).toBeNull();
  expect(host.notified).toEqual([]);
  expect(view.el.textContent).toContain("Uploaded sales.csv");
});

test("raises through its host that R kept the file but answered no path, until the next upload starts, and submits nothing", async () => {
  const pathless = { ...fixture, requests: fixture.requests.map((request) => ({ ...request, answer: [] })) };
  const host = stubBlockHost<State>(pathless, { state: {} });
  const view = await mountOnPage(upload, host);
  const input = find(view.el, 'input[type="file"]', HTMLInputElement);

  await step(() => {
    choose(input, sales());
  });
  await settle();
  const unanswered = { severity: "error", code: "upload_unanswered", message: "R kept sales.csv but gave no path for it." };
  expect(host.notified).toEqual([unanswered]);
  expect(view.el.querySelector('[role="alert"]')).toBeNull();
  expect(host.submitted).toEqual([]);

  act(() => {
    choose(input, sales());
  });
  expect(host.notified).toEqual([]);
  await settle();
  expect(host.notified).toEqual([unanswered]);
});
