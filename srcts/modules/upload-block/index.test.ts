// The upload block's UI, mounted on the stub of its host
// with the fixture that the upload block's R tests write,
// which records what the action upload answers for the file sales.csv.

import { act } from "react";
import { afterEach, beforeEach, expect, test, vi } from "vitest";
import { stubBlockHost } from "../../stubs/index.ts";
import { choose, find } from "../../testing/inputs.ts";
import { blockFixture, mountOnPage, settle } from "../../testing/modules.ts";
import { step } from "../../testing/react.ts";
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
  document.body.replaceChildren();
});

async function mounted(state: State) {
  const host = stubBlockHost<State>(fixture, { state });
  const view = await mountOnPage(upload, host);
  return { host, view, input: find(view.el, 'input[type="file"]', HTMLInputElement) };
}

test("says that no file is uploaded yet, and asks nothing until the user picks one", async () => {
  const { host, view } = await mounted({});
  await settle();

  expect(view.el.textContent).toContain("No file uploaded yet.");
  expect(host.requested).toEqual([]);
  expect(host.submitted).toEqual([]);
});

test("sends the file the user picks to R, and submits the path R keeps it at", async () => {
  const { host, view, input } = await mounted({});
  const file = sales();

  // Acting without waiting on the upload shows the input while it is on its
  // way.
  act(() => {
    choose(input, file);
  });
  expect(input.disabled).toBe(true);
  expect(view.el.textContent).toContain("Uploading sales.csv…");

  await settle();
  expect(host.requested).toEqual([{ name: "upload", args: {}, file }]);
  expect(host.submitted).toEqual([kept]);
  expect(input.disabled).toBe(false);
  expect(view.el.textContent).toContain("Uploaded sales.csv");
});

test("says why an upload failed, and submits nothing", async () => {
  const { host, view, input } = await mounted(kept);

  await step(() => {
    choose(input, new File(["x"], "notes.txt", { type: "text/plain" }));
  });
  await settle();

  expect(host.submitted).toEqual([]);
  expect(find(view.el, '[role="alert"]').hidden).toBe(false);
  expect(find(view.el, '[role="alert"]').textContent).toContain('records no answer of the request "upload"');
  expect(view.el.textContent).toContain("Uploaded sales.csv");
});
