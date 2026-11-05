// Core's plain renderer of text outputs, mounted with the stub of its host on
// the sample text outputs that R writes for the class.

import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { expect, test } from "vitest";
import { stubOutputHost } from "../../stubs/index.ts";
import { mountReact, step } from "../../testing/react.ts";
import type { OutputFixtures, TextOutput } from "../../types/outputs.ts";
import * as basicText from "./index.tsx";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../../..");
const fixtures = JSON.parse(
  readFileSync(join(root, "inst", "outputs", "fixtures", "text-output.json"), "utf8"),
) as OutputFixtures<TextOutput>;

const texts = fixtures.outputs.map(({ output }) => output);

test("shows each sample as print() shows it, in a box that scrolls rather than wraps", async () => {
  for (const output of texts) {
    const { el } = await mountReact(basicText, stubOutputHost({ output: { ...output } }));
    const pre = el.querySelector("pre");
    expect(pre?.textContent).toBe(output.text);
    expect(pre?.className).toContain("bc:whitespace-pre");
    expect(pre?.className).toContain("bc:overflow-auto");
    expect(pre?.tabIndex).toBe(0);
  }
  expect(basicText.api).toBe(1);
  expect(basicText.default.options).toBeUndefined();
});

test("draws each output that core hands it, marks it busy while a newer one loads, and shows why an ask failed over it", async () => {
  const [first, second] = texts;
  const host = stubOutputHost({ output: { ...first } });
  const mounted = await mountReact(basicText, host);
  const { el } = mounted;
  const pre = el.querySelector("pre");
  expect(el.querySelector('[role="alert"]')).toBeNull();

  await step(() => {
    host.setLoading(true);
  });
  expect(el.firstElementChild?.getAttribute("aria-busy")).toBe("true");
  await step(() => {
    host.setOutput({ ...second });
  });
  expect(pre?.textContent).toBe(second?.text);
  expect(el.firstElementChild?.getAttribute("aria-busy")).toBe("false");

  await step(() => {
    host.setError({ severity: "error", class: ["simpleError", "error", "condition"], phase: "output", message: "boom" });
  });
  expect(el.querySelector('[role="alert"]')?.textContent).toBe("boom");
  expect(pre?.textContent).toBe(second?.text);

  await step(() => {
    mounted.unmount();
  });
  await step(() => {
    host.setOutput({ ...first });
  });
  expect(pre?.textContent).toBe(second?.text);
});
