// Core's plain renderer of text outputs, mounted with the stub of its host on
// the sample text outputs that R writes for the class.

import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { expect, test } from "vitest";
import { mount, stubOutputHost } from "../../stubs/index.ts";
import type { OutputFixtures, TextOutput } from "../../types/outputs.ts";
import * as basicText from "./index.ts";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../../..");
const fixtures = JSON.parse(
  readFileSync(join(root, "inst", "outputs", "fixtures", "text-output.json"), "utf8"),
) as OutputFixtures<TextOutput>;

const texts = fixtures.outputs.map(({ output }) => output);

test("shows each sample as print() shows it, in a box that scrolls rather than wraps", async () => {
  for (const output of texts) {
    const { el } = await mount(basicText, stubOutputHost({ output: { ...output } }));
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
  const mounted = await mount(basicText, host);
  const { el } = mounted;
  const pre = el.querySelector("pre");
  const alert = el.querySelector<HTMLElement>('[role="alert"]');
  expect(alert?.hidden).toBe(true);

  host.setLoading(true);
  expect(el.firstElementChild?.getAttribute("aria-busy")).toBe("true");
  host.setOutput({ ...second });
  expect(pre?.textContent).toBe(second?.text);
  expect(el.firstElementChild?.getAttribute("aria-busy")).toBe("false");

  host.setError({ severity: "error", class: ["simpleError", "error", "condition"], phase: "output", message: "boom" });
  expect(alert?.hidden).toBe(false);
  expect(alert?.textContent).toBe("boom");
  expect(pre?.textContent).toBe(second?.text);

  mounted.unmount();
  host.setOutput({ ...first });
  expect(pre?.textContent).toBe(second?.text);
});
