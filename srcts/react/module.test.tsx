// The helpers that make a module or a renderer from a React component, mounted
// as core mounts a module, with the stubs of the hosts.

import { describe, expect, test } from "vitest";
import { expectedErrors, step } from "@blockr/ui/stubs";
import { stubOptionHost, stubOutputHost } from "../stubs/index.ts";
import { mountReact } from "../testing/react.ts";
import type { OptionHost } from "../types/hosts.ts";
import type { TextOutput } from "../types/outputs.ts";
import { outputRenderer, reactModule } from "./index.ts";
import type { ModuleProps, OutputProps } from "./index.ts";

const text = (value: string): TextOutput => ({
  renderer: { package: "blockr.core", id: "text_output", module: "blockr.core/js/basic-text.js" },
  text: value,
});

describe("reactModule()", () => {
  test("draws its component with the host and the signal as core mounts it, and empties the element as core unmounts it", async () => {
    const seen: ModuleProps<OptionHost>[] = [];
    function Rows(props: ModuleProps<OptionHost>) {
      seen.push(props);
      return <p>{JSON.stringify(props.host.value())}</p>;
    }
    const mod = reactModule(Rows);
    const host = stubOptionHost({ option: "n_rows", value: 50 });

    const mounted = await mountReact({ api: 1, default: mod }, host);

    expect(mounted.el.textContent).toBe("50");
    expect(seen[0]?.host).toBe(host);
    expect(seen[0]?.signal).toBe(mounted.signal);

    await step(() => {
      mounted.unmount();
    });
    expect(mounted.el.childNodes).toHaveLength(0);
  });

});

describe("outputRenderer()", () => {
  test("hands its component the output, its parameters and the host, through which it reads the board option values, and draws again as the host announces another output", async () => {
    const seen: OutputProps<TextOutput>[] = [];
    const rows: unknown[] = [];
    const renderer = outputRenderer<TextOutput>((props) => {
      seen.push(props);
      rows.push(props.host.option("n_rows"));
      return <pre>{props.output.text}</pre>;
    });
    const host = stubOutputHost({ output: { ...text("one") }, params: { page: 1 }, options: { n_rows: 5 } });

    const { el } = await mountReact({ api: 1, default: renderer }, host);
    expect(el.querySelector("pre")?.textContent).toBe("one");
    expect(seen.at(-1)).toMatchObject({ params: { page: 1 }, body: null, loading: false, error: null, host });
    expect(rows.at(-1)).toBe(5);

    await step(() => {
      host.setOutput({ ...text("two") });
    });
    expect(el.querySelector("pre")?.textContent).toBe("two");
  });

  test("marks itself busy and dims what its component drew while a newer output loads, and draws nothing of why the latest ask failed, which its component gets", async () => {
    const seen: (string | undefined)[] = [];
    const renderer = outputRenderer<TextOutput>(({ output, error }) => {
      seen.push(error?.message);
      return <pre>{output.text}</pre>;
    });
    const host = stubOutputHost({ output: { ...text("one") } });
    const { el } = await mountReact({ api: 1, default: renderer }, host);
    const frame = el.firstElementChild;
    expect(frame?.getAttribute("aria-busy")).toBe("false");
    expect(el.querySelector('[role="alert"]')).toBeNull();

    await step(() => {
      host.setLoading(true);
    });
    expect(frame?.getAttribute("aria-busy")).toBe("true");
    expect(el.querySelector("pre")?.closest(".bc\\:opacity-60")).not.toBeNull();

    // Core raises why the ask failed as a notification about the block, which
    // the frontend places.
    await step(() => {
      host.setError({ severity: "error", class: ["simpleError", "error", "condition"], message: "boom" });
    });
    expect(el.querySelector('[role="alert"]')).toBeNull();
    expect(seen.at(-1)).toBe("boom");
    expect(el.querySelector("pre")?.textContent).toBe("one");
  });

  test("draws nothing in place of a component that throws, raises why through the host and logs it there, and draws it again once another output comes", async () => {
    // React prints each error that the renderer's boundary catches.
    expectedErrors(/^Error: cannot show bad (one|two)\n/);
    const renderer = outputRenderer<TextOutput>(({ output }) => {
      if (output.text.startsWith("bad")) throw new Error(`cannot show ${output.text}`);
      return <pre>{output.text}</pre>;
    });
    const host = stubOutputHost({ output: { ...text("bad one") } });
    const failed = (what: string) => ({
      severity: "error",
      code: "render_failed",
      message: `The renderer failed to draw the output: cannot show ${what}`,
    });
    const { el } = await mountReact({ api: 1, default: renderer }, host);
    expect(el.textContent).toBe("");
    expect(el.querySelector('[role="alert"]')).toBeNull();
    expect(host.notified).toEqual([failed("bad one")]);

    // Another output that it cannot draw replaces the notification.
    await step(() => {
      host.setOutput({ ...text("bad two") });
    });
    expect(el.textContent).toBe("");
    expect(host.notified).toEqual([failed("bad two")]);

    await step(() => {
      host.setOutput({ ...text("good") });
    });
    expect(el.querySelector("pre")?.textContent).toBe("good");
    expect(host.notified).toEqual([]);
    // Each throw is logged at error, with the Error that the component threw.
    expect(host.logged.map((line) => [line.level, line.message])).toEqual([
      ["error", "cannot show bad one"],
      ["error", "cannot show bad two"],
    ]);
    expect(host.logged.every((line) => line.error instanceof Error)).toBe(true);
  });

  test("raises one notification where its component throws on an output that comes later, which holds until the renderer is unmounted", async () => {
    // React prints the error that the renderer's boundary catches.
    expectedErrors(/^Error: no text to show\n/);
    const renderer = outputRenderer<TextOutput>(({ output }) => {
      if (output.text === "bad") throw new Error("no text to show");
      return <pre>{output.text}</pre>;
    });
    const host = stubOutputHost({ output: { ...text("good") } });
    const mounted = await mountReact({ api: 1, default: renderer }, host);
    expect(mounted.el.querySelector("pre")?.textContent).toBe("good");
    expect(host.notified).toEqual([]);

    await step(() => {
      host.setOutput({ ...text("bad") });
    });
    expect(mounted.el.textContent).toBe("");
    expect(host.notified.map((notification) => notification.code)).toEqual(["render_failed"]);

    await step(() => {
      mounted.unmount();
    });
    expect(host.notified).toEqual([]);
    expect(host.logged.map((line) => line.message)).toEqual(["no text to show"]);
  });
});
