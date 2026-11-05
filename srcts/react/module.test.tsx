// The helpers that make a module or a renderer from a React component, mounted
// as core mounts a module, with the stubs of the hosts.

import { afterEach, describe, expect, test, vi } from "vitest";
import { stubOptionHost, stubOutputHost } from "../stubs/index.ts";
import { mountReact, step } from "../testing/react.ts";
import { reports } from "../testing/reports.ts";
import type { OptionHost } from "../types/hosts.ts";
import type { TextOutput } from "../types/outputs.ts";
import { outputRenderer, reactModule } from "./index.ts";
import type { ModuleProps, OutputProps } from "./index.ts";

const text = (value: string): TextOutput => ({
  renderer: { id: "blockr.core-text_output", module: "blockr.core/js/basic-text.js" },
  text: value,
});

afterEach(() => {
  vi.restoreAllMocks();
});

describe("reactModule()", () => {
  test("draws its component with the host and the signal as core mounts it, and empties the element as core unmounts it", async () => {
    const seen: ModuleProps<OptionHost>[] = [];
    function Rows(props: ModuleProps<OptionHost>) {
      seen.push(props);
      return <p>{JSON.stringify(props.host.value())}</p>;
    }
    const mod = reactModule(Rows, { options: ["n_rows"] });
    const host = stubOptionHost({ option: "n_rows", value: 50 });

    const mounted = await mountReact({ api: 1, default: mod }, host);

    expect(mod.options).toEqual(["n_rows"]);
    expect(mounted.el.textContent).toBe("50");
    expect(seen[0]?.host).toBe(host);
    expect(seen[0]?.signal).toBe(mounted.signal);

    await step(() => {
      mounted.unmount();
    });
    expect(mounted.el.childNodes).toHaveLength(0);
  });

  test("names no board options where none are given", () => {
    expect("options" in reactModule(() => null)).toBe(false);
  });
});

describe("outputRenderer()", () => {
  test("hands its component the output, its parameters and the board option values, and draws again as the host announces another output", async () => {
    const seen: OutputProps<TextOutput>[] = [];
    const renderer = outputRenderer<TextOutput>((props) => {
      seen.push(props);
      return <pre>{props.output.text}</pre>;
    });
    const host = stubOutputHost({ output: { ...text("one") }, params: { page: 1 }, options: { n_rows: 5 } });

    const { el } = await mountReact({ api: 1, default: renderer }, host);
    expect(el.querySelector("pre")?.textContent).toBe("one");
    expect(seen.at(-1)).toMatchObject({ params: { page: 1 }, options: { n_rows: 5 }, body: null, loading: false, error: null, host });

    await step(() => {
      host.setOutput({ ...text("two") });
    });
    expect(el.querySelector("pre")?.textContent).toBe("two");
  });

  test("marks itself busy and dims what its component drew while a newer output loads, and shows why the latest ask failed over it", async () => {
    const renderer = outputRenderer<TextOutput>(({ output }) => <pre>{output.text}</pre>);
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

    await step(() => {
      host.setError({ severity: "error", class: ["simpleError", "error", "condition"], message: "boom" });
    });
    expect(el.querySelector('[role="alert"]')?.textContent).toBe("boom");
    expect(el.querySelector("pre")?.textContent).toBe("one");
  });

  test("shows the error of a component that throws in its place, and draws it again once another output comes", async () => {
    vi.spyOn(console, "error").mockImplementation(() => undefined);
    const renderer = outputRenderer<TextOutput>(({ output }) => {
      if (output.text === "bad") throw new Error("no text to show");
      return <pre>{output.text}</pre>;
    });
    const host = stubOutputHost({ output: { ...text("bad") } });
    const reported = reports();
    let errors: unknown[];
    try {
      const { el } = await mountReact({ api: 1, default: renderer }, host);
      expect(el.querySelector('[role="alert"]')?.textContent).toBe("The renderer failed to draw the output: no text to show");
      expect(el.querySelector("pre")).toBeNull();

      await step(() => {
        host.setOutput({ ...text("good") });
      });
      expect(el.querySelector('[role="alert"]')).toBeNull();
      expect(el.querySelector("pre")?.textContent).toBe("good");
    } finally {
      reported.restore();
      errors = reported.errors();
    }
    // What the capture holds besides the reported error are React's own
    // microtasks, which throw nothing.
    expect(errors.filter((error) => error instanceof Error).map((error) => error.message)).toEqual(["no text to show"]);
  });
});
