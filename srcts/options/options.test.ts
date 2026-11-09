// Core's board options, n_rows, show_conditions and dark_mode: what each
// defines, the default each reads from its blockr option, what dark_mode does
// on the page, and their UI modules, mounted with the stub of their host.

import { afterEach, describe, expect, test, vi } from "vitest";
import type { Option } from "../store/index.ts";
import { createStore } from "../store/index.ts";
import { PAUSE } from "../binding/index.ts";
import { stubOptionHost } from "../stubs/index.ts";
import { find, pick, texts, toggle, type } from "../testing/inputs.ts";
import { mountReact, step } from "../testing/react.ts";
import type { Json } from "../types/json.ts";
import { coreOptions, darkMode, nRows, optionSchemas, setTheme, showConditions } from "./index.ts";
import type { BlockrOptions } from "./index.ts";

// CoreApi's blockrOption(), with the values that R holds, which records what
// it is asked for.
function blockr(values: Readonly<Record<string, Json>> = {}) {
  const asked: string[] = [];
  const core: BlockrOptions = {
    blockrOption(name: string): Promise<Json | undefined> {
      asked.push(name);
      return Promise.resolve(Object.hasOwn(values, name) ? values[name] : undefined);
    },
  };
  return { core, asked };
}

// The value that the board option's default resolves with.
function defaultOf(option: Option): Promise<Json> {
  const given = option.default;
  return typeof given === "function" ? given() : Promise.resolve(given);
}

// The user's system, as preferring a dark theme or not.
function prefers(theme: "light" | "dark"): void {
  vi.stubGlobal("matchMedia", (query: string) => ({
    matches: query === "(prefers-color-scheme: dark)" && theme === "dark",
  }));
}

afterEach(() => {
  vi.unstubAllGlobals();
  vi.useRealTimers();
  document.documentElement.removeAttribute("data-bs-theme");
});

describe("core's board options", () => {
  test("are defined as today's R constructors define them, with the schemas that R writes values by", () => {
    const options = coreOptions(blockr().core);

    expect(options.map(({ id, label, category }) => ({ id, label, category }))).toEqual([
      { id: "n_rows", label: "Preview rows", category: "Table options" },
      { id: "show_conditions", label: "Message types to display", category: "Board options" },
      { id: "dark_mode", label: "Light/dark mode", category: "Theme options" },
    ]);
    expect(Object.fromEntries(options.map(({ id, schema }) => [id, schema]))).toEqual(optionSchemas);
    for (const { schema } of options) expect(schema.description).toEqual(expect.any(String));
  });

  test("register with the store, which reads no default as they do", () => {
    const { core, asked } = blockr();
    const store = createStore();

    for (const option of coreOptions(core)) store.options.register(option);

    expect(Object.keys(store.options.registered())).toEqual(["n_rows", "show_conditions", "dark_mode"]);
    expect(asked).toEqual([]);
  });

  test("take as their defaults the blockr options of their names, else their fallbacks", async () => {
    prefers("light");
    const { core, asked } = blockr();

    expect(await defaultOf(nRows(core))).toBe(10);
    expect(await defaultOf(showConditions(core))).toEqual(["warning", "error"]);
    expect(await defaultOf(darkMode(core))).toBe("light");
    expect(asked).toEqual(["n_rows", "show_conditions", "dark_mode"]);

    const set = blockr({ n_rows: 25, show_conditions: ["message", "error"], dark_mode: "dark" }).core;
    expect(await defaultOf(nRows(set))).toBe(25);
    expect(await defaultOf(showConditions(set))).toEqual(["message", "error"]);
    expect(await defaultOf(darkMode(set))).toBe("dark");
  });

  test("read a single severity as R writes it, and TRUE and FALSE for dark and light, as today's R constructors do", async () => {
    const { core } = blockr({ show_conditions: "error", dark_mode: true });
    expect(await defaultOf(showConditions(core))).toEqual(["error"]);
    expect(await defaultOf(darkMode(core))).toBe("dark");
    expect(await defaultOf(darkMode(blockr({ dark_mode: false }).core))).toBe("light");
  });

  test("read the string that an environment variable sets as R's as.numeric() and as.logical() read it", async () => {
    const { core } = blockr({ n_rows: " 25 ", dark_mode: "TRUE" });
    expect(await defaultOf(nRows(core))).toBe(25);
    expect(await defaultOf(darkMode(core))).toBe("dark");
    expect(await defaultOf(darkMode(blockr({ dark_mode: "F" }).core))).toBe("light");
    expect(await defaultOf(darkMode(blockr({ dark_mode: "light" }).core))).toBe("light");
  });

  test("take the theme the user's system prefers where no blockr option sets dark_mode, as today's NULL does", async () => {
    prefers("dark");
    expect(await defaultOf(darkMode(blockr().core))).toBe("dark");
    prefers("light");
    expect(await defaultOf(darkMode(blockr().core))).toBe("light");
  });

  test("fail where a blockr option holds a value that the schema refuses", async () => {
    prefers("dark");
    const { core } = blockr({ n_rows: 0, show_conditions: ["fatal"], dark_mode: "blue" });

    await expect(defaultOf(nRows(core))).rejects.toThrow(
      "The blockr option n_rows is 0, which the board option n_rows does not take.",
    );
    await expect(defaultOf(showConditions(core))).rejects.toThrow(
      'The blockr option show_conditions is ["fatal"], which the board option show_conditions does not take.',
    );
    await expect(defaultOf(darkMode(core))).rejects.toThrow(
      'The blockr option dark_mode is "blue", which the board option dark_mode does not take.',
    );

    const strings = blockr({ n_rows: "many", dark_mode: " TRUE" }).core;
    await expect(defaultOf(nRows(strings))).rejects.toThrow(
      'The blockr option n_rows is "many", which the board option n_rows does not take.',
    );
    await expect(defaultOf(nRows(blockr({ n_rows: "Inf" }).core))).rejects.toThrow(
      'The blockr option n_rows is "Inf", which the board option n_rows does not take.',
    );
    await expect(defaultOf(darkMode(strings))).rejects.toThrow(
      'The blockr option dark_mode is " TRUE", which the board option dark_mode does not take.',
    );
    await expect(defaultOf(darkMode(blockr({ dark_mode: null }).core))).rejects.toThrow(
      "The blockr option dark_mode is null, which the board option dark_mode does not take.",
    );
  });
});

describe("dark_mode", () => {
  test("switches the page's theme as its value changes, in the attribute that Bootstrap and blockr.ui read", () => {
    const option = darkMode(blockr().core);
    const core = {} as Parameters<NonNullable<Option["onChange"]>>[1];

    option.onChange?.("dark", core);
    expect(document.documentElement.getAttribute("data-bs-theme")).toBe("dark");
    option.onChange?.("light", core);
    expect(document.documentElement.getAttribute("data-bs-theme")).toBe("light");

    setTheme("dark");
    setTheme(null);
    expect(document.documentElement.getAttribute("data-bs-theme")).toBe("light");
  });

  test("leaves the others without anything to do on the page", () => {
    const { core } = blockr();
    expect("onChange" in nRows(core)).toBe(false);
    expect("onChange" in showConditions(core)).toBe(false);
  });
});

describe("the UIs of core's board options", () => {
  // Mounts a board option's UI module as core mounts it, with the stub of its
  // host holding the value.
  async function mounted<V extends Json>(option: Option, value: V) {
    const host = stubOptionHost<V>({ option: option.id, value });
    const { el } = await mountReact({ api: 1, default: option.ui }, host);
    return { el, host };
  }

  test("n_rows is a number input under its label that submits a whole number of rows from one, once the user pauses", async () => {
    vi.useFakeTimers();
    const { el, host } = await mounted(nRows(blockr().core), 50);
    const input = find(el, "input", HTMLInputElement);
    expect(el.querySelector("label")?.textContent).toContain("Preview rows");
    expect([input.type, input.min, input.value]).toEqual(["number", "1", "50"]);

    await step(() => {
      type(input, "0");
    });
    await step(() => {
      vi.advanceTimersByTime(PAUSE);
    });
    await step(() => {
      type(input, "25");
    });
    await step(() => {
      vi.advanceTimersByTime(PAUSE);
    });

    expect(host.submitted).toEqual([25]);
  });

  test("show_conditions is a checkbox for each severity that submits the checked ones in their order", async () => {
    const { el, host } = await mounted(showConditions(blockr().core), ["error"]);
    expect(el.querySelector("legend")?.textContent).toBe("Message types to display");
    expect(texts(el, "label")).toEqual(["message", "warning", "error"]);

    await step(() => {
      toggle(find(el, 'input[value="message"]', HTMLInputElement));
    });

    expect(host.submitted).toEqual([["message", "error"]]);
  });

  test("dark_mode is a select of light and dark that submits the user's choice", async () => {
    const { el, host } = await mounted(darkMode(blockr().core), "light");
    const select = find(el, "select", HTMLSelectElement);
    expect(el.querySelector("label")?.textContent).toContain("Light/dark mode");
    expect(select.value).toBe("light");

    await step(() => {
      pick(select, "dark");
    });

    expect(host.submitted).toEqual(["dark"]);
  });
});
