// Core's board options, n_rows, show_conditions and dark_mode: what each
// defines, the default each reads from its blockr option, what dark_mode does
// on the page, and their UI modules, mounted with the stub of their host.

import { afterEach, describe, expect, test, vi } from "vitest";
import type { Option } from "../store/index.ts";
import { createStore } from "../store/index.ts";
import { mount, stubOptionHost } from "../stubs/index.ts";
import { fakeR } from "../testing/store.ts";
import { reports } from "../testing/reports.ts";
import type { Json } from "../types/json.ts";
import { PAUSE } from "./inputs.ts";
import { coreOptions, darkMode, nRows, optionSchemas, setTheme, showConditions } from "./index.ts";
import type { BlockrOptions } from "./index.ts";

// CoreApi's blockrOption(), with the values that R holds, which records what
// it is asked for.
function blockr(values: Readonly<Record<string, Json>> = {}) {
  const asked: { name: string; fallback: Json }[] = [];
  const core: BlockrOptions = {
    blockrOption<T extends Json>(name: string, fallback: T): Promise<T> {
      asked.push({ name, fallback });
      return Promise.resolve(Object.hasOwn(values, name) ? (values[name] as T) : fallback);
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
    const store = createStore({ client: fakeR() });

    for (const option of coreOptions(core)) store.options.register(option);

    expect(Object.keys(store.options.registered())).toEqual(["n_rows", "show_conditions", "dark_mode"]);
    expect(asked).toEqual([]);
  });

  test("take as their defaults the blockr options of their names, else today's fallbacks", async () => {
    prefers("light");
    const { core, asked } = blockr();

    expect(await defaultOf(nRows(core))).toBe(50);
    expect(await defaultOf(showConditions(core))).toEqual(["warning", "error"]);
    expect(await defaultOf(darkMode(core))).toBe("light");
    expect(asked).toEqual([
      { name: "n_rows", fallback: 50 },
      { name: "show_conditions", fallback: ["warning", "error"] },
      { name: "dark_mode", fallback: null },
    ]);

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

  test("take the theme the user's system prefers where no blockr option sets dark_mode, as today's NULL does", async () => {
    prefers("dark");
    expect(await defaultOf(darkMode(blockr().core))).toBe("dark");
    prefers("light");
    expect(await defaultOf(darkMode(blockr().core))).toBe("light");
  });

  test("report a blockr option that the schema refuses and take the fallback instead", async () => {
    prefers("dark");
    const { core } = blockr({ n_rows: 0, show_conditions: ["fatal"], dark_mode: "blue" });
    const reported = reports();
    let defaults: Json[];
    try {
      defaults = [
        await defaultOf(nRows(core)),
        await defaultOf(showConditions(core)),
        await defaultOf(darkMode(core)),
      ];
    } finally {
      reported.restore();
    }

    expect(defaults).toEqual([50, ["warning", "error"], "dark"]);
    expect(reported.errors().map((error) => (error as Error).message)).toEqual([
      "The blockr option n_rows is 0, which the board option n_rows does not take, so its default is 50.",
      'The blockr option show_conditions is ["fatal"], which the board option show_conditions does not take, so its default is ["warning","error"].',
      'The blockr option dark_mode is "blue", which the board option dark_mode does not take, so its default is "dark".',
    ]);
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

describe("the UI of n_rows", () => {
  async function field(value: number) {
    const host = stubOptionHost<number>({ option: "n_rows", value });
    const mounted = await mount({ api: 1, default: nRows(blockr().core).ui }, host);
    const input = mounted.el.querySelector("input");
    if (input === null) throw new Error("The UI draws no input.");
    const type = (text: string): void => {
      input.value = text;
      input.dispatchEvent(new Event("input", { bubbles: true }));
    };
    return {
      el: mounted.el,
      host,
      input,
      type,
      unmount: () => {
        mounted.unmount();
      },
    };
  }

  test("shows the value under its label and takes a whole number of rows from one", async () => {
    const { el, input } = await field(50);
    expect(el.querySelector("label")?.textContent).toBe("Preview rows");
    expect(input.type).toBe("number");
    expect([input.min, input.step]).toEqual(["1", "1"]);
    expect(input.value).toBe("50");
  });

  test("submits what the user types once they pause, as one edit", async () => {
    vi.useFakeTimers();
    const { host, input, type } = await field(50);

    type("2");
    type("25");
    vi.advanceTimersByTime(PAUSE - 1);
    expect(host.submitted).toEqual([]);
    vi.advanceTimersByTime(1);

    expect(host.submitted).toEqual([25]);
    expect(input.value).toBe("25");
  });

  test("submits at once as the user presses Enter or leaves the field, but not a value it holds already", async () => {
    vi.useFakeTimers();
    const { host, input, type } = await field(50);

    type("10");
    input.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter" }));
    type("20");
    input.dispatchEvent(new Event("blur"));
    type("20");
    input.dispatchEvent(new Event("blur"));
    vi.runAllTimers();

    expect(host.submitted).toEqual([10, 20]);
  });

  test("submits no count that the board option does not take, and shows the value again once the user leaves", async () => {
    vi.useFakeTimers();
    const { host, input, type } = await field(50);

    for (const text of ["0", "2.5", "", "-3"]) {
      type(text);
      vi.advanceTimersByTime(PAUSE);
      expect(input.getAttribute("aria-invalid")).toBe("true");
    }
    input.dispatchEvent(new Event("blur"));

    expect(host.submitted).toEqual([]);
    expect(input.value).toBe("50");
    expect(input.hasAttribute("aria-invalid")).toBe(false);
  });

  test("takes a value it did not set, but leaves what the user is typing", async () => {
    vi.useFakeTimers();
    const { host, input, type } = await field(50);

    host.setValue(30);
    expect(input.value).toBe("30");

    type("40");
    host.setValue(60);
    expect(input.value).toBe("40");
    vi.advanceTimersByTime(PAUSE);
    expect(host.submitted).toEqual([40]);

    host.setValue(70);
    expect(input.value).toBe("70");
  });

  test("submits an edit still pending as core unmounts it", async () => {
    vi.useFakeTimers();
    const { host, type, unmount } = await field(50);

    type("15");
    unmount();
    vi.runAllTimers();

    expect(host.submitted).toEqual([15]);
  });

  test("shows the value again where the submit is refused", async () => {
    vi.useFakeTimers();
    const { host, input, type } = await field(50);
    const reported = reports();
    host.submit = () => Promise.reject(new Error("refused"));

    type("15");
    vi.advanceTimersByTime(PAUSE);
    await vi.runAllTimersAsync();
    reported.restore();

    expect(input.value).toBe("50");
    expect(reported.errors().map((error) => (error as Error).message)).toEqual(["refused"]);
  });
});

describe("the UI of show_conditions", () => {
  async function boxes(value: string[]) {
    const host = stubOptionHost<string[]>({ option: "show_conditions", value });
    const mounted = await mount({ api: 1, default: showConditions(blockr().core).ui }, host);
    const inputs = [...mounted.el.querySelectorAll("input")];
    const checked = (): string[] => inputs.filter((box) => box.checked).map((box) => box.value);
    const click = (severity: string): void => {
      const box = inputs.find((input) => input.value === severity);
      if (box === undefined) throw new Error(`No box for ${severity}.`);
      box.checked = !box.checked;
      box.dispatchEvent(new Event("change", { bubbles: true }));
    };
    return { el: mounted.el, host, inputs, checked, click };
  }

  test("checks a box for each severity the value holds, under the board option's label", async () => {
    const { el, inputs, checked } = await boxes(["warning", "error"]);
    expect(el.querySelector("legend")?.textContent).toBe("Message types to display");
    expect(inputs.map((box) => [box.type, box.value, box.parentElement?.textContent])).toEqual([
      ["checkbox", "message", "Messages"],
      ["checkbox", "warning", "Warnings"],
      ["checkbox", "error", "Errors"],
    ]);
    expect(checked()).toEqual(["warning", "error"]);
  });

  test("submits the severities checked, in their order, on each click, and takes a value it did not set", async () => {
    const { host, checked, click } = await boxes(["error"]);

    click("message");
    click("error");
    click("warning");

    expect(host.submitted).toEqual([["message", "error"], ["message"], ["message", "warning"]]);

    host.setValue([]);
    expect(checked()).toEqual([]);
  });
});

describe("the UI of dark_mode", () => {
  test("selects the theme the value names, submits the user's choice and takes a value it did not set", async () => {
    const host = stubOptionHost<string>({ option: "dark_mode", value: "light" });
    const { el } = await mount({ api: 1, default: darkMode(blockr().core).ui }, host);
    const select = el.querySelector("select");
    if (select === null) throw new Error("The UI draws no select.");

    expect(el.querySelector("label span")?.textContent).toBe("Light/dark mode");
    expect([...select.options].map((option) => [option.value, option.textContent])).toEqual([
      ["light", "Light"],
      ["dark", "Dark"],
    ]);
    expect(select.value).toBe("light");

    select.value = "dark";
    select.dispatchEvent(new Event("change", { bubbles: true }));
    expect(host.submitted).toEqual(["dark"]);

    host.setValue("light");
    expect(select.value).toBe("light");
  });
});
