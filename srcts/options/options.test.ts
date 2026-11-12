// Core's board options, n_rows, show_conditions and dark_mode: what each
// defines, the module of each, the default each reads from its blockr option,
// what dark_mode does on the page, and their UI modules, mounted with the stub
// of their host.

import { afterEach, describe, expect, test, vi } from "vitest";
import type { Option } from "../store/index.ts";
import { createStore } from "../store/index.ts";
import { PAUSE } from "../binding/index.ts";
// The modules of core's board options, which the build bundles on their own.
// eslint-disable-next-line no-restricted-imports
import darkModeModule, * as darkModeExports from "../modules/dark-mode-option/index.ts";
// eslint-disable-next-line no-restricted-imports
import nRowsModule, * as nRowsExports from "../modules/n-rows-option/index.ts";
// eslint-disable-next-line no-restricted-imports
import showConditionsModule, * as showConditionsExports from "../modules/show-conditions-option/index.ts";
import { stubOptionHost } from "../stubs/index.ts";
import { find, pick, texts, toggle, type } from "../testing/inputs.ts";
import { mountReact, step } from "../testing/react.ts";
import type { CoreApi } from "../types/core-api.ts";
import type { Json } from "../types/json.ts";
import { coreOptions, darkMode, nRows, optionSchemas, setTheme, showConditions } from "./index.ts";

// CoreApi with its blockrOption(), with the values that R holds, which records
// what it is asked for.
function blockr(values: Readonly<Record<string, Json>> = {}) {
  const asked: string[] = [];
  const core = {
    blockrOption(name: string): Promise<Json | undefined> {
      asked.push(name);
      return Promise.resolve(Object.hasOwn(values, name) ? values[name] : undefined);
    },
  } as CoreApi;
  return { core, asked };
}

// The value that the board option's default resolves with, as core calls it
// with CoreApi.
function resolveDefault(option: Omit<Option, "id">, core: CoreApi): Promise<Json> {
  const given = option.default;
  return typeof given === "function" ? given(core) : Promise.resolve(given);
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
  document.documentElement.removeAttribute("data-bc-theme");
});

describe("core's board options", () => {
  test("are defined as today's R constructors define them, with the schemas that R writes values by", () => {
    const options = Object.entries(coreOptions);

    expect(options.map(([id, { label, category }]) => ({ id, label, category }))).toEqual([
      { id: "n_rows", label: "Preview rows", category: "Table options" },
      { id: "show_conditions", label: "Message types to display", category: "Board options" },
      { id: "dark_mode", label: "Light/dark mode", category: "Theme options" },
    ]);
    expect(Object.fromEntries(options.map(([id, { schema }]) => [id, schema]))).toEqual(optionSchemas);
    for (const [, { schema }] of options) expect(schema.description).toEqual(expect.any(String));
  });

  test("are each the default export of a module of its own", () => {
    const modules = [
      [nRowsExports, nRowsModule, nRows],
      [showConditionsExports, showConditionsModule, showConditions],
      [darkModeExports, darkModeModule, darkMode],
    ] as const;

    for (const [exports, def, option] of modules) {
      expect(exports.api).toBe(1);
      expect(def).toBe(option);
      // The module gives no id, which R names, and which core adds as it
      // registers the board option.
      expect("id" in def).toBe(false);
    }
  });

  test("register with the store under the ids that R gives them, which reads no default as they do", () => {
    const { asked } = blockr();
    const store = createStore();

    for (const [id, def] of Object.entries(coreOptions)) store.options.register({ ...def, id });

    expect(Object.keys(store.options.registered())).toEqual(["n_rows", "show_conditions", "dark_mode"]);
    expect(asked).toEqual([]);
  });

  test("take as their defaults the blockr options of their names, else their fallbacks", async () => {
    prefers("light");
    const { core, asked } = blockr();

    expect(await resolveDefault(nRows, core)).toBe(10);
    expect(await resolveDefault(showConditions, core)).toEqual(["message", "warning", "error"]);
    expect(await resolveDefault(darkMode, core)).toBe("light");
    expect(asked).toEqual(["n_rows", "show_conditions", "dark_mode"]);

    const set = blockr({ n_rows: 25, show_conditions: ["message", "error"], dark_mode: "dark" }).core;
    expect(await resolveDefault(nRows, set)).toBe(25);
    expect(await resolveDefault(showConditions, set)).toEqual(["message", "error"]);
    expect(await resolveDefault(darkMode, set)).toBe("dark");
  });

  test("read a single severity as R writes it, and TRUE and FALSE for dark and light, as today's R constructors do", async () => {
    const { core } = blockr({ show_conditions: "error", dark_mode: true });
    expect(await resolveDefault(showConditions, core)).toEqual(["error"]);
    expect(await resolveDefault(darkMode, core)).toBe("dark");
    expect(await resolveDefault(darkMode, blockr({ dark_mode: false }).core)).toBe("light");
  });

  test("read the string that an environment variable sets as the number or the logical that it holds", async () => {
    const { core } = blockr({ n_rows: " 25 ", dark_mode: "TRUE" });
    expect(await resolveDefault(nRows, core)).toBe(25);
    expect(await resolveDefault(darkMode, core)).toBe("dark");
    expect(await resolveDefault(darkMode, blockr({ dark_mode: "F" }).core)).toBe("light");
    expect(await resolveDefault(darkMode, blockr({ dark_mode: "light" }).core)).toBe("light");
  });

  test("take the theme the user's system prefers where no blockr option sets dark_mode, as today's NULL does", async () => {
    prefers("dark");
    expect(await resolveDefault(darkMode, blockr().core)).toBe("dark");
    prefers("light");
    expect(await resolveDefault(darkMode, blockr().core)).toBe("light");
  });

  test("fail where a blockr option holds a value that the schema refuses", async () => {
    prefers("dark");
    const { core } = blockr({ n_rows: 0, show_conditions: ["fatal"], dark_mode: "blue" });

    await expect(resolveDefault(nRows, core)).rejects.toThrow(
      "The blockr option n_rows is 0, which the board option n_rows does not take.",
    );
    await expect(resolveDefault(showConditions, core)).rejects.toThrow(
      'The blockr option show_conditions is ["fatal"], which the board option show_conditions does not take.',
    );
    await expect(resolveDefault(darkMode, core)).rejects.toThrow(
      'The blockr option dark_mode is "blue", which the board option dark_mode does not take.',
    );

    const strings = blockr({ n_rows: "many", dark_mode: "yes" }).core;
    await expect(resolveDefault(nRows, strings)).rejects.toThrow(
      'The blockr option n_rows is "many", which the board option n_rows does not take.',
    );
    await expect(resolveDefault(nRows, blockr({ n_rows: "Inf" }).core)).rejects.toThrow(
      'The blockr option n_rows is "Inf", which the board option n_rows does not take.',
    );
    await expect(resolveDefault(darkMode, strings)).rejects.toThrow(
      'The blockr option dark_mode is "yes", which the board option dark_mode does not take.',
    );
    await expect(resolveDefault(darkMode, blockr({ dark_mode: null }).core)).rejects.toThrow(
      "The blockr option dark_mode is null, which the board option dark_mode does not take.",
    );
  });
});

describe("dark_mode", () => {
  test("switches the page's theme as its value changes, in an attribute of core's own", () => {
    const core = {} as Parameters<NonNullable<Option["onChange"]>>[1];

    darkMode.onChange?.("dark", core);
    expect(document.documentElement.getAttribute("data-bc-theme")).toBe("dark");
    darkMode.onChange?.("light", core);
    expect(document.documentElement.getAttribute("data-bc-theme")).toBe("light");

    setTheme("dark");
    setTheme(null);
    expect(document.documentElement.getAttribute("data-bc-theme")).toBe("light");
  });

  test("leaves the others without anything to do on the page", () => {
    expect("onChange" in nRows).toBe(false);
    expect("onChange" in showConditions).toBe(false);
  });
});

describe("the UIs of core's board options", () => {
  // Mounts a board option's UI module as core mounts it, with the stub of its
  // host holding the value.
  async function mounted<V extends Json>(id: string, option: Omit<Option, "id">, value: V) {
    const host = stubOptionHost<V>({ option: id, value });
    const { el } = await mountReact({ api: 1, default: option.ui }, host);
    return { el, host };
  }

  test("n_rows is a number input under its label that submits a whole number of rows from one, once the user pauses", async () => {
    vi.useFakeTimers();
    const { el, host } = await mounted("n_rows", nRows, 50);
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
    const { el, host } = await mounted("show_conditions", showConditions, ["error"]);
    expect(el.querySelector("legend")?.textContent).toBe("Message types to display");
    expect(texts(el, "label")).toEqual(["message", "warning", "error"]);

    await step(() => {
      toggle(find(el, 'input[value="message"]', HTMLInputElement));
    });

    expect(host.submitted).toEqual([["message", "error"]]);
  });

  test("dark_mode is a select of light and dark that submits the user's choice", async () => {
    const { el, host } = await mounted("dark_mode", darkMode, "light");
    const select = find(el, "select", HTMLSelectElement);
    expect(el.querySelector("label")?.textContent).toContain("Light/dark mode");
    expect(select.value).toBe("light");

    await step(() => {
      pick(select, "dark");
    });

    expect(host.submitted).toEqual(["dark"]);
  });
});
