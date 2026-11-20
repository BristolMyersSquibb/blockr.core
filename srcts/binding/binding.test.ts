// The binding of a control to one value, a block's state arg or a board
// option's value, and requests that follow what they read, on the stubs of
// both hosts.

import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { READY, stubBlockHost, stubOptionHost } from "../stubs/index.ts";
import type { BlockFixture } from "../stubs/index.ts";
import { holdRequests } from "../testing/requests.ts";
import type { Json } from "../types/json.ts";
import { bind, follow, PAUSE } from "./index.ts";

// A block whose fixture lists four states, of two `columns` and two labels.
const fixture: BlockFixture = {
  states: [
    { columns: ["a"], label: "x" },
    { columns: ["a", "b"], label: "x" },
    { columns: ["a"], label: "y" },
    { columns: ["a", "b"], label: "y" },
  ],
  requests: [{ name: "names", args: {}, options: {}, answer: ["a", "b", "c"] }],
};

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
});

describe("a binding", () => {
  test("shows the value now and once it changes, and commits what the user picks", async () => {
    const host = stubBlockHost(fixture);
    const shown: (Json | undefined)[] = [];
    const columns = bind(host, "columns", (value) => shown.push(value));

    host.setState({ columns: ["a"], label: "y" });
    expect(shown).toEqual([["a"]]);

    await expect(columns.commit(["a", "b"])).resolves.toBe(true);
    await expect(columns.commit(["a", "b"])).resolves.toBe(true);
    expect(host.submitted).toEqual([{ columns: ["a", "b"], label: "y" }]);
    expect(shown).toEqual([["a"], ["a", "b"]]);
  });

  test("hears why the host refused a commit, and shows the value the host holds again", async () => {
    const host = stubBlockHost(fixture);
    const shown: (Json | undefined)[] = [];
    const settled: unknown[] = [];
    const columns = bind(host, "columns", (value) => shown.push(value), { settled: (error) => settled.push(error) });

    await expect(columns.commit(["c"])).resolves.toBe(false);
    expect(String(settled[0])).toContain("which the fixture does not list");
    expect(shown).toEqual([["a"], ["a"]]);

    await columns.commit(["a", "b"]);
    expect(settled[1]).toBeNull();
  });

  test("passes its notifier to each submit of the host, an unset among them, and none where it was given none", async () => {
    const host = stubBlockHost(fixture);
    const submit = vi.spyOn(host, "submit");
    const notifier = (): void => undefined;
    const columns = bind(host, "columns", () => undefined, { notifier });
    const label = bind(host, "label", () => undefined);

    await columns.commit(["a", "b"]);
    await columns.unset();
    await label.commit("y");

    expect(submit.mock.calls).toEqual([
      [{ columns: ["a", "b"] }, { notifier }],
      [{ columns: undefined }, { notifier }],
      [{ label: "y" }, {}],
    ]);

    const option = stubOptionHost({ option: "n_rows", value: 50 });
    const submitOption = vi.spyOn(option, "submit");
    await bind(option, "n_rows", () => undefined, { notifier }).commit(10);
    expect(submitOption.mock.calls).toEqual([[10, { notifier }]]);
  });

  test("submits what the user typed once the user pauses, and shows nothing the host announces until then", async () => {
    const host = stubBlockHost(fixture);
    const shown: (Json | undefined)[] = [];
    const label = bind(host, "label", (value) => shown.push(value));

    label.type("y");
    host.setState({ columns: ["a", "b"], label: "x" });
    expect(shown).toEqual(["x"]);

    await vi.advanceTimersByTimeAsync(PAUSE);
    expect(host.submitted).toEqual([{ columns: ["a", "b"], label: "y" }]);
    expect(shown).toEqual(["x", "y"]);
  });

  test("submits nothing where the user typed no value, and what is pending as it ends", async () => {
    const host = stubBlockHost(fixture);
    const shown: (Json | undefined)[] = [];
    const label = bind(host, "label", (value) => shown.push(value));

    label.type(undefined);
    await vi.advanceTimersByTimeAsync(PAUSE);
    expect(host.submitted).toEqual([]);

    label.type("y");
    label.release();
    expect(host.submitted).toEqual([{ columns: ["a"], label: "y" }]);

    host.setState({ columns: ["a"], label: "x" });
    expect(shown).toEqual(["x"]);
  });

  test("unsets the state arg at once, unless the host holds none, and refuses to unset a board option's value", async () => {
    const host = stubBlockHost({ states: [{ columns: ["a"], label: "x" }, { label: "x" }], requests: [] }, { state: { columns: ["a"], label: "x" } });
    const shown: (Json | undefined)[] = [];
    const columns = bind(host, "columns", (value) => shown.push(value));

    expect(await columns.unset()).toBe(true);
    expect(await columns.unset()).toBe(true);
    expect([host.submitted, shown]).toEqual([[{ label: "x" }], [["a"], undefined]]);

    const option = stubOptionHost<number>({ option: "n_rows", value: 25 });
    const refused: unknown[] = [];
    const rows = bind(option, "n_rows", () => undefined, { settled: (error) => refused.push(error) });
    expect(await rows.unset()).toBe(false);
    expect(String(refused[0])).toContain('The board option "n_rows" always holds a value');
  });

  test("binds a board option's value under its id, and nothing else", async () => {
    const host = stubOptionHost<number>({ option: "n_rows", value: 25 });
    const shown: (Json | undefined)[] = [];
    const rows = bind(host, "n_rows", (value) => shown.push(value));

    rows.type(50);
    await vi.advanceTimersByTimeAsync(PAUSE);
    host.setValue(10);

    expect(host.submitted).toEqual([50]);
    expect(shown).toEqual([25, 50, 10]);
    expect(() => bind(host, "dark_mode", () => undefined)).toThrow(
      'The host of the board option "n_rows" holds no value "dark_mode".',
    );
  });
});

// A block whose state args all_x and all_y make one of two joins, which a
// control picks by its name, and whose state arg `conditions` holds a list,
// the first of which a control edits.
type Joined = Readonly<{ all_x: boolean; all_y: boolean; conditions: readonly string[] }>;

const joined = (state: Partial<Joined>): Joined => ({ all_x: false, all_y: false, conditions: ["a", "b"], ...state });

const joinFixture: BlockFixture = {
  states: [
    joined({}),
    joined({ all_x: true }),
    joined({ conditions: ["a", "b", "c"] }),
    joined({ conditions: ["a2", "b", "c"] }),
    joined({ conditions: ["b"] }),
  ],
  requests: [],
};

const joins = [
  { name: "inner", args: { all_x: false, all_y: false } },
  { name: "left", args: { all_x: true, all_y: false } },
] as const;

// The name of the join that the state args make, and the state args of the
// join that a name gives, or none for a name of no join.
const join = (state: Joined): Json | undefined =>
  joins.find(({ args }) => args.all_x === state.all_x && args.all_y === state.all_y)?.name;
const joinArgs = (name: Json | undefined): Partial<Joined> => joins.find((join) => join.name === name)?.args ?? {};

// The first condition, and the list with the first condition replaced, or
// without it given undefined.
const first = (state: Joined): Json | undefined => state.conditions[0];
const withFirst = (value: Json | undefined, state: Joined): Partial<Joined> => ({
  conditions: value === undefined ? state.conditions.slice(1) : [value as string, ...state.conditions.slice(1)],
});

describe("a binding made with two functions", () => {
  test("shows the value that the first reads, and commits the change that the second builds, of several state args in one submit", async () => {
    const host = stubBlockHost<Joined>(joinFixture);
    const submit = vi.spyOn(host, "submit");
    const shown: (Json | undefined)[] = [];
    const notifier = (): void => undefined;
    const picked = bind(host, join, joinArgs, (value) => shown.push(value), { notifier });

    await expect(picked.commit("left")).resolves.toBe(true);
    expect(picked.get()).toBe("left");
    await expect(picked.commit("left")).resolves.toBe(true);
    host.setState(joined({}));

    expect(submit.mock.calls).toEqual([[{ all_x: true, all_y: false }, { notifier }]]);
    expect(shown).toEqual(["inner", "left", "inner"]);
  });

  test("builds the change of what the user typed once the user pauses, from the state as it is then", async () => {
    const host = stubBlockHost<Joined>(joinFixture);
    const shown: (Json | undefined)[] = [];
    const condition = bind(host, first, withFirst, (value) => shown.push(value));

    condition.type("a2");
    // The assistant adds a condition before the pause ends.
    host.setState(joined({ conditions: ["a", "b", "c"] }));
    await vi.advanceTimersByTimeAsync(PAUSE);

    expect(host.submitted).toEqual([joined({ conditions: ["a2", "b", "c"] })]);
    expect(shown).toEqual(["a", "a2"]);
  });

  test("hears why the host refused a change, or why the second failed to build it, and shows the value the host holds again", async () => {
    const host = stubBlockHost<Joined>(joinFixture);
    const shown: (Json | undefined)[] = [];
    const settled: unknown[] = [];
    const failing = (): Partial<Joined> => {
      throw new Error("No condition is first.");
    };
    const condition = bind(host, first, withFirst, (value) => shown.push(value), { settled: (error) => settled.push(error) });
    const broken = bind(host, first, failing, () => undefined, { settled: (error) => settled.push(error) });

    await expect(condition.commit("z")).resolves.toBe(false);
    await expect(broken.commit("a2")).resolves.toBe(false);

    expect(settled.map(String)).toEqual([expect.stringContaining("which the fixture does not list"), "Error: No condition is first."]);
    expect(shown).toEqual(["a", "a"]);
    expect(host.submitted).toEqual([joined({ conditions: ["z", "b"] })]);
  });

  test("raises why the second failed to build a change as a notification about the block where it has no notifier, and logs it, until a later change builds", async () => {
    const host = stubBlockHost<Joined>(joinFixture);
    let failing = true;
    const change = (value: Json | undefined, state: Joined): Partial<Joined> => {
      if (failing) throw new Error("No condition is first.");
      return withFirst(value, state);
    };
    const condition = bind(host, first, change, () => undefined);
    const shown = bind(host, first, change, () => undefined, { notifier: () => undefined });

    await expect(condition.commit("z")).resolves.toBe(false);
    await expect(condition.commit("y")).resolves.toBe(false);
    await expect(shown.commit("x")).resolves.toBe(false);
    expect(host.notified).toEqual([
      { severity: "error", code: "change_failed", message: "The control failed to build the change it submits: No condition is first." },
    ]);
    expect(host.logged.map((line) => [line.level, line.error?.message])).toEqual([
      ["error", "No condition is first."],
      ["error", "No condition is first."],
      ["error", "No condition is first."],
    ]);

    failing = false;
    await expect(condition.commit("a")).resolves.toBe(true);
    expect(host.notified).toEqual([]);
    expect(host.submitted).toEqual([]);
  });

  test("takes a second that builds the change, and binds no board option's value through functions", () => {
    const host = stubBlockHost<Joined>(joinFixture);
    const option = stubOptionHost<number>({ option: "n_rows", value: 25 });

    expect(() => {
      // @ts-expect-error: a state arg's name builds no change.
      bind(host, first, "conditions", () => undefined);
    }).toThrow("A binding made with a function that reads its value takes a second, which builds the change it submits.");
    expect(() => {
      // @ts-expect-error: a board option's value is bound by its id alone.
      bind(option, (value: Json) => value, (value: Json) => value, () => undefined);
    }).toThrow('The value of the board option "n_rows" is bound by its id, not through functions.');
  });

  test("unsets through the change that the second builds from undefined, and submits nothing where that change is held already", async () => {
    const host = stubBlockHost<Joined>(joinFixture);
    const condition = bind(host, first, withFirst, () => undefined);
    const picked = bind(host, join, joinArgs, () => undefined);

    await expect(condition.unset()).resolves.toBe(true);
    await expect(picked.unset()).resolves.toBe(true);

    expect(host.submitted).toEqual([joined({ conditions: ["b"] })]);
  });
});

describe("a followed request", () => {
  test("asks again once a block's linked data inputs change, and only then", async () => {
    const host = stubBlockHost(fixture);
    const answers: unknown[] = [];
    follow(host, "names", { answer: (answer) => answers.push(answer) });
    await vi.advanceTimersByTimeAsync(0);

    host.setContext({ status: { ...READY, status: "stale", revision: 2 } });
    host.setContext({ inputs: [{ input: "data", revision: 2 }] });
    await vi.advanceTimersByTimeAsync(0);

    expect(host.requested).toHaveLength(2);
    expect(answers).toEqual([
      ["a", "b", "c"],
      ["a", "b", "c"],
    ]);
  });

  test("asks a board option's request again once what it reads changes", async () => {
    const host = stubOptionHost<string>({
      option: "data_dir",
      value: "/data",
      requests: [
        { name: "list_dir", args: { path: "/data" }, answer: ["a"] },
        { name: "list_dir", args: { path: "/srv" }, answer: ["b"] },
      ],
    });
    const answers: unknown[] = [];
    follow(host, "list_dir", { args: () => ({ path: host.value() }), answer: (answer) => answers.push(answer) });
    await vi.advanceTimersByTimeAsync(0);

    host.setValue("/srv");
    await vi.advanceTimersByTimeAsync(0);

    expect(answers).toEqual([["a"], ["b"]]);
  });

  test("gives up the request in flight as it ends, and hands on no answer after", async () => {
    const host = stubBlockHost(fixture);
    const answers: unknown[] = [];
    const names = follow(host, "names", { answer: (answer) => answers.push(answer) });

    names.release();
    host.setContext({ inputs: [{ input: "data", revision: 2 }] });
    await vi.advanceTimersByTimeAsync(0);

    expect(answers).toEqual([]);
    expect(host.requested).toHaveLength(1);
  });

  test("says each time that it asks, before that ask answers or fails, and says nothing once it ends", async () => {
    const host = stubBlockHost(fixture);
    const release = holdRequests(host);
    const heard: string[] = [];
    let args: Readonly<Record<string, Json>> = {};
    const names = follow(host, "names", {
      args: () => args,
      loading: () => heard.push("loading"),
      answer: () => heard.push("answer"),
      failed: () => heard.push("failed"),
    });
    expect(heard).toEqual(["loading"]);

    release();
    await vi.advanceTimersByTimeAsync(0);
    host.setContext({ status: { ...READY, status: "stale", revision: 2 } });
    host.setContext({ inputs: [{ input: "data", revision: 2 }] });
    expect(heard).toEqual(["loading", "answer", "loading"]);

    // The fixture records no answer with these args, and the ask they replace
    // hands on nothing.
    args = { pattern: "z" };
    names.refresh();
    release();
    await vi.advanceTimersByTimeAsync(0);
    expect(heard).toEqual(["loading", "answer", "loading", "loading", "failed"]);

    names.release();
    host.setContext({ inputs: [{ input: "data", revision: 3 }] });
    release();
    await vi.advanceTimersByTimeAsync(0);
    expect(heard).toEqual(["loading", "answer", "loading", "loading", "failed"]);
  });
});
