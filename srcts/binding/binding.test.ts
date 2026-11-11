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
