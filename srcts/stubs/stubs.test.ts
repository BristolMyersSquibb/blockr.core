// The stubs of the hosts, with which a module mounts in a test runner, on the
// fixtures that core's R tests and output classes write.

import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, test } from "vitest";
import type { BlockHost, BlockModule, OptionHost } from "../types/hosts.ts";
import type { OutputFixtures } from "../types/outputs.ts";
import { mount, READY, stubBlockHost, stubOptionHost, stubOutputHost } from "./index.ts";
import type { BlockFixture } from "./index.ts";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

// The renderer that R puts in an output, as it would serve the module of the
// renderer with the id given.
function drawnBy(id: string): { id: string; module: string } {
  return { id, module: `${id.replace("-", "/")}.js` };
}

function json(...path: string[]): unknown {
  return JSON.parse(readFileSync(join(root, ...path), "utf8"));
}

// A head block's fixture, as expect_block_fixture() writes it for core's tests
// from the states `list(n = 6L, direction = "head")` and `list(n = 2L)`.
const headFixture = json("tests", "testthat", "_snaps", "block-fixture", "head_block.json") as BlockFixture;
const tables = json("inst", "outputs", "fixtures", "table-output.json") as OutputFixtures;

// A fixture of a block class whose request lists a package's datasets, by its
// state arg package and its parameter pattern, and reads the board option
// n_rows.
const datasets: BlockFixture = {
  states: [{ package: "datasets", dataset: "iris" }, { package: "datasets", dataset: "mtcars" }],
  requests: [
    { name: "datasets", args: { package: "datasets" }, options: {}, answer: ["iris", "mtcars"] },
    { name: "datasets", args: { package: "datasets", pattern: "^m" }, options: {}, answer: ["mtcars"] },
    { name: "preview", args: { dataset: "iris" }, options: { n_rows: 3 }, answer: [5.1, 4.9, 4.7] },
  ],
};

describe("the stub of a block's host", () => {
  test("answers a request from the fixture, with the state args the host fills in from the block's state", async () => {
    const host = stubBlockHost(datasets, { context: { options: { n_rows: 3 } } });

    await expect(host.request("datasets")).resolves.toEqual(["iris", "mtcars"]);
    await expect(host.request("datasets", { pattern: "^m" })).resolves.toEqual(["mtcars"]);
    await expect(host.request("preview")).resolves.toEqual([5.1, 4.9, 4.7]);
    await expect(host.request("datasets", { package: "MASS" })).rejects.toThrow(
      'The fixture records no answer of the request "datasets" with the args {"package":"MASS"}, on the state {"package":"datasets","dataset":"iris"} and the board options {"n_rows":3}.',
    );
    expect(host.requested.map(({ name, args }) => [name, args])).toEqual([
      ["datasets", {}],
      ["datasets", { pattern: "^m" }],
      ["preview", {}],
      ["datasets", { package: "MASS" }],
    ]);
  });

  test("answers a request only where the context holds the board option values it was recorded with", async () => {
    const host = stubBlockHost(datasets, { context: { options: { n_rows: 10 } } });
    await expect(host.request("preview")).rejects.toThrow('records no answer of the request "preview"');
  });

  test("records each submit, and refuses one that would leave the block in a state the fixture does not list", async () => {
    const host = stubBlockHost(datasets);
    const heard: unknown[] = [];
    host.on("state", () => heard.push(host.state()));

    await host.submit({ dataset: "mtcars" });
    await expect(host.submit({ dataset: "airquality" })).rejects.toThrow(
      'The module submitted the state {"package":"datasets","dataset":"airquality"}, which the fixture does not list.',
    );

    expect(host.state()).toEqual({ package: "datasets", dataset: "mtcars" });
    expect(heard).toEqual([{ package: "datasets", dataset: "mtcars" }]);
    expect(host.submitted).toEqual([
      { package: "datasets", dataset: "mtcars" },
      { package: "datasets", dataset: "airquality" },
    ]);
  });

  test("serves a fixture that R writes, which lists each state as a block holds it", async () => {
    const host = stubBlockHost(headFixture);
    expect(host.state()).toEqual(headFixture.states[0]);
    await expect(host.request("n_max")).resolves.toEqual([150]);
    await host.submit({ n: 2 });
    expect(host.state()).toEqual({ n: 2, direction: "head" });
  });

  test("answers an upload that R recorded with the file as its arg file, by the file's name, type and size", async () => {
    const uploads = json("tests", "testthat", "_snaps", "file-upload", "upload_block.json") as BlockFixture;
    const host = stubBlockHost(uploads, { state: {} });
    const file = new File(["Time,demand\n1,8.3\n"], "sales.csv", { type: "text/csv" });

    await expect(host.request("upload", {}, { file })).resolves.toEqual(["uploads/3f9a2c/sales.csv"]);
    await expect(host.request("upload", {}, { file: new File(["x"], "sales.csv", { type: "text/csv" }) })).rejects.toThrow(
      'records no answer of the request "upload"',
    );
    await expect(host.request("upload")).rejects.toThrow('records no answer of the request "upload"');
    expect(host.requested[0]).toEqual({ name: "upload", args: {}, file });
  });

  test("announces a state and a context set from outside the module", () => {
    const host = stubBlockHost(datasets);
    const heard: string[] = [];
    host.on("state", () => heard.push(`state ${JSON.stringify(host.state().dataset)}`));
    const off = host.on("context", () => heard.push(`context ${host.context().status.status}`));

    host.setState({ package: "datasets", dataset: "mtcars" });
    host.setContext({ status: { ...READY, status: "stale", revision: 2 } });
    off();
    host.setContext({ name: "Datasets" });

    expect(heard).toEqual(['state "mtcars"', "context stale"]);
    expect(host.context()).toMatchObject({ name: "Datasets", inputs: [], options: {} });
    expect(() => host.on("output" as "state", () => undefined)).toThrow('The host announces "state" and "context", not "output".');
  });

  test("holds what the module logs, at every level, naming the package of its class", () => {
    const host = stubBlockHost(datasets, { class: "blockr.io-dataset_block" });
    const failure = new Error("No dataset.");
    host.log.trace("Called.", { n: 1 });
    host.log.error(failure);

    expect(host.logged.map(({ level, package: pkg, message, attributes }) => ({ level, package: pkg, message, attributes }))).toEqual([
      { level: "trace", package: "blockr.io", message: "Called.", attributes: { n: 1 } },
      { level: "error", package: "blockr.io", message: "No dataset.", attributes: {} },
    ]);
    expect(host.logged[1]?.error).toBe(failure);
  });

  test("records each one-time evaluation and gives the modules that other packages offer", async () => {
    const filter = { api: 1 };
    const host = stubBlockHost(datasets, {
      modules: { "blockr.dplyr-filter": filter },
      evaluate: (params) => ({ ...READY, status: params.save === true ? "ready" : "invalid" }),
    });

    await expect(host.evaluate({ save: true })).resolves.toMatchObject({ status: "ready" });
    await expect(host.evaluate({})).resolves.toMatchObject({ status: "invalid" });
    expect(host.evaluated).toEqual([{ save: true }, {}]);
    await expect(host.module("blockr.dplyr-filter")).resolves.toBe(filter);
    await expect(host.module("blockr.dplyr-select")).rejects.toThrow('No package offers a module "blockr.dplyr-select".');
  });
});

describe("mount()", () => {
  test("mounts a module as core does, and unmounts it by aborting its signal and running its cleanup", async () => {
    let cleaned = 0;
    const mod: BlockModule = {
      render({ el, host }) {
        const draw = (): void => {
          el.textContent = JSON.stringify(host.state().dataset);
        };
        draw();
        const off = host.on("state", draw);
        return () => {
          off();
          cleaned++;
        };
      },
    };
    const host: BlockHost = stubBlockHost(datasets);

    const mounted = await mount({ api: 1, default: mod }, host);
    expect(mounted.el.textContent).toBe('"iris"');
    await host.submit({ dataset: "mtcars" });
    expect(mounted.el.textContent).toBe('"mtcars"');

    mounted.unmount();
    mounted.unmount();
    expect(mounted.signal.aborted).toBe(true);
    expect(cleaned).toBe(1);
  });

  test("refuses a module that declares another major of core's JS API, or has no render()", async () => {
    const host = stubBlockHost(datasets);
    await expect(mount({ default: { render: () => undefined } }, host)).rejects.toThrow(
      "The module declares no major of core's JS API, where core's is 1, as `export const api = 1`.",
    );
    await expect(mount({ api: 2, default: { render: () => undefined } }, host)).rejects.toThrow(
      "The module declares major 2 of core's JS API",
    );
    await expect(mount({ api: 1 }, host)).rejects.toThrow("The module has no default export with a render() function.");
  });
});

describe("the stub of a board option's host", () => {
  test("holds the value, records each submit and answers the board option's requests", async () => {
    const host: OptionHost<string> & ReturnType<typeof stubOptionHost<string>> = stubOptionHost({
      option: "data_dir",
      value: "/data",
      requests: [{ name: "list_dir", args: { path: "/data" }, answer: ["a", "b"] }],
    });
    const heard: string[] = [];
    host.on("value", () => heard.push(host.value()));

    await host.submit("/data/a");
    host.setValue("/data/b");

    expect(heard).toEqual(["/data/a", "/data/b"]);
    expect(host.submitted).toEqual(["/data/a"]);
    await expect(host.request("list_dir", { path: "/data" })).resolves.toEqual(["a", "b"]);
    await expect(host.request("list_dir", { path: "/" })).rejects.toThrow(
      'No answer is given for the request "list_dir" of the board option "data_dir" with the args {"path":"/"}.',
    );
  });

  test("holds what the module logs, naming no package", () => {
    const host = stubOptionHost({ option: "data_dir", value: "/data" });
    host.log.warn("No such directory.");
    expect(host.logged.map(({ level, package: pkg, message }) => ({ level, package: pkg, message }))).toEqual([
      { level: "warn", package: null, message: "No such directory." },
    ]);
  });
});

describe("the stub of an output's host", () => {
  test("holds the first output of its class's fixtures, and takes another from them as the renderer sets its parameters", () => {
    const host = stubOutputHost({ fixtures: tables, options: { n_rows: 50 } });
    expect(host.output()).toEqual(tables.outputs[0]?.output);
    expect(host.params()).toEqual({ page: 2 });
    let heard = 0;
    host.on("output", () => heard++);

    host.setParams({ page: 4, sort: { column: "Ozone", desc: true } });
    expect(host.output()).toMatchObject({ total: 153 });
    expect(host.params()).toEqual({ page: 4, sort: { column: "Ozone", desc: true } });
    expect(host.error()).toBeNull();

    // No fixture of the board option values given has these parameters.
    host.setParams({ page: 2 });
    expect(host.output()).toMatchObject({ total: 153 });
    expect(host.error()?.message).toBe('The fixtures hold no output asked with the parameters {"page":2}.');

    expect(heard).toBe(2);
    expect(host.asked).toEqual([{ page: 4, sort: { column: "Ozone", desc: true } }, { page: 2 }]);
  });

  test("gives the board option values the output was asked with, those given or else those of the first of the fixtures", () => {
    expect(stubOutputHost({ fixtures: tables, options: { n_rows: 50 } }).options()).toEqual({ n_rows: 50 });
    expect(stubOutputHost({ fixtures: tables }).options()).toEqual({ n_rows: 3 });

    const host = stubOutputHost({ fixtures: tables });
    host.setParams({ page: 2 });
    expect(host.error()).toBeNull();
    expect(stubOutputHost({ output: { renderer: "blockr.core-text_output", text: "1" } }).options()).toEqual({});
  });

  test("holds what the renderer logs, naming no package", () => {
    const host = stubOutputHost({ output: { renderer: drawnBy("blockr.core-table_output"), columns: [], rows: [], total: 0 } });
    host.log.info("Drawn.");
    expect(host.logged.map(({ level, package: pkg, message }) => ({ level, package: pkg, message }))).toEqual([
      { level: "info", package: null, message: "Drawn." },
    ]);
  });

  test("records what the renderer emits", () => {
    const host = stubOutputHost({ output: { renderer: drawnBy("blockr.core-table_output"), columns: [], rows: [], total: 0 } });
    host.emit("select", { rows: [1] });
    expect(host.emitted).toEqual([{ name: "select", payload: { rows: [1] } }]);
  });

  test("says what core would while it asks R for a newer output, or once that failed", () => {
    const host = stubOutputHost({ output: { renderer: drawnBy("blockr.core-text_output"), text: "1" } });
    host.setLoading(true);
    expect(host.loading()).toBe(true);
    host.setOutput({ renderer: drawnBy("blockr.core-text_output"), text: "2" });
    expect([host.loading(), host.output()]).toEqual([
      false,
      { renderer: drawnBy("blockr.core-text_output"), text: "2" },
    ]);
    host.setError({ severity: "error", class: ["simpleError", "error", "condition"], message: "boom" });
    expect(host.error()?.message).toBe("boom");
  });
});
