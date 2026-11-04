// @vitest-environment node

// What the build ships in inst/types, which a package's build resolves as
// `@blockr/core`: the declarations of the types that modules are written
// against and of the stubs of the hosts, the stubs bundled for a test runner,
// and the package.json that names them.

import { copyFile, mkdir, mkdtemp, readdir, readFile, rm, symlink, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import ts from "typescript";
import { afterEach, beforeEach, expect, test } from "vitest";
import type * as StubsModule from "../stubs/index.ts";
import type { BlockFixture } from "../stubs/index.ts";
import type { BlockModule } from "../types/hosts.ts";
import { types, TYPES_PACKAGE } from "./build.ts";

type Stubs = typeof StubsModule;

const repo = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

let root: string;

beforeEach(async () => {
  root = await mkdtemp(join(tmpdir(), "blockr-core-types-"));
});

afterEach(async () => {
  await rm(root, { recursive: true, force: true });
});

async function write(files: Record<string, string>): Promise<void> {
  for (const [path, content] of Object.entries(files)) {
    const file = join(root, path);
    await mkdir(dirname(file), { recursive: true });
    await writeFile(file, content);
  }
}

async function files(dir: string): Promise<string[]> {
  const entries = await readdir(dir, { recursive: true, withFileTypes: true });
  return entries
    .filter((entry) => entry.isFile())
    .map((entry) => relative(dir, join(entry.parentPath, entry.name)))
    .sort();
}

test("writes the declarations of the types and the stubs, the stubs bundled, and the package.json that names them", async () => {
  await copyFile(join(repo, "tsconfig.client.json"), join(root, "tsconfig.client.json"));
  await write({
    "srcts/types/json.ts": "export type Json = string | number | Json[];\n",
    "srcts/index.ts": 'export type { Json } from "./types/json.ts";\n',
    "srcts/stubs/index.ts": [
      'import type { Json } from "../types/json.ts";',
      "export function echo(x: Json): Json {",
      "  return x;",
      "}",
      "",
    ].join("\n"),
    "inst/types/gone.d.ts": "",
  });

  const written = await types(root);

  expect(written).toEqual([
    "inst/types/index.d.ts",
    "inst/types/package.json",
    "inst/types/stubs/index.d.ts",
    "inst/types/stubs/index.js",
    "inst/types/types/json.d.ts",
  ]);
  expect(await files(join(root, "inst", "types"))).toEqual(written.map((file) => relative("inst/types", file)));
  expect(JSON.parse(await readFile(join(root, "inst/types/package.json"), "utf8"))).toEqual(TYPES_PACKAGE);
  expect(await readFile(join(root, "inst/types/stubs/index.d.ts"), "utf8")).toContain(
    "export declare function echo(x: Json): Json;",
  );

  const stubs = (await import(pathToFileURL(join(root, "inst/types/stubs/index.js")).href)) as {
    echo: (x: unknown) => unknown;
  };
  expect(stubs.echo([1, "a"])).toEqual([1, "a"]);
});

test("refuses types that do not check", async () => {
  await copyFile(join(repo, "tsconfig.client.json"), join(root, "tsconfig.client.json"));
  await write({ "srcts/index.ts": "export const n: number = 'one';\n" });

  await expect(types(root)).rejects.toThrow("Type 'string' is not assignable to type 'number'.");
});

// A block's UI module and a board class, written against `@blockr/core` as a
// package writes them.
// The module types its block's state, and the class types the properties it
// adds by extending `Board`.
const headModule = `import type { BlockModule, Board, BoardClass } from "@blockr/core";

interface ViewsBoard extends Board {
  readonly views: Readonly<Record<string, { readonly name: string }>>;
}

type HeadState = { n: number; direction: "head" | "tail" };

export const api = 1;

const head: BlockModule<HeadState> = {
  options: ["n_rows"],
  render({ el, host, signal }) {
    const draw = (): void => {
      el.textContent = String(host.state().n);
    };
    draw();
    signal.addEventListener("abort", () => {
      el.replaceChildren();
    });
    void host.request<number[]>("n_max").then((max) => {
      el.title = String(max);
    });
    return host.on("state", draw);
  },
};

export default head;

export function views(board: ViewsBoard): string[] {
  return Object.values(board.views).map((view) => view.name);
}

export const viewsBoard: BoardClass = {
  class: "views_board",
  package: "blockr.test",
  parent: "board",
  frontEnd: "blockr.test-views",
  properties: { views: { schema: { type: "object" }, default: {} } },
  update: (delta, board) =>
    (delta.blocks?.rm ?? [])
      .filter((id) => Object.hasOwn((board as ViewsBoard).views, id))
      .map((id) => ({ op: "remove", path: "/views/" + id })),
};
`;

const headTest = `import { mount, stubBlockHost } from "@blockr/core/stubs";
import type { BlockFixture, StubBlockHost } from "@blockr/core/stubs";
import * as head from "./head-block.ts";

declare const fixture: BlockFixture;
const host: StubBlockHost<{ n: number; direction: "head" | "tail" }> = stubBlockHost(fixture);
const mounted = await mount(head, host);
await host.submit({ n: 2 });
mounted.unmount();
export const submitted: number = host.submitted.length;
`;

test.each(["bundler", "nodenext"] as const)(
  "a package's module and its test type-check against what core ships, as %s resolution finds it",
  async (resolution) => {
    await mkdir(join(root, "node_modules", "@blockr"), { recursive: true });
    await symlink(join(repo, "inst", "types"), join(root, "node_modules", "@blockr", "core"), "dir");
    await write({
      "package.json": JSON.stringify({ name: "blockr.test", private: true, type: "module" }),
      "src/head-block.ts": headModule,
      "src/head-block.test.ts": headTest,
    });

    const program = ts.createProgram([join(root, "src/head-block.ts"), join(root, "src/head-block.test.ts")], {
      target: ts.ScriptTarget.ES2022,
      lib: ["lib.es2022.d.ts", "lib.dom.d.ts"],
      module: resolution === "bundler" ? ts.ModuleKind.Preserve : ts.ModuleKind.NodeNext,
      moduleResolution: resolution === "bundler" ? ts.ModuleResolutionKind.Bundler : ts.ModuleResolutionKind.NodeNext,
      strict: true,
      verbatimModuleSyntax: true,
      allowImportingTsExtensions: true,
      noEmit: true,
      skipLibCheck: false,
      types: [],
    });

    const diagnostics = ts.getPreEmitDiagnostics(program).map((diagnostic) =>
      ts.flattenDiagnosticMessageText(diagnostic.messageText, "\n"),
    );
    expect(diagnostics).toEqual([]);
  },
  60_000,
);

test("a package's module mounts on the stubs that core ships, which answer from its class's fixture file", async () => {
  const stubs = (await import(pathToFileURL(join(repo, "inst/types/stubs/index.js")).href)) as Stubs;
  const fixture = JSON.parse(
    await readFile(join(repo, "tests/testthat/_snaps/transform-head/head_block.json"), "utf8"),
  ) as BlockFixture;
  const el = { textContent: "", title: "" } as unknown as HTMLElement;
  const host = stubs.stubBlockHost(fixture);
  const head: BlockModule = {
    render({ host: mountedHost }) {
      const draw = (): void => {
        el.textContent = JSON.stringify(mountedHost.state());
      };
      draw();
      void mountedHost.request("n_max").then((max) => {
        el.title = JSON.stringify(max);
      });
      return mountedHost.on("state", draw);
    },
  };

  const mounted = await stubs.mount({ api: 1, default: head }, host, el);
  await host.submit({ n: 10, direction: "tail" });
  await new Promise((done) => setTimeout(done, 0));
  mounted.unmount();

  expect(el.textContent).toBe('{"n":10,"direction":"tail"}');
  expect(el.title).toBe("[150]");
  await expect(host.submit({ n: 3 })).rejects.toThrow("which the fixture does not list");
});
