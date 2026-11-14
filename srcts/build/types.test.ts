// @vitest-environment node

// What the build ships in inst/types, which a package's build resolves as
// `@blockr/core`: the declarations of the types that modules are written
// against and of the stubs of the hosts, the stubs bundled for a test runner,
// the React SDK, its binding and the build preset bundled for a package's build,
// and the package.json that names them and the React that core's runtime
// bundles as its peer.
// The controls go there as the SDK does.

import { copyFile, cp, mkdir, mkdtemp, readdir, readFile, rm, symlink, writeFile } from "node:fs/promises";
import { createRequire } from "node:module";
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

// Installs core's types as a package installs them, with
// `npm install --no-save --install-links`: as a copy, outside core's repo.
// TypeScript resolves a file's imports from the file's real path, so the
// types find the React they import in the package's node_modules.
// Through a link to `inst/types`, the types would find React in the
// node_modules of core's repo, which a package does not have.
// The test would then pass where a package's type check fails.
async function installTypes(): Promise<void> {
  await cp(join(repo, "inst", "types"), join(root, "node_modules", "@blockr", "core"), { recursive: true });
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

// The packages that the bundle that core ships under the name given imports,
// each by its scope and name, as npm installs it.
async function importedPackages(name: string): Promise<string[]> {
  const text = await readFile(join(repo, "inst", "types", name, "index.js"), "utf8");
  const specifiers = ts.preProcessFile(text, true, true).importedFiles.map((file) => file.fileName);
  const bare = specifiers.filter((specifier) => !specifier.startsWith(".") && !specifier.startsWith("node:"));
  return [...new Set(bare.map((specifier) => specifier.split("/").slice(0, specifier.startsWith("@") ? 2 : 1).join("/")))];
}

test("what a package's tests load imports no package but the React that `@blockr/core` names as its peer, at the version that core's runtime bundles", async () => {
  const manifest = JSON.parse(await readFile(join(repo, "inst/types/package.json"), "utf8")) as typeof TYPES_PACKAGE;
  const installed = createRequire(import.meta.url);
  expect(manifest.peerDependencies).toEqual({
    react: (installed("react/package.json") as { version: string }).version,
    "react-dom": (installed("react-dom/package.json") as { version: string }).version,
  });

  // The stubs draw core's UIs, the board options' among them, with React.
  expect(await importedPackages("stubs")).toContain("react");
  for (const name of ["stubs", "react", "binding", "controls"]) {
    expect((await importedPackages(name)).filter((pkg) => !Object.hasOwn(manifest.peerDependencies, pkg))).toEqual([]);
  }
});

// A block's UI module and a plugin class, written against `@blockr/core` as a
// package writes them.
// The module types its block's state, and the class types the state of its
// plugin with `type`, so that the state is assignable to `Json`, which the
// board holds under the id of the plugin's class.
const headModule = `import type { BlockModule, Board, Json, PluginClass } from "@blockr/core";

type ViewsState = { readonly views: Readonly<Record<string, { readonly name: string }>> };

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

export function pluginViews(board: Board, plugin: string): ViewsState["views"] {
  const state = board.plugins[plugin] as ViewsState | undefined;
  return state?.views ?? {};
}

const initial: ViewsState = { views: {} };
const json: Json = initial;

export const viewsPlugin: PluginClass = {
  id: "blockr.views-views_plugin",
  state: { schema: { type: "object" }, default: json },
  start: ({ signal }) => {
    signal.addEventListener("abort", () => undefined);
  },
  update: (delta, board, plugin) =>
    (delta.blocks?.rm ?? [])
      .filter((id) => Object.hasOwn(pluginViews(board, plugin), id))
      .map((id) => ({ op: "remove", path: "/plugins/" + plugin + "/views/" + id })),
};
`;

// A block's UI module that draws with the controls core ships,
// as a package writes it.
const mergeModule = `import type { BlockModule } from "@blockr/core";
import { controls, strings } from "@blockr/core/controls";

type MergeState = { by?: string[]; all_x: boolean };

export const api = 1;

const merge: BlockModule<MergeState> = {
  render({ el, host, signal }) {
    const ui = controls(host, signal);
    const by = ui.checklist("by", { legend: "By columns", empty: "No column is shared." });
    ui.checkbox("all_x", { label: "Keep the rows of x that y does not match" });
    ui.follow("shared", {
      loading() {
        by.setOutdated(true);
      },
      answer(answer) {
        by.setChoices(strings(answer));
        by.setOutdated(false);
      },
    });
    el.replaceChildren(ui.el);
    return ui.cleanup;
  },
};

export default merge;
`;

// A plugin class's module, whose default export is the class without its id,
// with the `ui` that draws the plugin of the class, by the name that the app
// gives it, and the storage, which gives the controls that a frontend draws
// and names a board to open as the page starts, as a package writes it.
const notesModule = `import type { Notification, PluginClass, PluginModule, Raised, StorageControl } from "@blockr/core";

export const api = 1;

const notes: PluginModule = {
  render({ el, core, plugin, signal }) {
    const draw = (): void => {
      el.textContent = core.plugins.appPlugins()[plugin]?.name ?? "";
    };
    draw();
    const others = Object.keys(core.plugins.appPlugins()).filter(
      (id) => id !== plugin && core.plugins.pluginClass(id).ui !== undefined,
    );
    const ends = others.map((id) => core.mountPlugin(el.appendChild(document.createElement("section")), id));
    const offFocus = core.focus.subscribe((block) => {
      el.dataset.focus = block ?? "";
    });
    const opened: Raised = { severity: "message", code: "notes_opened", message: "Notes opened." };
    const withdraw = core.notifications.notify(plugin, opened);
    signal.addEventListener("abort", withdraw);
    const off = core.store.subscribe(draw);
    return () => {
      off();
      offFocus();
      for (const end of ends) end();
    };
  },
};

export default {
  ui: notes,
  storage: {
    controls: ({ core }) => {
      const save: StorageControl = { kind: "button", id: "save", label: "Save", shortcut: "Mod+S", run: () => Promise.resolve() };
      const status: StorageControl = { kind: "status", id: "saved", text: core.store.board().name, placement: "end" };
      return [save, { kind: "menu", id: "more", label: "More", items: [status] }];
    },
    subscribe: (listener) => {
      listener();
      return () => undefined;
    },
    load: ({ core }) => Promise.resolve(core.lock.load ? undefined : '{"format": 1}'),
  },
  start: ({ core, signal }) => {
    const off = core.notifications.subscribe((all: readonly Notification[]) => {
      const failing = all.some((n) => n.severity === "error" && n.source.kind === "r" && n.source.request.type === "evaluate");
      core.focus.set(failing ? null : core.focus.get());
    });
    signal.addEventListener("abort", off);
  },
} satisfies Omit<PluginClass, "id">;
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
  "a package's modules and their test type-check against what core ships, as %s resolution finds it",
  async (resolution) => {
    await installTypes();
    await write({
      "package.json": JSON.stringify({ name: "blockr.test", private: true, type: "module" }),
      "src/head-block.ts": headModule,
      "src/head-block.test.ts": headTest,
      "src/merge-block.ts": mergeModule,
      "src/notes-plugin.ts": notesModule,
    });

    const sources = ["head-block.ts", "head-block.test.ts", "merge-block.ts", "notes-plugin.ts"].map((file) =>
      join(root, "src", file),
    );
    const program = ts.createProgram(sources, {
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

// A renderer and a block's UI written in React on the React SDK that core
// ships, a block's UI in plain TypeScript on its binding, and the build script
// that bundles them with its build preset, as a package writes them.
const sdkFiles = {
  "src/text-renderer.tsx": `import type { TextOutput } from "@blockr/core";
import { outputRenderer } from "@blockr/core/react";

export const api = 1;

export default outputRenderer<TextOutput>(({ output }) => <pre>{output.text}</pre>);
`,
  "src/head-block.tsx": `import type { BlockHost } from "@blockr/core";
import { NumberInput, reactModule, useValue } from "@blockr/core/react";
import type { ModuleProps } from "@blockr/core/react";
import type { ReactElement } from "react";

type HeadState = { n: number; direction: "head" | "tail" };

function Head({ host }: ModuleProps<BlockHost<HeadState>>): ReactElement {
  return <NumberInput bound={useValue(host, "n")} label="Rows" min={1} />;
}

export const api = 1;

export default reactModule(Head);
`,
  "src/head-plain.ts": `import type { BlockModule } from "@blockr/core";
import { bind } from "@blockr/core/binding";

type HeadState = { n: number; direction: "head" | "tail" };

export const api = 1;

const head: BlockModule<HeadState> = {
  render({ el, host }) {
    const input = document.createElement("input");
    const n = bind(host, "n", (value) => {
      input.value = typeof value === "number" ? String(value) : "";
    });
    input.addEventListener("input", () => {
      n.type(Number(input.value));
    });
    el.replaceChildren(input);
    return () => {
      n.release();
    };
  },
};

export default head;
`,
  "build.ts": `import * as esbuild from "esbuild";
import { preset } from "@blockr/core/build";

await esbuild.build({ ...preset, entryPoints: ["src/text-renderer.tsx", "src/head-block.tsx"], outdir: "inst/js" });
`,
};

test.each(["bundler", "nodenext"] as const)(
  "a package's modules on the React SDK and the binding, and its build on the build preset, type-check against what core ships, as %s resolution finds them",
  async (resolution) => {
    await installTypes();
    // The React that npm installs with `@blockr/core`, and what a package
    // installs to type its modules in React and build them.
    for (const name of ["react", "@types/react", "esbuild"]) {
      await mkdir(dirname(join(root, "node_modules", name)), { recursive: true });
      await symlink(join(repo, "node_modules", name), join(root, "node_modules", name), "dir");
    }
    await write({ "package.json": JSON.stringify({ name: "blockr.test", private: true, type: "module" }), ...sdkFiles });

    const program = ts.createProgram(
      Object.keys(sdkFiles).map((file) => join(root, file)),
      {
        target: ts.ScriptTarget.ES2022,
        lib: ["lib.es2022.d.ts", "lib.dom.d.ts"],
        module: resolution === "bundler" ? ts.ModuleKind.Preserve : ts.ModuleKind.NodeNext,
        moduleResolution: resolution === "bundler" ? ts.ModuleResolutionKind.Bundler : ts.ModuleResolutionKind.NodeNext,
        jsx: ts.JsxEmit.ReactJSX,
        strict: true,
        verbatimModuleSyntax: true,
        allowImportingTsExtensions: true,
        noEmit: true,
        skipLibCheck: false,
        types: [],
      },
    );

    const diagnostics = ts.getPreEmitDiagnostics(program).map((diagnostic) =>
      ts.flattenDiagnosticMessageText(diagnostic.messageText, "\n"),
    );
    expect(diagnostics).toEqual([]);
  },
  60_000,
);
