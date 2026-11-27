// @vitest-environment node

// Checks every fixture of the wire protocol against core's types of the
// messages, by type-checking the fixture as the value of a variable of its
// type, so that the check is the compiler's own: a property that a type lacks,
// a kind or status that it does not allow, and a property that it requires and
// the fixture leaves out all fail.
// A fixture is JSON, so it is checked against the types as JSON gives them, in
// which each id that core interns is the record of its package and its id that
// it serializes to.

import { readdirSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { beforeAll, expect, test } from "vitest";
import { declaration, typecheck } from "../testing/typecheck.ts";
import { PROTOCOL_VERSION } from "./messages.ts";
import type { Fixture } from "./messages.ts";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const fixtures = join(root, "inst", "protocol", "fixtures");
const messages = join(root, "srcts", "protocol", "messages.ts");
const board = join(root, "srcts", "types", "board.ts");
const jsonTypes = join(root, "srcts", "types", "json.ts");

// The type of each fixture, by its file's name, since a fixture's replies say
// nothing of the kind of request they answer.
const types: Record<string, string> = {
  hello: "Fixture<[HelloRequest], [[HelloReply]]>",
  "hello-locked": "Fixture<[HelloRequest], [[HelloReply]]>",
  "hello-block": "Fixture<[HelloRequest], [[HelloReply]]>",
  "hello-file": "Fixture<[HelloRequest], [[HelloReply]]>",
  "hello-declared": "Fixture<[HelloRequest], [[HelloReply]]>",
  "blockr-options": "Fixture<[BlockrOptionsRequest], [[BlockrOptionsReply]]>",
  evaluate: "Fixture<[EvaluateRequest], [[EvaluateReply]]>",
  "evaluate-traced": "Fixture<[EvaluateRequest], [[EvaluateReply]]>",
  "evaluate-warning": "Fixture<[EvaluateRequest], [[EvaluateReply]]>",
  "evaluate-variadic": "Fixture<[EvaluateRequest], [[EvaluateReply]]>",
  "evaluate-error": "Fixture<[EvaluateRequest], [[EvaluateReply]]>",
  "evaluate-invalid": "Fixture<[EvaluateRequest], [[EvaluateReply]]>",
  "evaluate-waiting": "Fixture<[EvaluateRequest], [[EvaluateReply]]>",
  "evaluate-params": "Fixture<[EvaluateRequest], [[EvaluateReply]]>",
  "evaluate-missing": "Fixture<[EvaluateRequest], [[MissingReply]]>",
  "evaluate-locked": "Fixture<[EvaluateRequest], [[EvaluateReply]]>",
  "several-requests": `Fixture<
    [EvaluateRequest, EvaluateRequest, OutputRequest],
    [[EvaluateReply], [EvaluateReply], [OutputReply]]
  >`,
  request: "Fixture<[BlockClassRequest], [[RequestReply]]>",
  "request-error": "Fixture<[BlockClassRequest], [[ErrorReply]]>",
  "request-action": "Fixture<[BlockClassRequest], [[RequestReply]]>",
  "request-option": "Fixture<[BoardOptionRequest], [[RequestReply]]>",
  "request-plugin": "Fixture<[PluginRequest], [[RequestReply]]>",
  "request-progress":
    "Fixture<[PluginRequest], [[Progress], [Progress], [RequestReply]]>",
  "request-output": "Fixture<[OutputClassRequest], [[RequestReply]]>",
  "request-renderer": "Fixture<[RendererRequest], [[RequestReply]]>",
  output: "Fixture<[OutputRequest], [[OutputReply]]>",
  "output-plot": "Fixture<[OutputRequest], [[OutputReply]]>",
  "output-warning": "Fixture<[OutputRequest], [[OutputReply]]>",
  "output-error": "Fixture<[OutputRequest], [[ErrorReply]]>",
  cancel: "Fixture<[CancelRequest], [[CancelledReply]]>",
  forget: "Fixture<[ForgetRequest], [[CancelledReply]]>",
};

const names = readdirSync(fixtures)
  .filter((file) => file.endsWith(".json"))
  .map((file) => file.slice(0, -".json".length))
  .sort();

function text(name: string): string {
  return readFileSync(join(fixtures, `${name}.json`), "utf8").trimEnd();
}

function read(name: string): Fixture {
  return JSON.parse(text(name)) as Fixture;
}

// Fixtures that break the types, each in one way, made from the cancel
// fixture, with what the check reports for each.
const cancel = read("cancel");
const breaks: Record<string, [object, RegExp]> = {
  "a kind that its type does not allow": [
    { ...cancel, client: [{ kind: "cancle", request: "17" }] },
    /'"cancle"' is not assignable/,
  ],
  "a property that its type lacks": [
    { ...cancel, r: [[{ id: "17", status: "cancelled", reason: "gone" }]] },
    /'"reason"' does not exist in type 'CancelledReply'/,
  ],
  "a property of another type": [
    { ...cancel, client: [{ kind: "cancel", request: 17 }] },
    /'number' is not assignable to type 'string'/,
  ],
  "no property that its type requires": [
    { ...cancel, r: [[{ id: "17" }]] },
    /'status' is missing/,
  ],
};

// Where the module that declares a fixture is, which exists only in memory.
function path(name: string): string {
  return join(root, "srcts", "protocol", "__fixtures__", `${name}.ts`);
}

// A module, which exists only in memory too, that gives the types of the
// fixtures' messages as JSON gives them, under their names in messages.ts.
// Each id that core interns, as the registry id of a recipe or the class that a
// plugin's request names, is there the record of its package and its id, since
// the brand of an id exists for the type checker alone (Interned,
// ../types/board.ts).
// The compiler finds a module that its imports name only in a directory that
// exists, so this one sits beside messages.ts.
const serialized = join(root, "srcts", "protocol", "__serialized__.ts");

function serializedModule(): string {
  const names = [...new Set(Object.values(types).join(" ").match(/\b[A-Z]\w*/g))].filter((name) => name !== "Fixture");
  return [
    `import type { Interned } from ${JSON.stringify(board)};`,
    `import type { Json } from ${JSON.stringify(jsonTypes)};`,
    `import type * as messages from ${JSON.stringify(messages)};`,
    "type Serialized<T> = T extends Interned<infer P> ? { package: P; id: string } : T extends Json ? T : T extends object ? { [K in keyof T]: Serialized<T[K]> } : T;",
    "export type Fixture<C extends messages.ClientMessage, R extends messages.RMessage[]> = Serialized<messages.Fixture<C, R>>;",
    `export type { ${names.join(", ")} } from ${JSON.stringify(messages)};`,
  ].join("\n");
}

let diagnostics: Map<string, string[]>;

beforeAll(() => {
  const modules = new Map<string, string>([[serialized, serializedModule()]]);

  for (const name of names) {
    const type = types[name];
    if (type === undefined) continue;
    modules.set(path(name), declaration(type, serialized, text(name)));
  }

  for (const [what, [fixture]] of Object.entries(breaks)) {
    const json = JSON.stringify(fixture, null, 2);
    modules.set(path(`broken/${what}`), declaration(types.cancel ?? "", messages, json));
  }

  diagnostics = typecheck(root, modules);
}, 60_000);

test("every fixture has a type to be checked against", () => {
  expect(names).toEqual(Object.keys(types).sort());
});

test("the types of the messages as JSON gives them type-check", () => {
  expect(diagnostics.get(serialized)).toEqual([]);
});

test.each(names)("%s meets the types of its messages", (name) => {
  expect(diagnostics.get(path(name))).toEqual([]);
});

test.each(Object.entries(breaks))(
  "the check fails a fixture with %s",
  (what, [, expected]) => {
    const found = diagnostics.get(path(`broken/${what}`)) ?? [];
    expect(found.join("\n")).toMatch(expected);
  },
);

test("the client speaks the protocol version of the handshake's fixtures", () => {
  const versions = names
    .filter((name) => name.startsWith("hello"))
    .flatMap((name) => {
      const { client, r } = read(name);
      return [...client, ...r.flat()].flatMap((item) =>
        "protocol" in item ? [item.protocol] : [],
      );
    });

  expect(versions.length).toBeGreaterThan(0);
  expect(new Set(versions)).toEqual(new Set([PROTOCOL_VERSION]));
});
