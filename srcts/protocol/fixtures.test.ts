// @vitest-environment node

// Checks every fixture of the wire protocol against core's types of the
// messages, by type-checking the fixture as the value of a variable of its
// type, so that the check is the compiler's own: a field that a type lacks, a
// kind or status that it does not allow, and a field that it requires and the
// fixture leaves out all fail.

import { readdirSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import ts from "typescript";
import { beforeAll, describe, expect, test } from "vitest";
import { PROTOCOL_VERSION } from "./messages.ts";
import type { Fixture } from "./messages.ts";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const fixtures = join(root, "inst", "protocol", "fixtures");
const messages = join(root, "srcts", "protocol", "messages.ts");
const board = join(root, "srcts", "types", "board.ts");

// The type of each fixture, by its file's name, since a fixture's replies say
// nothing of the kind of request they answer.
const types: Record<string, string> = {
  hello: "Fixture<[HelloRequest], [[HelloReply]]>",
  "hello-board": "Fixture<[HelloRequest], [[HelloReply]]>",
  "hello-locked": "Fixture<[HelloRequest], [[HelloReply]]>",
  "hello-block": "Fixture<[HelloRequest], [[HelloReply]]>",
  open: "Fixture<[OpenRequest], [[OpenReply]]>",
  "open-locked": "Fixture<[OpenRequest], [[ErrorReply]]>",
  evaluate: "Fixture<[EvaluateRequest], [[EvaluateReply]]>",
  "evaluate-warning": "Fixture<[EvaluateRequest], [[EvaluateReply]]>",
  "evaluate-variadic": "Fixture<[EvaluateRequest], [[EvaluateReply]]>",
  "evaluate-error": "Fixture<[EvaluateRequest], [[EvaluateReply]]>",
  "evaluate-invalid": "Fixture<[EvaluateRequest], [[EvaluateReply]]>",
  "evaluate-params": "Fixture<[EvaluateRequest], [[EvaluateReply]]>",
  "evaluate-missing": "Fixture<[EvaluateRequest], [[MissingReply]]>",
  "several-requests": `Fixture<
    [EvaluateRequest, EvaluateRequest, OutputRequest],
    [[EvaluateReply], [EvaluateReply], [OutputReply]]
  >`,
  request: "Fixture<[BlockClassRequest], [[RequestReply]]>",
  "request-error": "Fixture<[BlockClassRequest], [[ErrorReply]]>",
  "request-action": "Fixture<[BlockClassRequest], [[RequestReply]]>",
  "request-option": "Fixture<[BoardOptionRequest], [[RequestReply]]>",
  "request-extension": "Fixture<[ExtensionRequest], [[RequestReply]]>",
  "request-progress":
    "Fixture<[ExtensionRequest], [[Progress], [Progress], [RequestReply]]>",
  "request-output": "Fixture<[OutputClassRequest], [[RequestReply]]>",
  output: "Fixture<[OutputRequest], [[OutputReply]]>",
  "output-plot": "Fixture<[OutputRequest], [[OutputReply]]>",
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

// The saved files that a fixture's requests carry, by where each sits.
function files(name: string): [string, string][] {
  return read(name).client.flatMap((request, i) =>
    "file" in request && request.file !== undefined
      ? [[`client[${String(i)}].file`, request.file] as [string, string]]
      : [],
  );
}

// Fixtures that break the types, each in one way, made from the cancel
// fixture, with what the check reports for each.
const cancel = read("cancel");
const breaks: Record<string, [object, RegExp]> = {
  "a kind that its type does not allow": [
    { ...cancel, client: [{ kind: "cancle", request: "17" }] },
    /'"cancle"' is not assignable/,
  ],
  "a field that its type lacks": [
    { ...cancel, r: [[{ id: "17", status: "cancelled", reason: "gone" }]] },
    /'"reason"' does not exist in type 'CancelledReply'/,
  ],
  "a field of another type": [
    { ...cancel, client: [{ kind: "cancel", request: 17 }] },
    /'number' is not assignable to type 'string'/,
  ],
  "no field that its type requires": [
    { ...cancel, r: [[{ id: "17" }]] },
    /'status' is missing/,
  ],
};

// Where the module that declares a fixture is, which exists only in memory.
function path(name: string): string {
  return join(root, "srcts", "protocol", "__fixtures__", `${name}.ts`);
}

// A module that declares a value of the type with the JSON as its literal,
// which starts on the module's first line, so that the compiler's line numbers
// are the JSON's.
function declaration(type: string, from: string, json: string): string {
  const imported = [...new Set(type.match(/\b[A-Z]\w*/g))].join(", ");
  return (
    `import type { ${imported} } from ${JSON.stringify(from)}; ` +
    `export const value: ${type} = ${json};\n`
  );
}

// Type-checks the modules, given by their paths, with the options that
// tsconfig.client.json checks the client with, and gives each module's
// diagnostics.
function typecheck(modules: Map<string, string>): Map<string, string[]> {
  const config = ts.readConfigFile(join(root, "tsconfig.client.json"), (file) =>
    ts.sys.readFile(file),
  );
  const { options } = ts.parseJsonConfigFileContent(
    config.config,
    ts.sys,
    root,
  );
  delete options.tsBuildInfoFile;

  const host = ts.createCompilerHost(options, true);
  const getSourceFile = host.getSourceFile.bind(host);
  const fileExists = host.fileExists.bind(host);
  const readFile = host.readFile.bind(host);

  host.getSourceFile = (file, version, ...rest) => {
    const module = modules.get(file);
    return module === undefined
      ? getSourceFile(file, version, ...rest)
      : ts.createSourceFile(file, module, version, true);
  };
  host.fileExists = (file) => modules.has(file) || fileExists(file);
  host.readFile = (file) => modules.get(file) ?? readFile(file);

  const program = ts.createProgram([...modules.keys()], options, host);

  const format = (diagnostic: ts.Diagnostic): string => {
    const message = ts.flattenDiagnosticMessageText(
      diagnostic.messageText,
      "\n",
    );
    if (diagnostic.file === undefined || diagnostic.start === undefined) {
      return message;
    }
    const { line, character } =
      diagnostic.file.getLineAndCharacterOfPosition(diagnostic.start);
    return `${String(line + 1)}:${String(character + 1)} ${message}`;
  };

  return new Map(
    [...modules.keys()].map((file) => [
      file,
      ts.getPreEmitDiagnostics(program, program.getSourceFile(file)).map(format),
    ]),
  );
}

let diagnostics: Map<string, string[]>;

beforeAll(() => {
  const modules = new Map<string, string>();

  for (const name of names) {
    const type = types[name];
    if (type === undefined) continue;
    modules.set(path(name), declaration(type, messages, text(name)));
    for (const [at, file] of files(name)) {
      const json = JSON.stringify(JSON.parse(file), null, 2);
      modules.set(path(`${name}/${at}`), declaration("SavedBoard", board, json));
    }
  }

  for (const [what, [fixture]] of Object.entries(breaks)) {
    const json = JSON.stringify(fixture, null, 2);
    modules.set(path(`broken/${what}`), declaration(types.cancel ?? "", messages, json));
  }

  diagnostics = typecheck(modules);
}, 60_000);

test("every fixture has a type to be checked against", () => {
  expect(names).toEqual(Object.keys(types).sort());
});

describe.each(names)("%s", (name) => {
  test("meets the types of its messages", () => {
    expect(diagnostics.get(path(name))).toEqual([]);
  });

  test.each(files(name))("carries in %s a saved board", (at) => {
    expect(diagnostics.get(path(`${name}/${at}`))).toEqual([]);
  });
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
