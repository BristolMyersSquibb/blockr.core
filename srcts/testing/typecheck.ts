// Type-checks JSON fixtures against core's types, by declaring each as the
// value of a variable of its type, so that the check is the compiler's own: a
// field that a type lacks, a value that it does not allow, and a field that it
// requires and the fixture leaves out all fail.

import { join } from "node:path";
import ts from "typescript";

// A module that declares a value of the type with the JSON as its literal,
// which starts on the module's first line, so that the compiler's line numbers
// are the JSON's.
// The type's names are imported from the module `from`,
// but for TypeScript's own, such as Record.
export function declaration(type: string, from: string, json: string): string {
  const imported = [...new Set(type.match(/\b[A-Z]\w*/g))]
    .filter((name) => !builtins.has(name))
    .join(", ");
  return (
    `import type { ${imported} } from ${JSON.stringify(from)}; ` +
    `export const value: ${type} = ${json};\n`
  );
}

const builtins = new Set(["Partial", "Readonly", "Record"]);

// Type-checks the modules, given by their paths, which exist only in memory,
// with the options that tsconfig.client.json in `root` checks the client with,
// and gives each module's diagnostics.
export function typecheck(
  root: string,
  modules: Map<string, string>,
): Map<string, string[]> {
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
