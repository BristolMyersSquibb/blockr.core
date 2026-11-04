// The validator of a JSON Schema. The store uses it to check what packages
// register, and the command registry uses it to check a command's args and a
// block's state.

import { Validator } from "@cfworker/json-schema";
import type { OutputUnit, SchemaDraft } from "@cfworker/json-schema";
import type { JsonSchema } from "../types/json.ts";
import { messageOf } from "./util.ts";

// A validator of a schema, which gives each way a value fails it in a clause
// that follows what the value is, such as `fails its schema at /x: Instance
// type "string" is invalid. Expected "number".`
export type Validate = (value: unknown) => readonly string[];

// The validator of a schema, or an error where the schema cannot be read. Its
// clauses start with `fails`, or with another verb for a value named in the
// plural, as "fail their schema" follows a command's args.
export function compile(
  what: string,
  schema: JsonSchema,
  fails = "fails its schema",
): Validate {
  let validator: Validator;
  try {
    validator = new Validator(schema, draftOf(schema), false);
  } catch (error) {
    throw new Error(`The schema of ${what} cannot be read: ${messageOf(error)}`, {
      cause: error,
    });
  }
  return (value) => {
    const result = validator.validate(value);
    return result.valid ? [] : describe(result.errors, fails);
  };
}

// The draft a schema declares in its `$schema`, 2020-12 where it declares
// none, as Zod's z.toJSONSchema() writes it.
function draftOf(schema: JsonSchema): SchemaDraft {
  const declared = typeof schema.$schema === "string" ? schema.$schema : "";
  if (declared.includes("draft-04")) return "4";
  if (declared.includes("draft-07")) return "7";
  if (declared.includes("2019-09")) return "2019-09";
  return "2020-12";
}

// The keywords whose failures only say that a part of the value failed, beside
// the failure of that part, which says how.
const wrappers = new Set([
  "properties",
  "items",
  "prefixItems",
  "additionalItems",
  "$ref",
  "$dynamicRef",
  "$recursiveRef",
  "allOf",
  "false",
]);

function describe(errors: readonly OutputUnit[], fails: string): string[] {
  return errors
    .filter((error) => !wrappers.has(error.keyword))
    .map((error) => {
      const at = error.instanceLocation.replace(/^#/, "");
      return `${fails}${at === "" ? "" : ` at ${at}`}: ${error.error}`;
    });
}
