// The checks a board passes after every delta and as it opens,
// each failure in a sentence: the invariants of core's objects,
// as R's validate_links() and the board's checks did,
// the schemas of the blocks' states and of the board options, and the
// plugins: their classes, the schemas of their states, and at most one
// frontend and one storage plugin.
// Where a delta changed only some objects, their shapes and schemas
// are checked alone, since nothing else can have broken them,
// while the invariants run on the whole board every time.
// A delta that skips core's commands is checked as one that runs through
// them, so that no delta leaves a value that would keep the board from
// opening again.

import { graphOf } from "../graph/index.ts";
import type { Board, RegistryId } from "../types/board.ts";
import type { RegistryEntry } from "../types/registry.ts";
import { compile, isObject, own, quoted } from "../util/index.ts";
import type { Validate } from "../util/index.ts";
import type { Changed } from "./commit.ts";
import { isId } from "./ids.ts";
import { inRole, ROLES } from "./plugins.ts";
import type { Role } from "./plugins.ts";
import type { Registrations } from "./registrations.ts";

// A stack's colour: a hex colour of six digits, such as "#66c2a5",
// the one form that a colour input takes.
// The JSON Schema of a JSON board, which the build generates from core's types,
// holds the same pattern.
export const HEX_COLOR = "^#[0-9a-fA-F]{6}$";

const hexColor = new RegExp(HEX_COLOR);

// The validator of each block class's state, made once for each entry of the
// block registry.
const validators = new WeakMap<RegistryEntry, Validate>();

// Each way a block's state fails the schema of its class, in a clause.
// The schema is the one in the class's entry of the block registry, as the
// class's UI ships it.
// It throws where the schema cannot be read.
export function stateFailures(entry: RegistryEntry, state: unknown): readonly string[] {
  let validate = validators.get(entry);
  if (validate === undefined) {
    validate = compile(`the state of the block class "${entry.id}"`, entry.schema);
    validators.set(entry, validate);
  }
  return validate(state);
}

export function check(
  board: Board,
  registry: Readonly<Record<RegistryId, RegistryEntry>>,
  registrations: Registrations,
  changed?: Changed,
): string[] {
  const failures: string[] = [];

  if (!isId(board.id)) {
    failures.push(`The board's id "${board.id}" is empty or made of digits alone.`);
  }
  if (typeof board.name !== "string") {
    failures.push("The board's name is no string.");
  }

  checkBlocks(board, registry, registrations, changed, failures);
  checkLinks(board, registry, changed, failures);
  checkStacks(board, changed, failures);
  checkValues(board, registrations, changed, failures);
  checkPlugins(board, registrations, changed, failures);

  return failures;
}

function ids(
  what: string,
  record: Readonly<Record<string, unknown>>,
  failures: string[],
): void {
  for (const id of Object.keys(record)) {
    if (!isId(id)) failures.push(`The ${what} id "${id}" is empty or made of digits alone.`);
  }
}

function checkBlocks(
  board: Board,
  registry: Readonly<Record<RegistryId, RegistryEntry>>,
  registrations: Registrations,
  changed: Changed | undefined,
  failures: string[],
): void {
  ids("block", board.blocks, failures);

  for (const [id, block] of Object.entries(board.blocks)) {
    if (changed !== undefined && !changed.blocks.has(id)) continue;
    const fields = shape(block, { registry_id: "string", state: "object", name: "string" });
    if (fields.length > 0) {
      failures.push(`The block "${id}" ${fields.join(", ")}.`);
      continue;
    }
    const entry = own(registry, block.registry_id);
    if (entry === undefined) {
      failures.push(
        `The block "${id}" is of the class "${block.registry_id}", which the block registry lacks.`,
      );
      continue;
    }
    for (const failure of stateFailures(entry, block.state)) {
      failures.push(`The state of the block "${id}" ${failure}`);
    }
  }
}

function checkLinks(
  board: Board,
  registry: Readonly<Record<RegistryId, RegistryEntry>>,
  changed: Changed | undefined,
  failures: string[],
): void {
  ids("link", board.links, failures);

  const fed = new Map<string, string[]>();

  for (const [id, link] of Object.entries(board.links)) {
    if (changed === undefined || changed.links.has(id)) {
      const fields = shape(link, { from: "string", to: "string", input: "string" });
      if (fields.length > 0) {
        failures.push(`The link "${id}" ${fields.join(", ")}.`);
        continue;
      }
    }

    const from = own(board.blocks, link.from);
    const to = own(board.blocks, link.to);
    if (from === undefined) {
      failures.push(`The link "${id}" comes from block "${link.from}", which the board lacks.`);
    }
    if (to === undefined) {
      failures.push(`The link "${id}" goes to block "${link.to}", which the board lacks.`);
      continue;
    }

    const inputs = own(registry, to.registry_id)?.inputs;
    if (inputs === undefined || inputs.includes("...")) continue;
    if (!inputs.includes(link.input)) {
      failures.push(
        `The link "${id}" feeds the input "${link.input}" of block "${link.to}", whose class has no such input.`,
      );
      continue;
    }
    const at = JSON.stringify([link.to, link.input]);
    fed.set(at, [...(fed.get(at) ?? []), id]);
  }

  for (const [at, links] of fed) {
    if (links.length < 2) continue;
    const [to, input] = JSON.parse(at) as [string, string];
    failures.push(
      `The input "${input}" of block "${to}" takes the links ${quoted(links)}, where it takes one.`,
    );
  }

  const cycle = graphOf(board).cycle();
  if (cycle !== undefined) {
    failures.push(`The links form a cycle through the blocks ${quoted(cycle)}.`);
  }
}

function checkStacks(
  board: Board,
  changed: Changed | undefined,
  failures: string[],
): void {
  ids("stack", board.stacks, failures);

  const holder = new Map<string, string>();
  for (const [id, stack] of Object.entries(board.stacks)) {
    if (changed === undefined || changed.stacks.has(id)) {
      const fields = shape(stack, { name: "string", blocks: "array", color: "string or null" });
      if (fields.length > 0) {
        failures.push(`The stack "${id}" ${fields.join(", ")}.`);
        continue;
      }
      if (stack.color !== null && !hexColor.test(stack.color)) {
        failures.push(
          `The stack "${id}" has the colour "${stack.color}", which is no hex colour of six digits, such as "#66c2a5".`,
        );
      }
    }
    for (const block of stack.blocks) {
      if (own(board.blocks, block) === undefined) {
        failures.push(`The stack "${id}" holds block "${block}", which the board lacks.`);
        continue;
      }
      const other = holder.get(block);
      if (other === id) {
        failures.push(`The stack "${id}" holds block "${block}" twice.`);
      } else if (other !== undefined) {
        failures.push(`The block "${block}" sits in the stacks "${other}" and "${id}", where it sits in one.`);
      }
      holder.set(block, id);
    }
  }
}

function checkValues(
  board: Board,
  registrations: Registrations,
  changed: Changed | undefined,
  failures: string[],
): void {
  for (const [id, value] of Object.entries(board.options)) {
    if (changed !== undefined && !changed.options.has(id)) continue;
    const option = registrations.option(id);
    if (option === undefined) {
      failures.push(`The board holds a value of the board option "${id}", which no package registered.`);
      continue;
    }
    for (const failure of option.validate(value)) {
      failures.push(`The value of the board option "${id}" ${failure}`);
    }
  }
}

// Each plugin that a patch added or changed, or every one as the board opens:
// its shape, its class, which a package has to register, and its state against
// the schema of its class's state, as a whole.
// The board holds at most one frontend and one storage plugin, since no opening
// makes a board that holds two.
function checkPlugins(
  board: Board,
  registrations: Registrations,
  changed: Changed | undefined,
  failures: string[],
): void {
  ids("plugin", board.plugins, failures);

  for (const [id, plugin] of Object.entries(board.plugins)) {
    if (changed !== undefined && !changed.plugins.has(id)) continue;
    const fields = shape(plugin, { class: "string", name: "string", state: "value" });
    if (fields.length > 0) {
      failures.push(`The plugin "${id}" ${fields.join(", ")}.`);
      continue;
    }
    const cls = registrations.plugin(plugin.class);
    if (cls === undefined) {
      failures.push(`The plugin "${id}" is of the class "${plugin.class}", which no package registered.`);
      continue;
    }
    for (const failure of cls.validate(plugin.state)) {
      failures.push(`The state of the plugin "${id}" ${failure}`);
    }
  }

  const classOf = (id: string) => registrations.plugin(id)?.def;
  for (const role of ROLES) {
    const held = inRole(board.plugins, classOf, role);
    if (held.length > 1) {
      failures.push(`The board holds the ${ROLE_NAMES[role]} ${quoted(held)}, where it holds one at most.`);
    }
  }
}

const ROLE_NAMES: Readonly<Record<Role, string>> = {
  frontend: "frontends",
  storage: "storage plugins",
};

type Kind = "string" | "string or null" | "object" | "array" | "value";

// What an object of core's lacks or holds beyond its fields, which a delta
// built in plain JS or by a model may get wrong.
function shape(value: unknown, fields: Readonly<Record<string, Kind>>): string[] {
  if (!isObject(value)) return ["is no object"];
  const problems: string[] = [];
  for (const [field, kind] of Object.entries(fields)) {
    if (!fits(value[field], kind)) problems.push(`has no ${kind} as its ${field}`);
  }
  for (const field of Object.keys(value)) {
    if (!Object.hasOwn(fields, field)) problems.push(`holds "${field}", which core does not keep`);
  }
  return problems;
}

function fits(x: unknown, kind: Kind): boolean {
  switch (kind) {
    case "value":
      return x !== undefined;
    case "array":
      return Array.isArray(x);
    case "object":
      return isObject(x);
    case "string or null":
      return x === null || typeof x === "string";
    case "string":
      return typeof x === "string";
  }
}
