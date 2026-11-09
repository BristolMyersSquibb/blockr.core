// The command registry: the edits that menus, shortcuts, plugins and the
// assistant's tools run alike, each a command that a package registers, core's
// own among them. A command reads the board and returns deltas, and the command
// registry applies them through the store as one step of the origin it is
// given, so that every edit, whoever makes it, passes the store's checks.
// The step is one undo step, unless it sets or removes only board option
// values (../store/store.ts).
// Core creates a command registry for each store and hands it to plugins,
// a frontend among them, through `CoreApi`. It ends with the store, as another
// board opens.

import { computed, signal } from "@preact/signals-core";
import type { Lock } from "../protocol/index.ts";
import { isDeltas, isRandId, pluginOf, RAND_IDS } from "../store/index.ts";
import type { BlockMod, Delta, Origin, Patch, RandId, Store } from "../store/index.ts";
import type { Board } from "../types/board.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import { compile, messageOf, packageOf, quoted } from "../util/index.ts";
import type { Validate } from "../util/index.ts";
import { coreCommands } from "./core.ts";

export interface Command<A extends Json = Json, P = A> {
  // Named as a registry id is, `{pkg}-{name}`, such as
  // "blockr.core-remove_block": the package that registers it, and a name
  // unique within that package.
  readonly id: string;
  // What menus and a command palette show.
  readonly label: string;
  // What it does and when to use it, for the assistant's model.
  readonly description?: string;
  // The JSON Schema of its args, for forms and the assistant's tools, against
  // which the command registry checks the args it is given.
  readonly args?: JsonSchema;
  // Such as "Mod+Backspace", which a front-end binds.
  readonly shortcut?: string;
  // Whether it applies to the board with these args, which menus show by
  // greying out what does not.
  enabled?(board: Board, args: A): boolean;
  // What has to wait, such as a layout that elkjs returns as a promise.
  prepare?(board: Board, args: A): Promise<P>;
  // Reads the board and returns the deltas of the edit, changing nothing
  // itself.
  run(board: Board, args: P): Delta | readonly Delta[];
}

export interface Commands {
  // Adds a command, which fails where another has its id.
  register<A extends Json, P = A>(command: Command<A, P>): () => void;
  // The registered commands by id, for menus, a command palette and the
  // assistant's tools.
  registered(): Readonly<Record<string, Command<Json, unknown>>>;
  // Whether the command applies to the current board with these args, which
  // fit the schema of its args.
  enabled(id: string, args?: Json): boolean;
  // Runs the command and applies its deltas as one step of the origin, "user"
  // unless given, or rejects with what refused it, having changed nothing. A
  // command that does not prepare is applied before run() returns.
  run(id: string, args?: Json, origin?: Origin): Promise<void>;
  // The deltas the command returns on the current board, not applied, with
  // the edit ids of the board they were built on, so that the store refuses
  // them once another edit has come first.
  plan(id: string, args?: Json): Promise<Delta[]>;
}

// What the command registry rejects with where it refuses to run a command, or
// a command fails, with each failure in a sentence. What the store refuses it
// rejects with as the store throws it.
export class CommandError extends Error {
  readonly failures: readonly string[];

  constructor(message: string, failures: readonly string[], options?: ErrorOptions) {
    super([message, ...failures.map((failure) => `- ${failure}`)].join("\n"), options);
    this.name = "CommandError";
    this.failures = failures;
  }
}

// The CommandError of a command that refuses to run, with each failure in a
// sentence.
export function refusal(id: string, failures: readonly string[]): CommandError {
  return new CommandError(`The command "${id}" refused to run`, failures);
}

interface Registered {
  readonly command: Command<Json, unknown>;
  readonly validate: Validate | undefined;
}

export interface CommandsOptions {
  // Ends the command registry as it aborts, as another board opens in a new
  // store: from then on the registry runs and plans no command, and drops a
  // command still in its prepare() rather than run it.
  readonly signal?: AbortSignal;
  // The lock that R's reply to `hello` gives, under which core's command for a
  // plugin's state is not enabled, since there only a plugin class's own
  // commands know what the plugin locks.
  readonly lock?: Lock | null;
  // The `rand_id` blockr option, the shape of the ids that core's commands
  // mint, which core asks R for as soon as R has answered its hello. It is
  // "adjective_animal" unless given, and the command registry refuses a value
  // that names no shape.
  readonly randId?: string;
}

export function createCommands(store: Store, options: CommandsOptions = {}): Commands {
  const commands = signal<ReadonlyMap<string, Registered>>(new Map());
  const registered = computed(() =>
    Object.fromEntries([...commands.value].map(([id, { command }]) => [id, command])),
  );
  const ended = (): boolean => options.signal?.aborted === true;

  function register<A extends Json, P>(command: Command<A, P>): () => void {
    const { id } = command;
    if (typeof id !== "string" || !isCommandId(id)) {
      throw new Error(
        `The command id ${JSON.stringify(id)} is not named as a registry id is, "{pkg}-{name}".`,
      );
    }
    if (typeof command.label !== "string" || command.label === "") {
      throw new Error(`The command "${id}" has no label.`);
    }
    if (typeof command.run !== "function") {
      throw new Error(`The command "${id}" has no run().`);
    }
    if (commands.value.has(id)) {
      throw new Error(`The command "${id}" is registered already.`);
    }
    const validate =
      command.args === undefined
        ? undefined
        : compile(`the args of the command "${id}"`, command.args, "fail their schema");

    const item: Registered = { command, validate };
    commands.value = new Map([...commands.value, [id, item]]);
    return () => {
      if (commands.value.get(id) !== item) return;
      const rest = new Map(commands.value);
      rest.delete(id);
      commands.value = rest;
    };
  }

  // The command and its args, once they fit its schema, or a CommandError.
  function lookup(
    id: string,
    given: Json | undefined,
  ): { command: Command<Json, unknown>; args: Json } {
    const item = commands.value.get(id);
    if (item === undefined) {
      throw refusal(id, [`No command "${id}" is registered.`]);
    }
    const args = given ?? {};
    const failures = item.validate?.(args) ?? [];
    if (failures.length > 0) {
      throw refusal(id, failures.map((failure) => `The args ${failure}`));
    }
    return { command: item.command, args };
  }

  function current(id: string): Board {
    if (ended()) {
      throw refusal(id, ["Another board opened in a new store, so this command registry runs no command."]);
    }
    if (!store.hasBoard()) {
      throw refusal(id, ["The store holds no board until it opens one."]);
    }
    return store.board();
  }

  function allow(command: Command<Json, unknown>, board: Board, args: Json): void {
    const enabled = attempt(command, () => command.enabled?.(board, args) ?? true);
    if (!enabled) {
      throw refusal(command.id, [
        `It is not enabled on the board as it is, with the args ${JSON.stringify(args)}.`,
      ]);
    }
  }

  // The deltas that the command returns on the board, with the edit ids the
  // board gives what they change where the command left them out.
  function build(command: Command<Json, unknown>, board: Board, args: unknown): Delta[] {
    const returned = attempt(command, () => command.run(board, args));
    const deltas: readonly Delta[] = isDeltas(returned) ? returned : [returned];
    return deltas.map(withEdits);
  }

  // The deltas of the command on the current board: at once where it does
  // not prepare, so that run() applies them before it returns, and otherwise
  // once prepare() resolves, on the board as it is then, where the command is
  // still enabled and no board has opened in the meantime.
  function deltas(id: string, given: Json | undefined): Delta[] | Promise<Delta[]> {
    const { command, args } = lookup(id, given);
    const board = current(id);
    allow(command, board, args);
    if (command.prepare === undefined) return build(command, board, args);
    return prepared(command, board, args);
  }

  async function prepared(
    command: Command<Json, unknown>,
    board: Board,
    args: Json,
  ): Promise<Delta[]> {
    let input: unknown;
    try {
      input = await command.prepare?.(board, args);
    } catch (error) {
      throw failed(command, error);
    }
    if (ended()) {
      throw refusal(command.id, ["A board opened while it prepared, which drops it."]);
    }
    const now = current(command.id);
    allow(command, now, args);
    return build(command, now, input);
  }

  // The delta with the edit id of each block and each plugin it changes,
  // where it names none, from the board that the command read.
  function withEdits(delta: Delta): Delta {
    const blocks = delta.blocks?.mod;
    const patches = delta.patches;
    if (blocks === undefined && patches === undefined) return delta;

    const edit = <T extends { readonly edit?: number }>(change: T, id: number | undefined): T =>
      change.edit !== undefined || id === undefined ? change : { ...change, edit: id };

    return {
      ...delta,
      ...(blocks === undefined
        ? {}
        : {
            blocks: {
              ...delta.blocks,
              mod: Object.fromEntries(
                Object.entries(blocks).map(([id, mod]): [string, BlockMod] => [
                  id,
                  edit(mod, store.editId({ block: id })),
                ]),
              ),
            },
          }),
      ...(patches === undefined
        ? {}
        : {
            patches: patches.map((patch): Patch => {
              const plugin = pluginOf(patch.path);
              return plugin === undefined ? patch : edit(patch, store.editId({ plugin }));
            }),
          }),
    };
  }

  const registry: Commands = {
    register,
    registered: () => registered.value,
    enabled(id, given) {
      try {
        const { command, args } = lookup(id, given);
        allow(command, current(id), args);
        return true;
      } catch {
        return false;
      }
    },
    async run(id, given, origin = "user") {
      const step = deltas(id, given);
      store.apply(Array.isArray(step) ? step : await step, origin);
    },
    async plan(id, given) {
      return deltas(id, given);
    },
  };

  for (const command of coreCommands(store, {
    locked: (options.lock ?? null) !== null,
    randId: shapeOf(options.randId),
  })) {
    registry.register(command);
  }

  return registry;
}

// The shape of the ids that core's commands mint, which the blockr option
// rand_id names, "adjective_animal" where it is not given. A value that names
// no shape fails, as it does in R's rand_names().
function shapeOf(randId: string | undefined): RandId {
  if (randId === undefined) return "adjective_animal";
  if (isRandId(randId)) return randId;
  throw new RangeError(`The blockr option rand_id is ${JSON.stringify(randId)}, where it takes one of ${quoted(RAND_IDS)}.`);
}

// Whether an id is named as a registry id is: a package name, a dash, and a
// name.
function isCommandId(id: string): boolean {
  const pkg = packageOf(id);
  return pkg !== null && id.length > pkg.length + 1;
}

// What a command's own enabled(), prepare() or run() threw, as a CommandError
// that names the command, unless it is one already.
function failed(command: Command<Json, unknown>, error: unknown): CommandError {
  if (error instanceof CommandError) return error;
  return new CommandError(`The command "${command.id}" failed`, [messageOf(error)], {
    cause: error,
  });
}

function attempt<T>(command: Command<Json, unknown>, fn: () => T): T {
  try {
    return fn();
  } catch (error) {
    throw failed(command, error);
  }
}
