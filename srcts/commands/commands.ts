// The command registry: the edits that menus, shortcuts, plugins and the
// assistant's tools run alike, each a command that a package registers, core's
// own among them. A command reads the board and returns deltas, and the command
// registry applies them through the store as one step of the origin it is
// given, so that every edit, whoever makes it, passes the store's checks.
// The step is one undo step, unless the command has `undo: false` or the step
// sets or removes only board option values (../store/store.ts).
// A list of commands runs as one step too, as the assistant's edit tool runs
// one. The command registry plans each command on the board that the deltas
// of the commands before it leave, which a dry run of the store gives. Then it
// applies the deltas of every command, or none where one command refuses.
// That step is one undo step unless every command of the list has
// `undo: false`.
// Beside the board and its args, the command registry hands every command its
// context: what the app fixes for the page.
// Core creates a command registry for each store and hands it to plugins,
// a frontend among them, through `CoreApi`. It ends with the store, as another
// board opens.

import { computed, signal } from "@preact/signals-core";
import type { Lock } from "../protocol/index.ts";
import { isDeltas, isRandId, mintId, pluginClassFromPath, RAND_IDS, StoreError } from "../store/index.ts";
import type { BlockMod, Delta, Origin, Patch, RandId, Store } from "../store/index.ts";
import type { Board } from "../types/board.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import { compile, errorMessage, packageFromId, quoted } from "../util/index.ts";
import type { Validate } from "../util/index.ts";
import { coreCommands } from "./core.ts";

// What the command registry hands every command beside the board and its
// args: what the app fixes for the page. A plugin class's commands read it to
// refuse what its plugins lock, and to mint ids as core's commands do.
export interface CommandContext {
  // The locks that R's reply to `hello` gives: whether the board lock and the
  // load lock are on.
  readonly lock: Lock;
  // A new id, such as "calm_heron", that `taken` does not hold, in the shape
  // that the blockr option rand_id names.
  readonly mintId: (taken: (id: string) => boolean) => string;
  // A blockr option as R sent it, or undefined where nothing sets it, as
  // CoreApi.blockrOption() reads it. It returns a promise, so a command reads
  // a blockr option in its prepare().
  readonly blockrOption: (name: string) => Promise<Json | undefined>;
}

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
  // Such as "Mod+Backspace", which a frontend binds.
  readonly shortcut?: string;
  // Whether it applies to the board with these args, which menus show by
  // greying out what does not.
  enabled?(board: Board, args: A, context: CommandContext): boolean;
  // False where its deltas take no undo step, as for a command that changes
  // only what the user looks at, such as the dock's active view. The store
  // still applies and announces them as any others.
  readonly undo?: false;
  // What has to wait, such as a layout that elkjs returns as a promise.
  prepare?(board: Board, args: A, context: CommandContext): Promise<P>;
  // Reads the board and returns the deltas of the edit, changing nothing
  // itself.
  run(board: Board, args: P, context: CommandContext): Delta | readonly Delta[];
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
  // unless given, or rejects with what refused it, having changed nothing. The
  // step is an undo step unless the command has `undo: false`. A command that
  // does not prepare is applied before run() returns.
  run(id: string, args?: Json, origin?: Origin): Promise<void>;
  // Runs the commands of the list in order, each planned on the board that the
  // deltas of the commands before it leave, and applies the deltas of every
  // command as one step of the origin, "user" unless given. Where a command
  // refuses, it applies none and rejects with a CommandError that gives the
  // command's index. The step is an undo step unless every command of the
  // list has `undo: false`. A list whose commands do not prepare is applied
  // before run() returns.
  run(list: readonly { readonly id: string; readonly args?: Json }[], origin?: Origin): Promise<void>;
  // The deltas the command returns on the current board, not applied, with
  // the edit ids of the board they were built on, so that the store refuses
  // them once another edit has come first.
  plan(id: string, args?: Json): Promise<Delta[]>;
  // The deltas of the commands of the list in order, each built on the board
  // that the deltas of the commands before it leave, not applied. It rejects
  // where run() would reject for the list. The deltas name the edit ids of the
  // board that the list was planned on.
  plan(list: readonly { readonly id: string; readonly args?: Json }[]): Promise<Delta[]>;
}

// What the command registry rejects with where it refuses to run a command, or
// a command fails, with each failure in a sentence. What the store refuses it
// rejects with as the store throws it, but for a command of a list.
// Where it refuses a list, the CommandError gives the index of the command
// that refused, and has what refused it as its cause: the command's own
// CommandError, or the StoreError with which the store refused its deltas.
export class CommandError extends Error {
  readonly failures: readonly string[];
  // The command's place in the list that run() or plan() was given, counted
  // from 0, where the command refused as one of a list.
  readonly index?: number;
  // False where the command's enabled() returned false, and left out
  // otherwise, as a command's `undo` is false only where it takes no undo
  // step. The assistant's edit tool reads it to tell the model what to check.
  readonly enabled?: false;

  constructor(
    message: string,
    failures: readonly string[],
    options?: ErrorOptions & { readonly index?: number; readonly enabled?: false },
  ) {
    super([message, ...failures.map((failure) => `- ${failure}`)].join("\n"), options);
    this.name = "CommandError";
    this.failures = failures;
    if (options?.index !== undefined) this.index = options.index;
    if (options?.enabled === false) this.enabled = false;
  }
}

// The CommandError of a command that refuses to run, with each failure in a
// sentence.
export function refusal(
  id: string,
  failures: readonly string[],
  options?: { readonly enabled?: false },
): CommandError {
  return new CommandError(`The command "${id}" refused to run`, failures, options);
}

// Why the command registry drops a command that was still in its prepare() as
// another board opened.
const DROPPED = "A board opened while it prepared, which drops it.";

interface Registered {
  readonly command: Command<Json, unknown>;
  readonly validate: Validate | undefined;
}

export interface CommandsOptions {
  // Ends the command registry as it aborts, as another board opens in a new
  // store: from then on the registry runs and plans no command, and drops a
  // command still in its prepare() rather than run it.
  readonly signal?: AbortSignal;
  // The locks that R's reply to `hello` gives, which the context of every
  // command holds. Under the board lock, core's command for a plugin's state
  // is not enabled, since there only a plugin class's own commands know what
  // the plugin locks. No lock is on unless given.
  readonly lock?: Lock;
  // The `rand_id` blockr option as R sent it, the shape of the ids that the
  // context of every command mints, which core asks R for as soon as R has
  // answered its hello.
  // It is "adjective_animal" unless given, and the command registry refuses a
  // value that names no shape.
  readonly randId?: Json | undefined;
  // Reads a blockr option for the context of every command, as
  // CoreApi.blockrOption() does. Unless given, nothing sets a blockr option.
  readonly blockrOption?: (name: string) => Promise<Json | undefined>;
}

export function createCommands(store: Store, options: CommandsOptions = {}): Commands {
  const commands = signal<ReadonlyMap<string, Registered>>(new Map());
  const registered = computed(() =>
    Object.fromEntries([...commands.value].map(([id, { command }]) => [id, command])),
  );
  const ended = (): boolean => options.signal?.aborted === true;
  const randId = options.randId === undefined ? "adjective_animal" : asRandId(options.randId);
  const context: CommandContext = {
    lock: options.lock ?? { board: false, load: false },
    mintId: (taken) => mintId(taken, randId),
    blockrOption: options.blockrOption ?? (() => Promise.resolve(undefined)),
  };

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
    const enabled = attempt(command, () => command.enabled?.(board, args, context) ?? true);
    if (!enabled) {
      throw refusal(
        command.id,
        [`It is not enabled on the board as it is, with the args ${JSON.stringify(args)}.`],
        { enabled: false },
      );
    }
  }

  // The deltas that the command returns on the board, with the edit ids the
  // board gives what they change where the command left them out.
  function build(command: Command<Json, unknown>, board: Board, args: unknown): Delta[] {
    const returned = attempt(command, () => command.run(board, args, context));
    const deltas: readonly Delta[] = isDeltas(returned) ? returned : [returned];
    return deltas.map(withEdits);
  }

  // The deltas of the command on the current board: at once where it does
  // not prepare, so that run() applies them before it returns, and otherwise
  // once prepare() resolves, on the board as it is then, where the command is
  // still enabled and no board has opened in the meantime.
  function deltas(command: Command<Json, unknown>, args: Json): Delta[] | Promise<Delta[]> {
    const board = current(command.id);
    allow(command, board, args);
    if (command.prepare === undefined) return build(command, board, args);
    return prepared(command, board, args);
  }

  async function prepared(
    command: Command<Json, unknown>,
    board: Board,
    args: Json,
  ): Promise<Delta[]> {
    const input = await prepare(command, board, args);
    if (ended()) throw refusal(command.id, [DROPPED]);
    const now = current(command.id);
    allow(command, now, args);
    return build(command, now, input);
  }

  // What the command's prepare() resolves to, or a CommandError that names the
  // command where it rejects.
  async function prepare(command: Command<Json, unknown>, board: Board, args: Json): Promise<unknown> {
    try {
      return await command.prepare?.(board, args, context);
    } catch (error) {
      throw failed(command, error);
    }
  }

  // Plans the commands of a list in order, and hands their deltas to `then`,
  // with the commands, as soon as the last command's deltas are built. So
  // run() applies the deltas on the board that they were built on.
  // The registry first checks every command's args against its schema. Then
  // it plans each command as it plans one alone: the first on the store's
  // board, and each later one on the board that a dry run of the deltas before
  // it gives. The dry run checks those deltas as apply() would. A dry run
  // follows the last command too, so that a refusal by the store always names
  // the command whose deltas it refused.
  // A command that prepares is built once its prepare() resolves. Where the
  // board has changed meanwhile, the registry plans the list again from its
  // first command, on the board as it is then. Each command keeps what its
  // prepare() gave, so that none prepares twice, and each is checked again
  // with its enabled(), as a command alone is once it has prepared.
  // Whatever refuses a command, the store's refusal of its deltas included,
  // rejects as a CommandError that gives the command's index.
  async function planned<T>(
    list: readonly { readonly id: string; readonly args?: Json }[],
    then: (built: Delta[], listed: readonly Command<Json, unknown>[]) => T,
  ): Promise<T> {
    const checked = list.map(({ id, args }, index) => {
      try {
        return lookup(id, args);
      } catch (error) {
        throw inList(index, id, error);
      }
    });
    const [first] = checked;
    if (first === undefined) return then([], []);

    // What each command that prepares got from its prepare(), by its index.
    const inputs = new Map<number, unknown>();
    let built: Delta[] = [];
    let again = true;
    while (again) {
      again = false;
      built = [];
      let start: Board;
      try {
        start = current(first.command.id);
      } catch (error) {
        throw inList(0, first.command.id, error);
      }
      let board = start;
      for (const [index, { command, args }] of checked.entries()) {
        try {
          allow(command, board, args);
          if (command.prepare !== undefined && !inputs.has(index)) {
            inputs.set(index, await prepare(command, board, args));
            if (ended()) throw refusal(command.id, [DROPPED]);
            if (store.board() !== start) {
              again = true;
              break;
            }
          }
          built.push(...build(command, board, inputs.has(index) ? inputs.get(index) : args));
          board = store.dryRun(built);
        } catch (error) {
          throw inList(index, command.id, error);
        }
      }
    }
    return then(built, checked.map(({ command }) => command));
  }

  // Applies the deltas of the commands of a list as one step of the origin,
  // which takes no undo step where every command of the list has
  // `undo: false`.
  function runList(list: readonly { readonly id: string; readonly args?: Json }[], origin: Origin): Promise<void> {
    return planned(list, (built, listed) => {
      const undo = listed.every((command) => command.undo === false) ? false : undefined;
      store.apply(built, origin, { undo });
    });
  }

  // The delta with the edit id of each block and each plugin it changes,
  // where it names none, as the store holds it. For a command alone, that is
  // the board that the command read. For a command of a list, it is the board
  // that the list was planned on, as the store checks every delta of a step
  // against the edit ids of the board that the step started on. So a block
  // that an earlier command of the list adds gets no edit id.
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
              const plugin = pluginClassFromPath(patch.path);
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
    // A list comes with its origin second, in place of the args.
    async run(
      target: string | readonly { readonly id: string; readonly args?: Json }[],
      given?: Json,
      origin: Origin = "user",
    ) {
      if (typeof target !== "string") return runList(target, typeof given === "string" ? given : "user");
      const { command, args } = lookup(target, given);
      const step = deltas(command, args);
      store.apply(Array.isArray(step) ? step : await step, origin, { undo: command.undo });
    },
    async plan(target: string | readonly { readonly id: string; readonly args?: Json }[], given?: Json) {
      if (typeof target !== "string") return planned(target, (built) => built);
      const { command, args } = lookup(target, given);
      return deltas(command, args);
    },
  };

  for (const command of coreCommands(store)) {
    registry.register(command);
  }

  return registry;
}

// The blockr option rand_id as the shape of the ids that core's commands
// mint. A value that names no shape fails, as it does in R's rand_names(), a
// value that is no string among them.
function asRandId(randId: Json): RandId {
  if (isRandId(randId)) return randId;
  throw new RangeError(`The blockr option rand_id is ${JSON.stringify(randId)}, where it takes one of ${quoted(RAND_IDS)}.`);
}

// Whether an id is named as a registry id is: a package name, a dash, and a
// name.
function isCommandId(id: string): boolean {
  const pkg = packageFromId(id);
  return pkg !== null && id.length > pkg.length + 1;
}

// The CommandError with which the command registry refuses a list, for what
// refused the command at the index: the command's own CommandError, or the
// StoreError with which the store refused its deltas. It keeps that error as
// its cause, with its failures.
function inList(index: number, id: string, error: unknown): CommandError {
  const named = error instanceof CommandError || error instanceof StoreError;
  return new CommandError(
    `The list of commands was refused at command ${String(index + 1)}, "${id}"`,
    named ? error.failures : [errorMessage(error)],
    { cause: error, index, enabled: error instanceof CommandError ? error.enabled : undefined },
  );
}

// What a command's own enabled(), prepare() or run() threw, as a CommandError
// that names the command, unless it is one already.
function failed(command: Command<Json, unknown>, error: unknown): CommandError {
  if (error instanceof CommandError) return error;
  return new CommandError(`The command "${command.id}" failed`, [errorMessage(error)], {
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
