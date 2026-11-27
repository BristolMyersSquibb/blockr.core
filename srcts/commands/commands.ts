// The command registry: the edits that menus, shortcuts, plugins and the
// assistant's tools run alike, each a command that a package registers, core's
// own among them. A command reads the board and returns deltas, and the command
// registry applies them through the store as one step of the origin it is
// given, so that every edit, whoever makes it, passes the store's checks.
// A batch runs as one step too, as the assistant's edit tool runs one.
// The command registry plans each command on the board that the deltas
// of the commands before it leave, which a dry run of the store gives. Then it
// applies the deltas of every command, or none where one command refuses.
// The command registry sets `undo: false` on each delta of a command that has
// `undo: false`, whether the command runs alone or in a batch. The store keeps
// no inverse of such a delta, as it keeps no board option values. So a step
// is one undo step only where the store keeps the inverse of at least one of
// its deltas (../store/store.ts). The command registry's run() resolves with
// whether the step took one, as the store's apply() returns it, so that no
// caller copies the store's rule.
// Beside the board and its args, the command registry hands every command its
// context: what the app fixes for the page.
// Core creates a command registry for each store and hands it to plugins,
// a frontend among them, through `CoreApi`. It ends with the store, as another
// board opens.
//
// The command registry records a span of each command that it runs, as
// Tracing has it (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#tracing),
// from the call of run() until the store has committed and announced the
// command's deltas, or until the command is refused or fails, which marks the
// span failed. The span goes under the span that is active as run() is called,
// so that a plugin that runs a command inside a span of its own, as the
// assistant's tools do, holds the command in its trace. It names the command
// by its package and its id and the step's origin, as "assistant". A batch runs
// in one span, "command batch", which names the packages and the ids of its
// commands in order, since the store applies their deltas as one step. The span is active while
// the command prepares and runs, and while the store applies its deltas, so
// that the scheduler takes it as what gave each block that the deltas change
// its revision, and the span of the block's evaluation links to it.
//
// The command registry logs at error what a command's own preflight(),
// prepare() or run() throws, with its stack, as Logging has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#logging),
// whoever runs the command and whatever its origin. It still rejects with it,
// as a CommandError that names the command, and preflight() still answers
// false. It logs no refusal, whether the command registry, the command or the
// store refuses, since the call hands the refusal to its caller. A command
// refuses with what its context's refusal() makes.

// The API's context is named apart from the context that the command registry
// hands every command.
import { context as otelContext, trace } from "@opentelemetry/api";
import type { Attributes } from "@opentelemetry/api";
import { computed, signal } from "@preact/signals-core";
import { log } from "../log/index.ts";
import type { Lock } from "../protocol/index.ts";
import { asRandId, isDeltas, mintId, pluginClassFromPath, StoreError } from "../store/index.ts";
import type { BlockMod, Delta, Origin, Patch, Store } from "../store/index.ts";
import type { Board, CommandId } from "../types/board.ts";
import type { Notification } from "../types/core-api.ts";
import type { Json, JsonSchema } from "../types/json.ts";
import { failSpan, keepContext, tracer } from "../trace/index.ts";
import { compile, errorMessage, idAttributes, idText, interned, isIdRecord, isObject, isPackageName } from "../util/index.ts";
import type { Validate } from "../util/index.ts";
import { CORE, coreCommands } from "./core.ts";

// What the command registry hands every command beside the board and its
// args: what the app fixes for the page, and what the command refuses with.
// A plugin class's commands read it to refuse what its plugins lock, and to
// mint ids as core's commands do.
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
  // A refusal of the command, with each failure in a sentence, which the
  // command throws to refuse from its preflight(), prepare() or run(), as
  // where its args place a block that the board does not hold. The command
  // registry rejects with the refusal as it is, and logs no line of it, as
  // for its own refusals. It logs at error whatever else the command's own
  // code throws.
  readonly refusal: (failures: readonly string[]) => CommandError;
}

export interface Command<A extends Json = Json, P = A> {
  // Its id within the package that registers it, unique there, such as
  // "remove_block". The command registry holds it under the package and the
  // id, as core.id("blockr.core", "remove_block") names it, so that two
  // packages may give commands of one id.
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
  // Its preflight: whether the command has what it needs on the board as it
  // is, with these args, such as the block that the args name. The command
  // registry runs it before the command's prepare() and again before its
  // run(), and refuses a command that fails it. Menus grey such a command
  // out. A pass promises nothing beyond itself, since run() and the store
  // can still refuse.
  preflight?(board: Board, args: A, context: CommandContext): boolean;
  // False where undo never takes back its deltas, as for a command that
  // changes only what the user looks at, such as the dock's active view. The
  // command registry sets `undo: false` on each of its deltas, whether it runs
  // alone or in a batch. The store keeps no inverse of them, but still applies
  // and announces them as any others.
  readonly undo?: false;
  // What has to wait, such as a layout that elkjs returns as a promise.
  prepare?(board: Board, args: A, context: CommandContext): Promise<P>;
  // Reads the board and returns the deltas of the edit, changing nothing
  // itself.
  run(board: Board, args: P, context: CommandContext): Delta | readonly Delta[];
}

export interface Commands {
  // Adds a command of the package, which fails where the package has given a
  // command of its id already. Core registers its own under blockr.core, and
  // those that a plugin class lists under the class's package.
  register<A extends Json, P = A>(pkg: string, command: Command<A, P>): () => void;
  // The registered commands by id, for menus, a command palette and the
  // assistant's tools, in the order registered.
  registered(): ReadonlyMap<CommandId, Command<Json, unknown>>;
  // Whether the command passes its preflight on the current board with these
  // args, which fit the schema of its args.
  preflight(id: CommandId, args?: Json): boolean;
  // Runs the command and applies its deltas as one step of the origin, "user"
  // unless given, or rejects with a CommandError that says what stopped it,
  // having changed nothing. It resolves with whether the step took an undo
  // step, as the store's apply() returns it: none where the command has
  // `undo: false` or changes nothing besides board options. A command that
  // does not prepare is applied before run() returns.
  // Through `CoreApi`, core tells the user why the command was refused, or
  // hands that notification to the notifier in `opts`, before run() rejects
  // (../core/core.ts). The command registry itself ignores the notifier.
  run(id: CommandId, args?: Json, origin?: Origin, opts?: { notifier?(notifications: readonly Notification[]): void }): Promise<boolean>;
  // Runs the commands of the batch in order, each planned on the board that the
  // deltas of the commands before it leave, and applies the deltas of every
  // command as one step of the origin, "user" unless given. Where a command
  // refuses, it applies none and rejects with a CommandError that gives the
  // command's index. It resolves with whether the step took an undo step, as
  // for a command alone: one where a command without `undo: false` changes
  // something besides board options. Its undo takes back only what those
  // commands changed. A batch whose commands do not prepare is applied before
  // run() returns. Its notifier is as for a command alone.
  run(
    batch: readonly CommandCall[],
    origin?: Origin,
    opts?: { notifier?(notifications: readonly Notification[]): void },
  ): Promise<boolean>;
  // The deltas the command returns on the current board, not applied, with
  // the edit ids of the board they were built on, so that the store refuses
  // them once another edit has come first, and with `undo: false` where the
  // command has it.
  plan(id: CommandId, args?: Json): Promise<Delta[]>;
  // The deltas of the commands of the batch in order, each built on the board
  // that the deltas of the commands before it leave, not applied. It rejects
  // where run() would reject for the batch. The deltas name the edit ids of the
  // board that the batch was planned on.
  plan(batch: readonly CommandCall[]): Promise<Delta[]>;
}

// A command of any package, by its id, with its args: one entry of a batch, as
// { id: core.id("blockr.core", "remove_block"), args: { id: "a" } }.
export interface CommandCall {
  readonly id: CommandId;
  readonly args?: Json;
}

// What the command registry rejects with, whatever stopped a command that it
// runs or plans, alone or as one of a batch. It names the command by its id,
// says in `by` what stopped it, and gives each failure in a sentence. Its
// message says the same: a first line that names the command and what stopped
// it, then a line for each failure.
export class CommandError extends Error {
  // The id of the command that was stopped, such as
  // { package: "blockr.core", id: "remove_block" }. It is left out only where
  // the store refused a batch as a whole, after the dry run of each command's
  // deltas had passed, which the first line says. There, a plugin class's
  // afterStep failed, or returned what the store refuses, and the failures name
  // the class.
  readonly command?: CommandId;
  // What stopped the command:
  // - "registry": the command registry refused to run it, as where no command
  //   has its id, its args fail their schema or the store holds no board.
  // - "preflight": its preflight() answered false. The assistant's edit tool
  //   reads this to tell the model what to check.
  // - "command": the command refused, throwing the refusal() of its context.
  // - "code": the command's own preflight(), prepare() or run() threw
  //   anything else. That is the cause, and the command registry has logged
  //   it at error.
  // - "store": the store refused its deltas. The StoreError is the cause.
  readonly by: "registry" | "preflight" | "command" | "code" | "store";
  readonly failures: readonly string[];
  // The command's place in the batch that run() or plan() was given, counted
  // from 0, where it was stopped as one of a batch.
  readonly index?: number;

  constructor(fields: Pick<CommandError, "command" | "by" | "failures" | "index"> & { readonly cause?: unknown }) {
    super(
      [firstLine(fields), ...fields.failures.map((failure) => `- ${failure}`)].join("\n"),
      fields.cause === undefined ? undefined : { cause: fields.cause },
    );
    this.name = "CommandError";
    this.by = fields.by;
    this.failures = fields.failures;
    if (fields.command !== undefined) this.command = fields.command;
    if (fields.index !== undefined) this.index = fields.index;
  }
}

// The first line of a CommandError's message, which says what stopped the
// command.
function firstLine({ command, by, index }: Pick<CommandError, "command" | "by" | "index">): string {
  if (command === undefined) {
    return by === "store"
      ? "The store refused the batch as a whole, after the deltas of each command had passed"
      : "The command registry refused a command that it cannot name";
  }
  const named = idText(command);
  if (index !== undefined) return `The batch was refused at command ${String(index + 1)}, "${named}"`;
  if (by === "code") return `The command "${named}" failed`;
  if (by === "store") return `The store refused the deltas of the command "${named}"`;
  return `The command "${named}" refused to run`;
}

// The CommandError of a command that refuses to run, with each failure in a
// sentence. By default, the command itself refuses. The command registry
// passes "registry" for its own refusals, and "preflight" where the command's
// preflight() answered false.
export function refusal(
  id: CommandId,
  failures: readonly string[],
  by: "registry" | "preflight" | "command" = "command",
): CommandError {
  return new CommandError({ command: id, by, failures });
}

// Why the command registry drops a command that was still in its prepare() as
// another board opened.
const DROPPED = "A board opened while it prepared, which drops it.";

interface Registered {
  // The command's id, its package and its id within the package, as core's
  // table holds it.
  readonly id: CommandId;
  readonly command: Command<Json, unknown>;
  readonly validate: Validate | undefined;
  // What the command registry hands the command's preflight(), prepare() and
  // run(), whose refusal() names the command.
  readonly context: CommandContext;
}

export interface CommandsOptions {
  // Ends the command registry as it aborts, as another board opens in a new
  // store: from then on the registry runs and plans no command, and drops a
  // command still in its prepare() rather than run it.
  readonly signal?: AbortSignal;
  // The locks that R's reply to `hello` gives, which the context of every
  // command holds. Under the board lock, core's command for a plugin's state
  // fails its preflight, since there only a plugin class's own commands know
  // what the plugin locks. No lock is on unless given.
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
  const commands = signal<ReadonlyMap<CommandId, Registered>>(new Map());
  const registered = computed(
    (): ReadonlyMap<CommandId, Command<Json, unknown>> => new Map([...commands.value].map(([id, { command }]) => [id, command])),
  );
  const ended = (): boolean => options.signal?.aborted === true;
  const randId = asRandId(options.randId);
  // What the context of every command holds, all but its refusal().
  const shared: Omit<CommandContext, "refusal"> = {
    lock: options.lock ?? { board: false, load: false },
    mintId: (taken) => mintId(taken, randId),
    blockrOption: options.blockrOption ?? (() => Promise.resolve(undefined)),
  };

  function register<A extends Json, P>(pkg: string, command: Command<A, P>): () => void {
    if (!isPackageName(pkg)) {
      throw new Error(`A command is registered under the package that gives it, named as R names a package, not ${JSON.stringify(pkg)}.`);
    }
    const given: unknown = command.id;
    if (typeof given !== "string" || given === "") {
      throw new Error(`The command id ${JSON.stringify(given)} of ${pkg} is no non-empty string, as "remove_block".`);
    }
    const id = interned(pkg, given);
    const named = `The command "${idText(id)}"`;
    if (typeof command.label !== "string" || command.label === "") {
      throw new Error(`${named} has no label.`);
    }
    if (typeof command.run !== "function") {
      throw new Error(`${named} has no run().`);
    }
    if (commands.value.has(id)) {
      throw new Error(`${named} is registered already.`);
    }
    const validate =
      command.args === undefined
        ? undefined
        : compile(`the args of the command "${idText(id)}"`, command.args, "fail their schema");

    const item: Registered = {
      id,
      command,
      validate,
      context: { ...shared, refusal: (failures) => refusal(id, failures) },
    };
    commands.value = new Map([...commands.value, [id, item]]);
    return () => {
      if (commands.value.get(id) !== item) return;
      const rest = new Map(commands.value);
      rest.delete(id);
      commands.value = rest;
    };
  }

  // The registered command and its args, once they fit its schema, or a
  // CommandError. A caller in plain JS may name the command by a record of its
  // package and its id, which is not the object that core's table holds, and
  // the registry finds the command all the same.
  function lookup(name: unknown, given: Json | undefined): { item: Registered; args: Json } {
    const id = nameOf(name);
    if (id === undefined) {
      throw new CommandError({
        by: "registry",
        failures: [
          `No command is named ${JSON.stringify(name)}, where a command is named by its id, as core.id("blockr.core", "remove_block") gives it.`,
        ],
      });
    }
    const item = commands.value.get(id);
    if (item === undefined) {
      throw refusal(id, [`No command "${idText(id)}" is registered.`], "registry");
    }
    const args = given ?? {};
    const failures = item.validate?.(args) ?? [];
    if (failures.length > 0) {
      throw refusal(id, failures.map((failure) => `The args ${failure}`), "registry");
    }
    return { item, args };
  }

  function current(id: CommandId): Board {
    if (ended()) {
      throw refusal(id, ["Another board opened in a new store, so this command registry runs no command."], "registry");
    }
    if (!store.hasBoard()) {
      throw refusal(id, ["The store holds no board until it opens one."], "registry");
    }
    return store.board();
  }

  function allow(item: Registered, board: Board, args: Json): void {
    const { command, context } = item;
    const passes = attempt(item, () => command.preflight?.(board, args, context) ?? true);
    if (!passes) {
      throw refusal(
        item.id,
        [`It fails its preflight on the board as it is, with the args ${JSON.stringify(args)}.`],
        "preflight",
      );
    }
  }

  // The deltas that the command returns on the board, with the edit ids the
  // board gives what they change where the command left them out, and with
  // `undo: false` where the command has it.
  function build(item: Registered, board: Board, args: unknown): Delta[] {
    const { command, context } = item;
    const returned = attempt(item, () => command.run(board, args, context));
    const deltas: readonly Delta[] = isDeltas(returned) ? returned : [returned];
    return deltas.map((delta) => withEdits(command.undo === false ? { ...delta, undo: false } : delta));
  }

  // The deltas of the command on the current board: at once where it does
  // not prepare, so that run() applies them before it returns, and otherwise
  // once prepare() resolves, on the board as it is then, where the command
  // still passes its preflight and no board has opened in the meantime.
  function deltas(item: Registered, args: Json): Delta[] | Promise<Delta[]> {
    const board = current(item.id);
    allow(item, board, args);
    if (item.command.prepare === undefined) return build(item, board, args);
    return prepared(item, board, args);
  }

  async function prepared(item: Registered, board: Board, args: Json): Promise<Delta[]> {
    const input = await prepare(item, board, args);
    if (ended()) throw refusal(item.id, [DROPPED], "registry");
    const now = current(item.id);
    allow(item, now, args);
    return build(item, now, input);
  }

  // What the command's prepare() resolves to, or a CommandError that names the
  // command where it rejects.
  async function prepare(item: Registered, board: Board, args: Json): Promise<unknown> {
    try {
      return await item.command.prepare?.(board, args, item.context);
    } catch (error) {
      throw failed(item, error);
    }
  }

  // Plans the commands of a batch in order, and hands their deltas to `then` as
  // soon as the last command's deltas are built. So run() applies the deltas
  // on the board that they were built on.
  // The registry first checks every command's args against its schema. Then
  // it plans each command as it plans one alone: the first on the store's
  // board, and each later one on the board that a dry run of the deltas before
  // it gives. The dry run checks those deltas as apply() would, but calls no
  // afterStep of a plugin's class. A dry run follows the last command too, so
  // that where the store refuses a command's deltas, the refusal names that
  // command. After that, only an afterStep can make the store refuse the
  // batch, which then names no command (runBatch(), below).
  // A command that prepares is built once its prepare() resolves. Where the
  // board has changed meanwhile, the registry plans the batch again from its
  // first command, on the board as it is then. Each command keeps what its
  // prepare() gave, so that none prepares twice, and each runs its preflight()
  // again, as a command alone does once it has prepared.
  // Whatever refuses a command, the store's refusal of its deltas included,
  // rejects as a CommandError that gives the command's index.
  async function planned<T>(batch: readonly CommandCall[], then: (built: Delta[]) => T): Promise<T> {
    // The span of run(), active as planned() is called, which a command's
    // prepare() and `then` run in after an await.
    const within = keepContext();
    const checked = batch.map((call, index) => {
      // The command's name is the call's id, without its args.
      const name: unknown = isObject(call) ? call.id : call;
      try {
        return lookup(name, call.args);
      } catch (error) {
        throw inBatch(index, nameOf(name), error);
      }
    });
    const [first] = checked;
    if (first === undefined) return then([]);

    // What each command that prepares got from its prepare(), by its index.
    const inputs = new Map<number, unknown>();
    let built: Delta[] = [];
    let again = true;
    while (again) {
      again = false;
      built = [];
      let start: Board;
      try {
        start = current(first.item.id);
      } catch (error) {
        throw inBatch(0, first.item.id, error);
      }
      let board = start;
      for (const [index, { item, args }] of checked.entries()) {
        try {
          allow(item, board, args);
          if (item.command.prepare !== undefined && !inputs.has(index)) {
            inputs.set(index, await within(() => prepare(item, board, args)));
            if (ended()) throw refusal(item.id, [DROPPED], "registry");
            if (store.board() !== start) {
              again = true;
              break;
            }
          }
          built.push(...build(item, board, inputs.has(index) ? inputs.get(index) : args));
          board = byStore(item.id, () => store.dryRun(built));
        } catch (error) {
          throw inBatch(index, item.id, error);
        }
      }
    }
    return within(() => then(built));
  }

  // Applies the deltas of the commands of a batch as one step of the origin.
  // The batch runs in one span, which names the packages and the ids of its
  // commands in order, since the store applies their deltas at once.
  // The store can still refuse the step once the dry run of each command's
  // deltas has passed. The store's apply() runs the afterStep of each plugin's
  // class, and its dry run does not. That refusal names no command, since an
  // afterStep gets only the boards before and after the step as a whole.
  // Where an afterStep threw, the store has logged that at error
  // (../store/expand.ts).
  function runBatch(batch: readonly CommandCall[], origin: Origin): Promise<boolean> {
    // A caller in plain JS may give a call that names no command, which
    // planned() refuses.
    const names = batch.map((call) => nameOf(isObject(call) ? call.id : undefined));
    const attributes = {
      "blockr.command.packages": names.map((name) => name?.package),
      "blockr.command.ids": names.map((name) => name?.id),
      "blockr.delta.origin": origin,
    };
    return traced("command batch", attributes, () => planned(batch, (built) => byStore(undefined, () => store.apply(built, origin))));
  }

  // Runs a command, or a batch, in a span of its own, as the head
  // of this file has it, and resolves with what `run` resolves with. The span
  // is active as `run` is called, and `run` keeps it active after an await,
  // as keepContext() keeps it. A refusal or a failure marks the span failed.
  async function traced<T>(name: string, attributes: Attributes, run: () => Promise<T>): Promise<T> {
    const span = tracer().startSpan(name, { attributes });
    try {
      return await otelContext.with(trace.setSpan(otelContext.active(), span), run);
    } catch (error) {
      failSpan(span, error);
      throw error;
    } finally {
      span.end();
    }
  }

  // The delta with the edit id of each block and each plugin it changes,
  // where it names none, as the store holds it. For a command alone, that is
  // the board that the command read. For a command of a batch, it is the board
  // that the batch was planned on, as the store checks every delta of a step
  // against the edit ids of the board that the step started on. So a block
  // that an earlier command of the batch adds gets no edit id.
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
    preflight(id, given) {
      try {
        const { item, args } = lookup(id, given);
        allow(item, current(item.id), args);
        return true;
      } catch {
        return false;
      }
    },
    // A batch comes with its origin second, in place of the args, and its
    // options third, in place of the origin. The command registry ignores the
    // notifier in the options, which CoreApi acts on (../core/core.ts).
    async run(
      target: CommandId | readonly CommandCall[],
      given?: Json,
      third?: Origin | { notifier?(notifications: readonly Notification[]): void },
    ) {
      if (isBatch(target)) return runBatch(target, typeof given === "string" ? given : "user");
      const origin = typeof third === "string" ? third : "user";
      const named = nameOf(target);
      const attributes = { ...(named === undefined ? {} : idAttributes("blockr.command.id", named)), "blockr.delta.origin": origin };
      return traced(`command ${named === undefined ? "unnamed" : named.id}`, attributes, async () => {
        const within = keepContext();
        const { item, args } = lookup(target, given);
        const step = deltas(item, args);
        const ready = Array.isArray(step) ? step : await step;
        return within(() => byStore(item.id, () => store.apply(ready, origin)));
      });
    },
    async plan(target: CommandId | readonly CommandCall[], given?: Json) {
      if (isBatch(target)) return planned(target, (built) => built);
      const { item, args } = lookup(target, given);
      return deltas(item, args);
    },
  };

  for (const command of coreCommands(store)) {
    registry.register(CORE, command);
  }

  return registry;
}

// The id of the command that a caller names, as core's table holds it, or
// undefined where the caller names it by no package and id, as a caller in
// plain JS may.
function nameOf(name: unknown): CommandId | undefined {
  return isIdRecord(name) ? interned(name.package, name.id) : undefined;
}

// Whether run() or plan() was given a batch rather than a command alone.
function isBatch(target: CommandId | readonly CommandCall[]): target is readonly CommandCall[] {
  return Array.isArray(target);
}

// Whether the command registry logged what the error reports already: what a
// command's own code threw, alone or as one of a batch. So CoreApi does not
// log it again as it tells the user why an edit failed (../core/core.ts).
export function isLogged(error: unknown): boolean {
  return error instanceof CommandError && error.by === "code";
}

// The CommandError with which the command registry refuses a batch, where a
// CommandError stopped the command at the index. It says what that error
// says, and gives the index. Any other error is a bug, and the batch rejects
// with it as it is, as a command alone does.
function inBatch(index: number, id: CommandId | undefined, error: unknown): unknown {
  if (!(error instanceof CommandError)) return error;
  return new CommandError({ command: id, by: error.by, failures: error.failures, index, cause: error.cause });
}

// What `fn` returns, which hands the store the deltas of the command whose id
// is given, or of a batch as a whole. Where the store refuses them, it throws
// a CommandError whose `by` is "store", with the StoreError as its cause.
function byStore<T>(id: CommandId | undefined, fn: () => T): T {
  try {
    return fn();
  } catch (error) {
    if (!(error instanceof StoreError)) throw error;
    throw new CommandError({ command: id, by: "store", failures: error.failures, cause: error });
  }
}

// What a command's own preflight(), prepare() or run() threw, as a CommandError
// that names the command, unless it is one already, as a refusal is. It logs
// the CommandError at error, whose cause holds the stack of what the command
// threw.
function failed(item: Registered, error: unknown): CommandError {
  if (error instanceof CommandError) return error;
  const failure = new CommandError({ command: item.id, by: "code", failures: [errorMessage(error)], cause: error });
  log.error(failure);
  return failure;
}

function attempt<T>(item: Registered, fn: () => T): T {
  try {
    return fn();
  } catch (error) {
    throw failed(item, error);
  }
}
