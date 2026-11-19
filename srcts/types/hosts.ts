// The hosts that core gives the modules it mounts inside a frontend's layout,
// each narrowed to the one thing the module draws: a block's UI gets a
// BlockHost, a board option's UI or renderer an OptionHost, and an output's
// renderer an OutputHost. A module gets everything it shares with the page
// through its host, but for a library it imports as it loads, such as React
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract).
//
// A host follows changes through a getter and a subscription. The getter
// returns the same object until the event that announces a change fires, since
// React's useSyncExternalStore compares what it returns by identity.

import type { Condition } from "../protocol/index.ts";
import type { BlockStatus } from "../scheduler/index.ts";
import type { BlockId, OptionId, RegistryId } from "./board.ts";
import type { CoreFailure, Notification, Raised, RequestOpts } from "./core-api.ts";
import type { Json } from "./json.ts";
import type { Log } from "./log.ts";

// A block's state: its values for the formals of its class's `expr`.
export type State = Readonly<Record<string, Json>>;

// What a module that core mounts exports as its default, beside the JS API
// major it was built against, as `export const api = 1`: a block class's UI
// module, a board option's UI or renderer, or an output's renderer. Core calls
// render() whenever it mounts the module, with the element to draw in, the
// host, and a signal that it aborts when it unmounts the module. The render()
// function may be async and may return its cleanup, which core runs when it
// aborts the signal. No module names the board options that it reads: R names
// them in its reply to `hello`, as a block class names those of its UI module
// and its requests, and an output family those of its outputs, which every
// renderer of them reads. Core adds each to the board, with its definition's
// default where the board lacks it, before it calls render(), and the host
// gives the module their values through option(), which throws for any other
// id.
export interface BlockModule<S extends State = State> {
  render(args: { el: HTMLElement; host: BlockHost<S>; signal: AbortSignal }): Rendered;
}

// A board option's UI, or the renderer that the app names for it, reads its
// own board option's value alone, so it names no board options.
export interface OptionModule<V extends Json = Json> {
  render(args: { el: HTMLElement; host: OptionHost<V>; signal: AbortSignal }): Rendered;
}

export interface OutputModule {
  render(args: { el: HTMLElement; host: OutputHost; signal: AbortSignal }): Rendered;
}

// What render() returns: its cleanup, or nothing, at once or once it resolves.
export type Rendered = (() => void) | undefined | Promise<(() => void) | undefined>;

export interface BlockHost<S extends State = State> {
  readonly block: BlockId;
  // Its block class, since one module may serve several.
  readonly class: RegistryId;
  // The block's current state.
  state(): S;
  // What the client knows of the block besides its state.
  context(): BlockContext;
  on(event: "state" | "context", fn: () => void): () => void;
  // One edit, on a user's action only, through core's command that sets the
  // block's state. It rejects with what refused it. Core also tells the user
  // why, through a notification of the code `board_edit_failed` about the
  // block, unless the module takes it through the notifier in `opts`, as
  // core's inputs do, which show why below themselves (./core-api.ts).
  // A state arg that the change gives as undefined, it unsets, as where the
  // user clears an arg without a default, which then stays out of the block's
  // state until something sets it.
  submit(change: Partial<S>, opts?: { notifier?(notifications: readonly Notification[]): void }): Promise<void>;
  // A request of the block class, with state args and parameters. It resolves
  // with the answer as R writes it, every vector as an array, or with a File
  // for a download.
  request<T = Json>(
    name: string,
    args?: Partial<S> & Record<string, Json>,
    opts?: HostRequestOpts,
  ): Promise<T | Blob>;
  // A one-time evaluation of the block with parameters, which its
  // block_eval() method reads, such as a save button's `{ save: true }`.
  evaluate(params: Record<string, Json>): Promise<BlockStatus>;
  // A module another package offers, by id, such as "blockr.dplyr-filter".
  module<T = unknown>(id: string): Promise<T>;
  // Raises a notification of the module's, which concerns the block unless it
  // names another board object, and holds until the function that it returns
  // runs or the mount ends, as CoreApi's notify() raises a plugin's
  // (./core-api.ts).
  notify(notification: Raised): () => void;
  // A log whose lines name the package of the block's class, or none for a
  // class that is defined outside a package, as CoreApi's log() gives a
  // plugin's.
  readonly log: Log;
}

export interface BlockContext {
  // Its name, as "Head".
  readonly name: string;
  // Its status, revision, conditions and summary, as the scheduler has them.
  readonly status: BlockStatus;
  // Its linked inputs, in the order of its class's inputs, whose revisions
  // say when the data changed.
  readonly inputs: readonly { readonly input: string; readonly revision: number }[];
  // The value of a board option that its class reads, as the board holds it:
  // one that the class names in `options` in R, or that a request of the
  // class reads. It throws for any other id, a misspelled one included, naming
  // the class and the id.
  option(id: OptionId): Json;
}

// The options of a host's request, such as the file of an upload or the bitmap
// of a chart. The host fills in the blocks it reads, from the block's inputs or
// the output it holds.
// The conditions of R's reply raise notifications about the module's block or
// board option, which the frontend shows, as on the block's card, unless the
// module takes them through the request's notifier, as a module that shows
// them itself does (./core-api.ts).
export type HostRequestOpts = Omit<RequestOpts, "reads">;

// The options of an output's request: as a host's, and the scope of the
// request, "class" for one of the output class, as unless given, or
// "renderer" for one of the renderer's own, which the host sends naming the
// renderer.
export interface OutputRequestOpts extends HostRequestOpts {
  scope?: "class" | "renderer";
}

export interface OptionHost<V extends Json = Json> {
  readonly option: OptionId;
  // The board option's value in the board, which core adds to the board with
  // the board option's default before it calls render(), where the board
  // lacks it. The host announces each change of it through "value".
  value(): V;
  on(event: "value", fn: () => void): () => void;
  // Runs core's command that sets a board option. It rejects with what
  // refused it, which core also raises, about the board option, unless the
  // notifier in `opts` takes it, as for a block's submit().
  submit(value: V, opts?: { notifier?(notifications: readonly Notification[]): void }): Promise<void>;
  // A request of the board option, never kept.
  request<T = Json>(
    name: string,
    args?: Record<string, Json>,
    opts?: HostRequestOpts,
  ): Promise<T | Blob>;
  // Raises a notification of the module's, which concerns the board option
  // unless it names another board object, and holds until the function that
  // it returns runs or the mount ends.
  notify(notification: Raised): () => void;
  // A log whose lines name the package of the module: the renderer's, or else
  // the package that defines the board option.
  readonly log: Log;
}

export interface OutputHost {
  readonly block: BlockId;
  // The output it holds, which its block class's coercion made from the
  // block's result.
  output(): Json;
  // The binary body it came with, if any, such as a plot's PNG.
  body(): Blob | null;
  // The parameters that the output it holds was made with, such as a table's
  // page and sort.
  params(): Json;
  // The value of a board option that the output family of its block's class
  // names for its outputs, as the output it holds was asked with. It throws
  // for any other id, a misspelled one included, naming the block's class, the
  // renderer and the id.
  option(id: OptionId): Json;
  // Whether core is asking R for a newer output.
  loading(): boolean;
  // Why the latest ask failed, while output() keeps what it held: R's
  // condition where R's error reply holds one, and core's failure otherwise.
  // Core raises it as a notification about the block too, which the frontend
  // places, so a renderer need not show it.
  error(): Condition | CoreFailure | null;
  on(event: "output", fn: () => void): () => void;
  // Asks R for an output made with these, which params() returns once it
  // arrives.
  setParams(params: Json): void;
  // A request of its output class, or with the scope "renderer" one of the
  // renderer's own, which R runs with the result that the output was made
  // from. The host holds it while the block reads stale or unevaluated, sends
  // it for the output of the block's current revision once core holds that
  // output, and rejects it at once where the block reads failed, invalid or
  // waiting, with the block's status.
  request<T = Json>(
    name: string,
    args?: Record<string, Json>,
    opts?: OutputRequestOpts,
  ): Promise<T | Blob>;
  // Reports what the user did on the output, such as a selection, for a
  // plugin to act on.
  emit(name: string, payload: Json): void;
  // Raises a notification of the renderer's, which comes from the block,
  // naming the renderer, concerns the block unless it names another board
  // object, and holds until the function that it returns runs or the renderer
  // is unmounted.
  notify(notification: Raised): () => void;
  // A log whose lines name the package of the renderer.
  readonly log: Log;
}
