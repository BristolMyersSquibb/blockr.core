// The hosts that core gives the modules it mounts inside a front-end's layout,
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
import type { RequestOpts } from "./core-api.ts";
import type { Json } from "./json.ts";

// A block's state: its values for the formals of its class's `expr`.
export type State = Readonly<Record<string, Json>>;

// What a module that core mounts exports as its default, beside the JS API
// major it was built against, as `export const api = 1`: a block class's UI
// module, a board option's UI or renderer, or an output's renderer. Core calls
// render() whenever it mounts the module, with the element to draw in, the
// host, and a signal that it aborts when it unmounts the module. The render()
// function may be async and may return its cleanup, which core runs when it
// aborts the signal. The default may name the board options the module
// requires. Core adds each to the board, with its definition's default where
// the board lacks it, before it calls render().
export interface BlockModule<S extends State = State> {
  readonly options?: readonly OptionId[];
  render(args: { el: HTMLElement; host: BlockHost<S>; signal: AbortSignal }): Rendered;
}

export interface OptionModule<V extends Json = Json> {
  readonly options?: readonly OptionId[];
  render(args: { el: HTMLElement; host: OptionHost<V>; signal: AbortSignal }): Rendered;
}

export interface OutputModule {
  readonly options?: readonly OptionId[];
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
  // block's state. It rejects with what refused it.
  submit(change: Partial<S>): Promise<void>;
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
}

export interface BlockContext {
  // Its name, as "Head".
  readonly name: string;
  // Its status, revision, conditions and summary, as the scheduler has them.
  readonly status: BlockStatus;
  // Its linked inputs, in the order of its class's inputs, whose revisions
  // say when the data changed.
  readonly inputs: readonly { readonly input: string; readonly revision: number }[];
  // The values of the board options that its module names.
  readonly options: Readonly<Record<OptionId, Json>>;
}

// The options of a host's request. The host fills in the blocks it reads,
// from the block's inputs or the output it holds.
export interface HostRequestOpts extends Omit<RequestOpts, "reads"> {
  // A file it sends, such as an upload or the bitmap of a chart.
  file?: File;
}

// The options of an output's request: as a host's, and the scope of the
// request, "class" for one of the output class, as unless given, or
// "renderer" for one of the renderer's own, which the host sends naming the
// renderer.
export interface OutputRequestOpts extends HostRequestOpts {
  scope?: "class" | "renderer";
}

export interface OptionHost<V extends Json = Json> {
  readonly option: OptionId;
  // The board option's value in the board, or else its default, announced
  // through "value" once resolved.
  value(): V;
  on(event: "value", fn: () => void): () => void;
  // Runs core's command that sets a board option.
  submit(value: V): Promise<void>;
  // A request of the board option, never kept.
  request<T = Json>(
    name: string,
    args?: Record<string, Json>,
    opts?: HostRequestOpts,
  ): Promise<T | Blob>;
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
  // The values of the board options that its module names, which the output
  // it holds was asked with, such as the n_rows by which a table pages.
  options(): Readonly<Record<OptionId, Json>>;
  // Whether core is asking R for a newer output.
  loading(): boolean;
  // Why the latest ask failed, while output() keeps what it held.
  error(): Condition | null;
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
  // Reports what the user did on the output, such as a selection, for an
  // extension to act on.
  emit(name: string, payload: Json): void;
  // Draws an output nested in this one, with the renderer that R names in it.
  draw(el: HTMLElement, key: string, output: Json, body?: Blob): Nested;
}

export interface Nested {
  // Hands the nested renderer a new output without mounting it again.
  update(output: Json, body?: Blob): void;
  remove(): void;
}
