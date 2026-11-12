// The messages of the wire protocol between the client and R, as
// inst/protocol/README.md gives them, which the fixtures in
// inst/protocol/fixtures are checked against.

import type { BlockId, JsonBoard, OptionId, PluginClassId, RegistryId } from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { BlockOutput } from "../types/outputs.ts";
import type { RegistryEntry } from "../types/registry.ts";

// The version of the protocol that the client speaks. R's protocol_version()
// gives the same, as the fixtures check in both test suites.
export const PROTOCOL_VERSION = "1.0";

export type Kind =
  | "hello"
  | "blockr_options"
  | "evaluate"
  | "request"
  | "output"
  | "cancel"
  | "forget";

// Optional behavior, which both sides use only where both named it.
export type Capability = "cancel" | "interrupt" | "progress";

export type ReplyStatus = "ok" | "error" | "cancelled" | "invalid" | "missing";

// A message is an array, of requests from the client or of replies and
// progress notes from R, so that several travel together.
export type ClientMessage = Request[];
export type RMessage = (Reply | Progress)[];

export type Request =
  | HelloRequest
  | BlockrOptionsRequest
  | EvaluateRequest
  | BlockClassRequest
  | BoardOptionRequest
  | PluginRequest
  | OutputClassRequest
  | RendererRequest
  | OutputRequest
  | CancelRequest
  | ForgetRequest;

export type Reply =
  | HelloReply
  | BlockrOptionsReply
  | EvaluateReply
  | RequestReply
  | OutputReply
  | ErrorReply
  | CancelledReply
  | MissingReply;

// A note that R sends for a request while it runs, by the request's id.
export interface Progress {
  id: string;
  // The share of the work done, from 0 to 1, and what it is doing.
  progress: { value?: number; message?: string };
}

// The handshake

export interface HelloRequest {
  id: string;
  kind: "hello";
  protocol: string;
  capabilities: Capability[];
}

export interface HelloReply {
  id: string;
  status: "ok";
  protocol: string;
  capabilities: Capability[];
  // The version of each package that R has loaded and the client needs to know
  // of.
  packages: Record<string, string>;
  block_registry: Record<RegistryId, RegistryEntry>;
  // Where each module that the loaded packages offer each other's UIs is
  // served, by the module's id, as "blockr.core-controls", which
  // `host.module()` and `CoreApi.module()` load.
  modules: Record<string, { module: string; css?: string }>;
  // The app's plugins, which run on every board that the app opens, each
  // under the id of its class, in the order of the rows that `serve()` takes,
  // and none for a block that `serve()` serves alone.
  plugins: Record<PluginClassId, PluginEntry>;
  // What the app's frontend reads of the arguments that `serve()` hands it,
  // by name, as the method of R's `frontend_args()` for the frontend's class
  // returns them, which core passes to the frontend's render() in `args`. It
  // is empty for core's basic frontend, which takes none, and for a block that
  // `serve()` serves alone, which has no frontend.
  frontend: Record<string, Json>;
  // The board options that core and the packages that the app uses list in
  // R, each under its id.
  options: Record<OptionId, OptionEntry>;
  // The renderers that the app names for board options, by the renderer's id.
  renderers: Record<string, Renderer>;
  // The locks that the app sets.
  lock: Lock;
  // The opening board, as a JSON board: the one an app script built, which R
  // wrote with every id, or the one that `serve()` was given, which R passes on
  // as it is.
  board: JsonBoard;
  // For a block that `serve()` serves alone, the inputs that R binds itself
  // to the data it was given, which the client counts as linked.
  bound?: string[];
}

// One of the app's plugins, under the id of its class in the reply to `hello`:
// the name of its class, which titles its panel, as "Document", the version
// of its class's state, which the class's method of R's `plugin_version()`
// gives, 1 for a class without one, and where R serves the module of its
// class, whose default export is the class's PluginClass without its id.
export interface PluginEntry {
  name: string;
  version: number;
  module: string;
  css?: string;
}

// A board option, under its id in the reply to `hello`: the package that lists
// it in R, where R serves its module, whose default export is the board
// option's Option without its id, and the version of the shape of its value,
// which the method of R's `board_option_version()` for the board option gives,
// 1 for one without.
export interface OptionEntry {
  package: string;
  module: string;
  css?: string;
  version: number;
}

// A renderer that the app names for a board option, which draws the board
// option's value in place of its UI, while the renderer of an output comes in
// the output itself.
export interface Renderer {
  module: string;
  css?: string;
  option: OptionId;
}

// The locks that the app sets, each true where the app sets it: the board lock,
// the blockr option `locked`, which fixes what a board holds but its blocks'
// state, its board options and its plugins' state, and the load lock, the
// blockr option `lock_load`, which fixes the app to its opening board. Each
// block's `locked` says whether the board lock leaves the block open.
export interface Lock {
  board: boolean;
  load: boolean;
}

// Reading blockr options, which the client asks R for by name, since it can
// read neither an R option nor an environment variable of the server

export interface BlockrOptionsRequest {
  id: string;
  kind: "blockr_options";
  names: string[];
}

export interface BlockrOptionsReply {
  id: string;
  status: "ok";
  // The value of each blockr option that R has one for, by name, as
  // `blockr_option()` reads it: a string where an environment variable set it,
  // and null for Inf. R leaves out the others, for which the client takes its
  // fallback.
  values: Record<string, Json>;
}

// Evaluating a block

export interface EvaluateRequest {
  id: string;
  kind: "evaluate";
  block: BlockId;
  revision: number;
  // The block's `locked`, which R reads only under the board lock. It travels
  // outside the recipe, so that a change of it bumps no revision.
  locked: boolean | null;
  recipe: Recipe;
  // The parameters of a one-time evaluation, which R passes to `block_eval()`.
  params?: Record<string, Json>;
}

export interface Recipe {
  registry_id: RegistryId;
  state: Record<string, Json>;
  // In the order of the class's inputs, and a variadic block's in link order.
  inputs: RecipeInput[];
}

// A result that R holds, by its block and revision.
export interface BlockRevision {
  block: BlockId;
  revision: number;
}

// An input, by the input it feeds, with the upstream block's result that it
// binds, or alone where R binds it itself, for a block that `serve()` serves
// alone.
export type RecipeInput =
  | (BlockRevision & { input: string })
  | { input: string; block?: never; revision?: never };

export interface EvaluateReply {
  id: string;
  status: "ok" | "error" | "invalid";
  block: BlockId;
  revision: number;
  // The result's summary from `result_summary()`, or null where there is no
  // result or its summary failed.
  summary: string | null;
  conditions: Condition[];
  stdout: string;
  stderr: string;
}

export interface Condition {
  severity: "error" | "warning" | "message";
  // The class vector it has in R, as ["simpleWarning", "warning", "condition"].
  class: string[];
  // What raised it. A condition that R raises itself, such as for an
  // evaluation that a lock refuses, has none.
  phase?: Phase;
  message: string;
}

// The client's scheduler writes conditions of the phase `status`, and R
// writes the others.
export type Phase =
  | "expr"
  | "inputs"
  | "block_eval"
  | "result_summary"
  | "output"
  | "request"
  | "status";

// Requests. Each names one of core's types: block_class, output_class,
// renderer, option or plugin. R reads a request into an object of class
// `{type}_request` and answers it through its method of request_call(). No
// package adds a type, and R replies error to a request of any other type.

// A block class's request, which R resolves by registry id. Its `block` names
// the block that asks. It carries no `locked`, since R runs every request as on
// any board, whatever the locks.
export interface BlockClassRequest {
  id: string;
  kind: "request";
  type: "block_class";
  registry_id: RegistryId;
  block: BlockId;
  name: string;
  // The state args that its function names, filled in by the host from the
  // block's state where the module passes none, and the parameters the UI
  // gives it.
  args: Record<string, Json>;
  // The results it reads, by the input each feeds.
  reads: RecipeInput[];
  // The values of the board options that it reads, where it reads any.
  options?: Record<OptionId, Json>;
  body?: Body;
  option?: never;
  output?: never;
  renderer?: never;
}

// A board option's request, which R finds among the requests that
// board_option_requests() returns for the board option's class.
export interface BoardOptionRequest {
  id: string;
  kind: "request";
  type: "option";
  option: OptionId;
  name: string;
  args: Record<string, Json>;
  body?: Body;
  registry_id?: never;
  output?: never;
  renderer?: never;
}

// A plugin's request, which R finds among the requests that
// plugin_requests() returns for the plugin's class. It names the class by the
// class's id, as "blockr.md-md_plugin", since R holds no board. R calls the
// request's function with its args by name, and with the results it reads.
export interface PluginRequest {
  id: string;
  kind: "request";
  type: "plugin";
  plugin: PluginClassId;
  name: string;
  args: Record<string, Json>;
  // The results it reads, by block and revision, which the client has
  // evaluated first.
  reads: BlockRevision[];
  body?: Body;
  registry_id?: never;
  option?: never;
  output?: never;
  renderer?: never;
}

// A request of an output class, which R resolves against the class vector of
// the output it belongs to.
export interface OutputClassRequest {
  id: string;
  kind: "request";
  type: "output_class";
  output: BlockRevision;
  name: string;
  args: Record<string, Json>;
  // The board option values and parameters that its output was asked with.
  options: Record<OptionId, Json>;
  params: Record<string, Json>;
  body?: Body;
  registry_id?: never;
  option?: never;
  renderer?: never;
}

// A renderer's own request, which R resolves among that renderer's requests
// alone and runs as it runs a request of the output class, with the result of
// the output it belongs to.
export interface RendererRequest {
  id: string;
  kind: "request";
  type: "renderer";
  // The renderer's id.
  renderer: string;
  output: BlockRevision;
  name: string;
  args: Record<string, Json>;
  // The board option values and parameters that its output was asked with.
  options: Record<OptionId, Json>;
  params: Record<string, Json>;
  body?: Body;
  registry_id?: never;
  option?: never;
}

// An answer as R writes it, every vector as an array, or a file. R marks with
// `volatile` its reply to a request of an output class that it resolved to one
// that the class makes volatile, whose answer the client never keeps.
export type RequestReply =
  | { id: string; status: "ok"; value: Json; body?: never; volatile?: true }
  | { id: string; status: "ok"; body: Body; value?: never; volatile?: true };

// Outputs

export interface OutputRequest {
  id: string;
  kind: "output";
  block: BlockId;
  revision: number;
  options: Record<OptionId, Json>;
  info: OutputInfo;
  params: Record<string, Json>;
}

// What the client measured for the output's element: its size in CSS pixels,
// pixel ratio, colours and font.
export interface OutputInfo {
  width: number;
  height: number;
  pixel_ratio: number;
  bg: string;
  fg: string;
  accent: string;
  font: string;
}

// An output, whose properties beside its renderer are those of its class's
// contract, as srcts/types/outputs.ts gives core's.
export interface OutputReply {
  id: string;
  status: "ok";
  output: BlockOutput & Record<string, Json>;
  body?: Body;
}

// A binary body, which travels beside the message as the transport carries it,
// with its MIME type and, for a download, a file name.
export interface Body {
  type: string;
  name?: string;
}

// Replies that any request may get

export interface ErrorReply {
  id: string;
  status: "error";
  conditions: Condition[];
}

export interface CancelledReply {
  id: string;
  status: "cancelled";
}

export interface MissingReply {
  id: string;
  status: "missing";
  // The results that the request names and R does not hold.
  missing: BlockRevision[];
}

// Cancelling and forgetting, which get no reply of their own

export interface CancelRequest {
  kind: "cancel";
  // The id of the request to cancel.
  request: string;
  id?: never;
}

export interface ForgetRequest {
  kind: "forget";
  blocks: BlockId[];
  id?: never;
}

// A fixture: a message that the client sends, the messages that R sends back,
// and what it shows.
export interface Fixture<
  C extends ClientMessage = ClientMessage,
  R extends RMessage[] = RMessage[],
> {
  description: string;
  client: C;
  r: R;
}
