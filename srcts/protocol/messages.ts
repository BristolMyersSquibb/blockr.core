// The messages of the wire protocol between the client and R, as
// inst/protocol/README.md gives them, which the fixtures in
// inst/protocol/fixtures are checked against.

import type {
  BlockId,
  OpeningBoard,
  OptionId,
  RegistryId,
  SavedBoard,
} from "../types/board.ts";
import type { Json } from "../types/json.ts";
import type { RegistryEntry } from "../types/registry.ts";

// The version of the protocol that the client speaks. R's protocol_version()
// gives the same, as the fixtures check in both test suites.
export const PROTOCOL_VERSION = "1.0";

export type Kind =
  | "hello"
  | "open"
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
  | OpenRequest
  | EvaluateRequest
  | BlockClassRequest
  | BoardOptionRequest
  | ExtensionRequest
  | OutputClassRequest
  | OutputRequest
  | CancelRequest
  | ForgetRequest;

export type Reply =
  | HelloReply
  | OpenReply
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
  // The packages that a board the client holds already names, as when the
  // server ended its session while the page lived on, for R to load.
  packages?: string[];
}

export type HelloReply = {
  id: string;
  status: "ok";
  protocol: string;
  capabilities: Capability[];
  // The version of each package that R has loaded and the client needs to know
  // of.
  packages: Record<string, string>;
  block_registry: Record<RegistryId, RegistryEntry>;
  // Where each front-end's module is served, by the front-end's id.
  front_ends: Record<string, { module: string; css?: string }>;
  // In the order of the packages' R dependencies, each package's after those
  // of the packages it depends on.
  startup_modules: { package: string; module: string; css?: string }[];
  // The modules that packages offer each other, by id.
  modules: Record<string, { module: string; css?: string }>;
  // The renderers that R picked, by the renderer's id.
  renderers: Record<string, Renderer>;
  // The values of the blockr options that the client reads, by package.
  blockr_options: Record<string, Record<string, Json>>;
  lock: Lock | null;
  // For a block that `serve()` serves alone, the inputs that R binds itself
  // to the data it was given, which the client counts as linked.
  bound?: string[];
} & (
  // The opening board, as R sends the one an app script built, or as the saved
  // file that `serve()` was given holds it, which R passes on as it is.
  { board: OpeningBoard; file?: never } | { file: SavedBoard; board?: never }
);

// A renderer that R picked for an output class or for a board option.
export type Renderer = { module: string; css?: string } & (
  | { class: string; option?: never }
  | { option: OptionId; class?: never }
);

// The blocks that a locked board leaves open.
export interface Lock {
  open: BlockId[];
}

// Opening a board, which the client does itself, having R load the packages
// that the board names

export interface OpenRequest {
  id: string;
  kind: "open";
  packages: string[];
}

export type OpenReply = Pick<
  HelloReply,
  | "id"
  | "status"
  | "packages"
  | "block_registry"
  | "front_ends"
  | "startup_modules"
  | "modules"
  | "renderers"
  | "blockr_options"
>;

// Evaluating a block

export interface EvaluateRequest {
  id: string;
  kind: "evaluate";
  block: BlockId;
  revision: number;
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
  omitted: Omitted;
  stdout: string;
  stderr: string;
}

export interface Condition {
  severity: "error" | "warning" | "message";
  // The class vector it has in R, as ["simpleWarning", "warning", "condition"].
  class: string[];
  // What raised it. A condition that R raises itself, such as for a request
  // that the lock refuses, has none.
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

// The conditions that a reply counted rather than kept, by severity.
export interface Omitted {
  warning: number;
  message: number;
}

// Requests, by the object that each belongs to

// A block class's request, which R resolves by registry id. Its `block` names
// the block that asks, which R reads only on a locked board.
export interface BlockClassRequest {
  id: string;
  kind: "request";
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
  extension?: never;
  output?: never;
}

export interface BoardOptionRequest {
  id: string;
  kind: "request";
  option: OptionId;
  name: string;
  args: Record<string, Json>;
  body?: Body;
  registry_id?: never;
  extension?: never;
  output?: never;
}

export interface ExtensionRequest {
  id: string;
  kind: "request";
  // The extension's id, `{pkg}-{name}`.
  extension: string;
  name: string;
  args: Record<string, Json>;
  reads: BlockRevision[];
  body?: Body;
  registry_id?: never;
  option?: never;
  output?: never;
}

// A request of an output class, which R resolves against the class vector of
// the output it belongs to.
export interface OutputClassRequest {
  id: string;
  kind: "request";
  output: BlockRevision;
  name: string;
  args: Record<string, Json>;
  // The board option values and parameters that its output was asked with.
  options: Record<OptionId, Json>;
  params: Record<string, Json>;
  body?: Body;
  registry_id?: never;
  option?: never;
  extension?: never;
}

// An answer as R writes it, every vector as an array, or a file.
export type RequestReply =
  | { id: string; status: "ok"; value: Json; body?: never }
  | { id: string; status: "ok"; body: Body; value?: never };

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

export interface OutputReply {
  id: string;
  status: "ok";
  output: BlockOutput & Record<string, Json>;
  body?: Body;
}

// What every output holds, whatever its class, whose contract gives the rest.
export interface BlockOutput {
  // The renderer that R picked for the output's class.
  renderer: string;
}

// A binary body, which travels beside the message as the bridge carries it,
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
  omitted: Omitted;
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
