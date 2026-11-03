// The wire protocol between the client and R: its messages, as
// inst/protocol/README.md gives them, and the client of the protocol. Core's
// store, scheduler and `CoreApi` send their requests through the client.

export { createClient, SessionEndedError } from "./client.ts";
export type {
  Client,
  ClientOptions,
  ProgressNote,
  RequestOptions,
  Transport,
  TransportListener,
} from "./client.ts";
export type {
  BlockRevision,
  Body,
  CancelledReply,
  Capability,
  ClientMessage,
  Condition,
  ErrorReply,
  EvaluateReply,
  EvaluateRequest,
  Fixture,
  HelloReply,
  MissingReply,
  Omitted,
  OpenReply,
  OutputRequest,
  Progress,
  Recipe,
  Reply,
  Request,
} from "./messages.ts";
