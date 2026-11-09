// The wire protocol between the client and R: its messages, as
// inst/protocol/README.md gives them, and the client of the protocol. Core's
// store, scheduler and `CoreApi` send their requests through the client.

export { createClient, sameMajor, SessionEndedError } from "./client.ts";
export type {
  Client,
  ClientOptions,
  ProgressNote,
  RequestOptions,
  Transport,
  TransportListener,
} from "./client.ts";
// Every message, since the README gives each one as part of the contract
// between the client and R.
export * from "./messages.ts";
