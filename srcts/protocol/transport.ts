// What core sends R its requests through: the Shiny bridge on the page, and
// replies from the fixtures in tests.

import type {
  CancelRequest,
  ForgetRequest,
  Reply,
  Request,
} from "./messages.ts";

export interface Transport {
  // Sends R a request that it replies to, giving it an id that no other
  // request waiting on R has, and resolves with R's reply to it.
  send(
    request: WithoutId<Exclude<Request, CancelRequest | ForgetRequest>>,
  ): Promise<Reply>;
}

type WithoutId<R> = R extends unknown ? Omit<R, "id"> : never;
