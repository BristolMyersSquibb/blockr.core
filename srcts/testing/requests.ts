// The requests of a stub host held until the test lets them through,
// so that the test sees what a module shows while a request is asked,
// as while R evaluates the block's inputs and then answers.

import type { BlockHost, OptionHost } from "../types/hosts.ts";

type Request = (...args: unknown[]) => Promise<unknown>;

// Holds each request that the stub host gets from now on,
// and returns what lets every request held so far go on to the stub,
// which answers or refuses it as it would have at once.
export function holdRequests(host: BlockHost | OptionHost): () => void {
  const target = host as { request: Request };
  const request = target.request.bind(host);
  const held: (() => void)[] = [];
  target.request = (...args) =>
    new Promise((resolve, reject) => {
      held.push(() => {
        request(...args).then(resolve, reject);
      });
    });
  return () => {
    for (const go of held.splice(0)) go();
  };
}
