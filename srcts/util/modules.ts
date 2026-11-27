// A module of the contract whose code loads as it first renders: its render()
// imports the rest of the module's code with import() and renders that, as the
// contract allows, since render() may be async
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract).
// Core's build bundles the code that an import() names into a file of its own,
// so the page loads what the module draws with, such as blockr.ui's components
// and React Aria, only once core first renders the module
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#the-bundle).
// The UIs of core's board options and the advanced actions of core's board
// actions are made so.

import type { Rendered } from "../types/hosts.ts";

// A module whose render() renders the module that `load` resolves with.
// Where the signal aborts while the code loads, it renders nothing, so that a
// module that core has unmounted draws nothing into the element.
// Where the code fails to load, render() rejects, which core reports as it
// reports a render() that fails.
export function loadedModule<A extends { readonly signal: AbortSignal }>(
  load: () => Promise<{ render(args: A): Rendered }>,
): { render(args: A): Promise<(() => void) | undefined> } {
  return {
    async render(args) {
      const module = await load();
      if (args.signal.aborted) return undefined;
      return module.render(args);
    },
  };
}
