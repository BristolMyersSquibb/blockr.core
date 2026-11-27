// The controls as the module that core offers other packages' modules,
// controls of blockr.core, which a module that builds nothing reaches through
// host.module(), and which the page's import map names `@blockr/core/controls`.

import { expect, test } from "vitest";
import { find, type } from "@blockr/ui/stubs";
import { stubBlockHost } from "../stubs/index.ts";
import { mountOnPage, unmount } from "../testing/modules.ts";
import type { BlockModule } from "../types/hosts.ts";
import * as offered from "./index.ts";
import { interned } from "../util/index.ts";

test("gives the controls to a module that builds nothing, through host.module()", async () => {
  const host = stubBlockHost(
    { states: [{ label: "x" }, { label: "y" }], requests: [] },
    { modules: { "blockr.core": { controls: offered } } },
  );
  const module: BlockModule = {
    async render({ el, host, signal }) {
      const ui = (await host.module<typeof offered>(interned("blockr.core", "controls"))).controls(host, signal);
      ui.text("label", { label: "Label" });
      el.replaceChildren(ui.el);
      return ui.cleanup;
    },
  };

  const view = await mountOnPage({ api: 1, default: module }, host);
  const input = find(view.el, "input", HTMLInputElement);
  expect(input.value).toBe("x");

  type(input, "y");
  unmount(view);
  expect(host.submitted).toEqual([{ label: "y" }]);
});
