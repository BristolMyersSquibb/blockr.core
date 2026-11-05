// The fixed block's UI, mounted on the stub of its host
// with the fixture that the block's R tests write.

import { afterEach, expect, test } from "vitest";
import { stubBlockHost } from "../../stubs/index.ts";
import { blockFixture, mountOnPage, unmount } from "../../testing/modules.ts";
import * as ui from "./index.ts";

afterEach(() => {
  document.body.replaceChildren();
});

test("draws nothing, asks nothing and submits nothing", async () => {
  const host = stubBlockHost(blockFixture("transform-fixed", "fixed_block"));
  const view = await mountOnPage(ui, host);

  expect(view.el.childNodes).toHaveLength(0);
  expect(host.requested).toEqual([]);
  expect(host.submitted).toEqual([]);
  unmount(view);
});
