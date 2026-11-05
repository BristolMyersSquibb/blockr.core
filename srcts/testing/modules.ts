// What the tests of core's block UI modules share:
// the fixture file of a block class, as the class's R tests write it,
// a module mounted on a stub of its host in an element on the page,
// and time passing, each through React's act(),
// as the tests run under fake timers, so that they decide when a binding's
// pause ends.
// What a user does to an input is in ./inputs.ts,
// which a test wraps in step() of ./react.ts.

import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { act } from "react";
import { vi } from "vitest";
import type { BlockFixture, Mounted } from "../stubs/index.ts";
import { mountReact } from "./react.ts";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");

// The fixture file that expect_block_fixture() writes for a block class,
// by the name of the test file that writes it and the name of the class,
// as tests/testthat/_snaps/transform-head/head_block.json.
export function blockFixture(test: string, name: string): BlockFixture {
  const file = join(root, "tests", "testthat", "_snaps", test, `${name}.json`);
  return JSON.parse(readFileSync(file, "utf8")) as BlockFixture;
}

// Mounts the module in an element on the page,
// so that its inputs can take the focus.
export async function mountOnPage(exports: unknown, host: unknown): Promise<Mounted> {
  const el = document.createElement("div");
  document.body.append(el);
  return mountReact(exports, host, el);
}

// Lets the time pass, and what waits on it run,
// such as a binding's pause or a module's handling of an answer.
export async function wait(ms: number): Promise<void> {
  await act(() => vi.advanceTimersByTimeAsync(ms));
}

export async function settle(): Promise<void> {
  await wait(0);
}

// Unmounts the module, as core does.
export function unmount(mounted: Mounted): void {
  act(() => {
    mounted.unmount();
  });
}
