// How a control runs its edits through useEdits(): where the store or the
// command registry refuses one, the control says why itself, under the
// sentence that names the edit, and core raises nothing, whether the edit
// runs through the command registry or through the done() of an advanced
// action.
// The fixture's board holds the blocks a, Iris, and b, Head, among others.

import { act } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { afterEach, describe, expect, test } from "vitest";
import { stubCore } from "../stubs/index.ts";
import { find } from "../testing/inputs.ts";
import { held } from "../testing/notifications.ts";
import { step } from "../testing/react.ts";
import type { CoreApi } from "../types/core-api.ts";
import { Refusal, useEdits } from "./index.ts";
import type { Done } from "./index.ts";

const stops: (() => void)[] = [];

afterEach(() => {
  for (const stop of stops.splice(0)) stop();
});

// A control with a button for each edit, which removes blocks, and the banner
// that says why the last edit was refused. The edits of the buttons named
// "through done" run through the `done` given.
function Control({ core, done }: { core: Pick<CoreApi, "commands">; done?: Done }): ReactElement {
  const edits = useEdits(core);
  const runs: Record<string, () => Promise<boolean>> = {
    "Remove Head": () => edits.run('Could not remove the block "Head".', "blockr.core-remove_block", { id: "b" }),
    "Remove nowhere": () => edits.run('Could not remove the block "nowhere".', "blockr.core-remove_block", { id: "nowhere" }),
    "Remove Iris and nowhere": () =>
      edits.run('Could not remove the blocks "Iris" and "nowhere".', [
        { id: "blockr.core-remove_block", args: { id: "a" } },
        { id: "blockr.core-remove_block", args: { id: "nowhere" } },
      ]),
    "Remove Head through done": () => edits.run('Could not remove the block "Head".', [{ id: "blockr.core-remove_block", args: { id: "b" } }], done),
    "Remove nowhere through done": () =>
      edits.run('Could not remove the block "nowhere".', [{ id: "blockr.core-remove_block", args: { id: "nowhere" } }], done),
  };
  return (
    <div>
      {Object.entries(runs).map(([label, run]) => (
        <button
          key={label}
          type="button"
          onClick={() => {
            void run();
          }}
        >
          {label}
        </button>
      ))}
      <Refusal refusal={edits.refusal} />
    </div>
  );
}

// The control on the board of the fixture hello, drawn in an element on the
// page. Its edits run through `run` where one is given, in place of the
// command registry's, and those through done through `done`.
async function drawn({ run, done }: { run?: CoreApi["commands"]["run"]; done?: Done } = {}): Promise<{
  el: HTMLElement;
  core: CoreApi;
}> {
  const stubbed = await act(() => stubCore());
  const core = run === undefined ? stubbed.core : { ...stubbed.core, commands: { ...stubbed.core.commands, run } };
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  await step(() => {
    root.render(<Control core={core} {...(done === undefined ? {} : { done })} />);
  });
  stops.push(stubbed.stop);
  return { el, core: stubbed.core };
}

async function click(el: HTMLElement, label: string): Promise<void> {
  const button = [...el.querySelectorAll("button")].find((b) => b.textContent === label);
  if (button === undefined) throw new Error(`Found no button ${label}.`);
  await act(async () => {
    button.click();
    await Promise.resolve();
  });
}

// What each banner in `el` says, in order: the sentence that names the edit,
// then each reason.
function refusals(el: HTMLElement): string[][] {
  return [...el.querySelectorAll('[role="alert"]')].map((alert) => [...alert.querySelectorAll("p, li")].map((line) => line.textContent));
}

describe("an edit that the command registry or the store refuses", () => {
  test("is said by the control that made it, under the sentence that names the edit, until its next edit, and core raises no notification of it", async () => {
    const { el, core } = await drawn();

    await click(el, "Remove nowhere");
    expect(refusals(el)).toEqual([['Could not remove the block "nowhere".', 'It fails its preflight on the board as it is, with the args {"id":"nowhere"}.']]);
    // The banner marks an error as a card's banners do, and says the one
    // reason in a sentence, rather than in a list.
    const alert = find(el, '[role="alert"]');
    expect(alert.className).toContain("bc:border-border-danger");
    expect(alert.className).toContain("bc:bg-red-500/10");
    expect(find(alert, "p").className).toContain("bc:text-text-danger");
    expect(alert.querySelector("ul")).toBeNull();
    expect(held(core)).toEqual([]);

    // A list that refuses applies none of its commands, and gives the reasons
    // of the command that refused.
    await click(el, "Remove Iris and nowhere");
    expect(refusals(el)).toEqual([['Could not remove the blocks "Iris" and "nowhere".', 'It fails its preflight on the board as it is, with the args {"id":"nowhere"}.']]);
    expect(Object.keys(core.store.board().blocks)).toContain("a");
    expect(held(core)).toEqual([]);

    // The next edit that runs takes the reason down.
    await click(el, "Remove Head");
    expect(refusals(el)).toEqual([]);
    expect(Object.keys(core.store.board().blocks)).not.toContain("b");
  });

  test("that runs through the done() of an advanced action is said by the control as well, which hands done() a notifier that takes the refusal's notifications", async () => {
    // A done() that runs the list as one step of the user with the notifier
    // that the control hands it, as core's does, and records each list.
    const lists: (readonly unknown[])[] = [];
    const ran: { commands?: CoreApi["commands"] } = {};
    const done: Done = async (list, opts) => {
      lists.push(list);
      await ran.commands?.run(list, "user", opts);
    };
    const { el, core } = await drawn({ done });
    ran.commands = core.commands;

    await click(el, "Remove nowhere through done");
    expect(refusals(el)).toEqual([['Could not remove the block "nowhere".', 'It fails its preflight on the board as it is, with the args {"id":"nowhere"}.']]);
    expect(held(core)).toEqual([]);

    await click(el, "Remove Head through done");
    expect(refusals(el)).toEqual([]);
    expect(Object.keys(core.store.board().blocks)).not.toContain("b");
    expect(lists).toEqual([
      [{ id: "blockr.core-remove_block", args: { id: "nowhere" } }],
      [{ id: "blockr.core-remove_block", args: { id: "b" } }],
    ]);
  });
});

test("an edit that fails with an error that lists no reasons says what the error says", async () => {
  const { el } = await drawn({ run: () => Promise.reject(new Error("The session ended.")) });

  await click(el, "Remove Head");
  expect(refusals(el)).toEqual([['Could not remove the block "Head".', "The session ended."]]);
});
