// How a control runs its edits through useEdits(): where the store or the
// command registry refuses one, the control says why itself and core raises
// nothing, or, for a control that leaves the refusal to core, core raises it.
// The fixture's board holds the blocks a, Iris, and b, Head, among others.

import { act } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { afterEach, describe, expect, test } from "vitest";
import { stubCore } from "../stubs/index.ts";
import { find } from "../testing/inputs.ts";
import { held, notificationCode } from "../testing/notifications.ts";
import { step } from "../testing/react.ts";
import type { CoreApi } from "../types/core-api.ts";
import { Refusal, useEdits } from "./index.ts";

const stops: (() => void)[] = [];

afterEach(() => {
  for (const stop of stops.splice(0)) stop();
  document.body.replaceChildren();
});

// A control with a button for each edit, which removes blocks, and the line
// that says why the last edit was refused, where the control shows refusals.
// Two of the edits give the sentence that names the edit, as a table does for
// the buttons of its rows.
function Control({ core, shows }: { core: CoreApi; shows?: boolean }): ReactElement {
  const edits = useEdits(core, shows);
  const runs: Record<string, () => Promise<boolean>> = {
    "Remove Head": () => edits.run("blockr.core-remove_block", { id: "b" }),
    "Remove nowhere": () => edits.run("blockr.core-remove_block", { id: "nowhere" }),
    "Remove nowhere, naming it": () => edits.run("blockr.core-remove_block", { id: "nowhere" }, 'Could not remove the block "nowhere".'),
    "Remove Iris and nowhere": () =>
      edits.run(
        [
          { id: "blockr.core-remove_block", args: { id: "a" } },
          { id: "blockr.core-remove_block", args: { id: "nowhere" } },
        ],
        'Could not remove the blocks "Iris" and "nowhere".',
      ),
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
// page.
async function drawn(shows?: boolean): Promise<{ el: HTMLElement; core: CoreApi }> {
  const stubbed = await act(() => stubCore());
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  await step(() => {
    root.render(<Control core={stubbed.core} {...(shows === undefined ? {} : { shows })} />);
  });
  stops.push(() => {
    act(() => {
      root.unmount();
    });
  }, stubbed.stop);
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

function refusals(el: HTMLElement): string[] {
  return [...el.querySelectorAll('[role="alert"]')].map((alert) => alert.textContent);
}

describe("an edit that the command registry or the store refuses", () => {
  test("is said by the control that made it, until its next edit, and core raises no notification of it", async () => {
    const { el, core } = await drawn();

    await click(el, "Remove nowhere");
    expect(refusals(el)).toEqual([expect.stringMatching(/^The command "blockr\.core-remove_block" refused to run/)]);
    expect(find(el, '[role="alert"]').className).toContain("bc:text-text-danger");
    expect(held(core)).toEqual([]);

    // The sentence that names the edit comes first.
    await click(el, "Remove nowhere, naming it");
    expect(refusals(el)).toEqual([expect.stringMatching(/^Could not remove the block "nowhere"\.\nThe command "blockr\.core-remove_block" refused to run/)]);

    // A list that refuses applies none of its commands.
    await click(el, "Remove Iris and nowhere");
    expect(refusals(el)).toEqual([
      expect.stringMatching(/^Could not remove the blocks "Iris" and "nowhere"\.\nThe list of commands was refused at command 2, "blockr\.core-remove_block"/),
    ]);
    expect(Object.keys(core.store.board().blocks)).toContain("a");
    expect(held(core)).toEqual([]);

    // The next edit that runs takes the reason down.
    await click(el, "Remove Head");
    expect(refusals(el)).toEqual([]);
    expect(Object.keys(core.store.board().blocks)).not.toContain("b");
  });

  test("is left to core, which raises it, by a control that shows no refusal", async () => {
    const { el, core } = await drawn(false);

    await click(el, "Remove nowhere, naming it");
    await click(el, "Remove Iris and nowhere");
    expect(refusals(el)).toEqual([]);
    // Core's notification holds the reason alone.
    expect(held(core).map((n) => [n.severity, notificationCode(n), n.message.split("\n")[0]])).toEqual([
      ["error", "board_edit_failed", 'The command "blockr.core-remove_block" refused to run'],
      ["error", "board_edit_failed", 'The list of commands was refused at command 2, "blockr.core-remove_block"'],
    ]);
  });
});
