// The hooks through which a component that holds CoreApi follows it, drawn
// with React on notifications that the test sets, as CoreApi would hold them,
// and on the board of the fixture hello, which a store opened.

import { act } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { describe, expect, test } from "vitest";
import { holding } from "../testing/notifications.ts";
import { step } from "../testing/react.ts";
import { openedStore } from "../testing/store.ts";
import type { Board } from "../types/board.ts";
import type { BoardObject, CoreApi, Notification } from "../types/core-api.ts";
import { interned } from "../util/index.ts";
import { useBoard, useNotified } from "./index.ts";

// The class of core's Edit board plugin, which names the plugin.
const EDIT = interned("blockr.core", "edit_board_plugin");

// A notification that core's Edit board plugin raised, about the board object
// given, or about the board.
function raised(id: string, about?: BoardObject): Notification {
  return {
    id,
    severity: "warning",
    message: `warning ${id}`,
    source: { kind: "module", module: { object: { plugin: EDIT }, package: "blockr.core" }, code: "edited" },
    ...(about === undefined ? {} : { about }),
  };
}

describe("useNotified()", () => {
  test("gives the notifications about the board object given, by its kind and its id, or every notification given null, in the order they came, and the same array until one comes or goes", async () => {
    const given = holding([
      raised("a", { plugin: EDIT }),
      raised("b", { block: "b" }),
      raised("c"),
      raised("d", { stack: "b" }),
      raised("e", { plugin: EDIT }),
    ]);
    const core: { readonly notifications: Pick<CoreApi["notifications"], "get" | "subscribe"> } = given;
    const seen: (readonly Notification[])[] = [];
    function Ids({ about }: { about: BoardObject | null }): ReactElement {
      const notified = useNotified(core, about);
      seen.push(notified);
      return <p>{notified.map((notification) => notification.id).join(" ")}</p>;
    }
    const el = document.createElement("div");
    const root = createRoot(el);

    await step(() => {
      root.render(<Ids about={{ plugin: EDIT }} />);
    });
    expect(el.textContent).toBe("a e");

    // A new object that names the same plugin gives the same array, so that a
    // component may give one on each render.
    await step(() => {
      root.render(<Ids about={{ plugin: EDIT }} />);
    });
    expect(seen).toHaveLength(2);
    expect(seen[1]).toBe(seen[0]);

    // A block and a stack of the same id are two board objects.
    await step(() => {
      root.render(<Ids about={{ block: "b" }} />);
    });
    expect(el.textContent).toBe("b");
    await step(() => {
      root.render(<Ids about={null} />);
    });
    expect(el.textContent).toBe("a b c d e");

    await step(() => {
      root.render(<Ids about={{ block: "b" }} />);
    });
    const before = seen.at(-1);
    await step(() => {
      given.set([...given.notifications.get(), raised("f", { block: "b" })]);
    });
    expect(el.textContent).toBe("b f");
    expect(seen.at(-1)).not.toBe(before);

    act(() => {
      root.unmount();
    });
    expect(given.listening()).toBe(0);
  });
});

describe("useBoard()", () => {
  // The names of the board's blocks, in their order on the board.
  const names = (board: Board): string[] => Object.values(board.blocks).map((block) => block.name);

  test("gives what a function takes from the board, given one, and draws again only where that changes", async () => {
    const core = { store: await openedStore() };
    const drawn: (readonly string[])[] = [];
    function Names(): ReactElement {
      const taken = useBoard(core, (board) => names(board));
      drawn.push(taken);
      return <p>{taken.join(", ")}</p>;
    }
    const el = document.createElement("div");
    const root = createRoot(el);

    await step(() => {
      root.render(<Names />);
    });
    expect(el.textContent).toBe("Iris, Head, Merge, Wide sepals, Bind, Scatter");
    expect(drawn).toHaveLength(1);

    // A change of a block's state leaves the blocks' names as they were, so
    // the component draws nothing again.
    await step(() => {
      core.store.apply({ blocks: { mod: { b: { state: { n: 3, direction: "head" } } } } });
    });
    expect(drawn).toHaveLength(1);

    await step(() => {
      core.store.apply({ blocks: { mod: { b: { name: "First rows" } } } });
    });
    expect(el.textContent).toBe("Iris, First rows, Merge, Wide sepals, Bind, Scatter");
    expect(drawn).toHaveLength(2);

    act(() => {
      root.unmount();
    });
  });

  test("keeps what the function took the same object while what it takes from a newer board is equal", async () => {
    const core = { store: await openedStore() };
    const drawn: (readonly string[])[] = [];
    function Names({ label }: { label: string }): ReactElement {
      const taken = useBoard(core, names);
      drawn.push(taken);
      return (
        <p>
          {label}: {taken.join(", ")}
        </p>
      );
    }
    const el = document.createElement("div");
    const root = createRoot(el);

    await step(() => {
      root.render(<Names label="Blocks" />);
    });
    await step(() => {
      core.store.apply({ blocks: { mod: { b: { state: { n: 3, direction: "head" } } } } });
    });
    // Drawn again for its label, the component reads the names as the same
    // array.
    await step(() => {
      root.render(<Names label="The blocks" />);
    });
    expect(el.textContent).toBe("The blocks: Iris, Head, Merge, Wide sepals, Bind, Scatter");
    expect(drawn).toHaveLength(2);
    expect(drawn[1]).toBe(drawn[0]);

    act(() => {
      root.unmount();
    });
  });
});
