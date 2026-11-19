// The hooks through which a component that holds CoreApi follows it, drawn
// with React on notifications that the test sets, as CoreApi would hold them.

import { act } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { describe, expect, test } from "vitest";
import { holding } from "../testing/notifications.ts";
import { step } from "../testing/react.ts";
import type { BoardObject, CoreApi, Notification } from "../types/core-api.ts";
import { useNotified } from "./index.ts";

// A notification that core's Edit board plugin raised, about the board object
// given, or about the board.
function raised(id: string, about?: BoardObject): Notification {
  return {
    id,
    severity: "warning",
    message: `warning ${id}`,
    source: { kind: "module", module: { object: { plugin: "blockr.core-edit_board_plugin" }, package: "blockr.core" }, code: "edited" },
    ...(about === undefined ? {} : { about }),
  };
}

describe("useNotified()", () => {
  test("gives the notifications about the board object given, by its kind and its id, or every notification given null, in the order they came, and the same array until one comes or goes", async () => {
    const given = holding([
      raised("a", { plugin: "blockr.core-edit_board_plugin" }),
      raised("b", { block: "b" }),
      raised("c"),
      raised("d", { stack: "b" }),
      raised("e", { plugin: "blockr.core-edit_board_plugin" }),
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
      root.render(<Ids about={{ plugin: "blockr.core-edit_board_plugin" }} />);
    });
    expect(el.textContent).toBe("a e");

    // A new object that names the same plugin gives the same array, so that a
    // component may give one on each render.
    await step(() => {
      root.render(<Ids about={{ plugin: "blockr.core-edit_board_plugin" }} />);
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
