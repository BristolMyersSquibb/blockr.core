// @vitest-environment node

// Patches as JSON Patch applies them, on a board's properties:
// the board is any record here, since a patch knows nothing of what it changes.

import { describe, expect, test } from "vitest";
import type { Patch } from "./delta.ts";
import { applyPatch, malformed, overlap, pathOf, pointer } from "./patch.ts";

const board = {
  views: { main: { title: "Main", panels: ["a", "b"] } },
  zoom: 1,
};

// The board after the patch, with the patch that takes it back, or the failure.
function apply(patch: Patch, on: Readonly<Record<string, unknown>> = board) {
  const parts = pointer(patch.path);
  if (parts === undefined) throw new Error(`No pointer: ${patch.path}`);
  return applyPatch(on, patch, parts);
}

// Applies the patch and then its inverse, which gives the board back.
function roundTrip(patch: Patch): Readonly<Record<string, unknown>> {
  const done = apply(patch);
  if ("failure" in done) throw new Error(done.failure);
  if (done.inverse === undefined) throw new Error("The patch changed nothing.");
  const back = apply(done.inverse, done.next);
  if ("failure" in back) throw new Error(back.failure);
  return back.next;
}

describe("pointers", () => {
  test("read and write the parts of a path, escaping / and ~", () => {
    expect(pointer("/views/main/panels/0")).toEqual(["views", "main", "panels", "0"]);
    expect(pointer("/extensions/blockr.dag-dag/a~1b~0c")).toEqual(["extensions", "blockr.dag-dag", "a/b~c"]);
    expect(pathOf(["extensions", "a/b~c"])).toBe("/extensions/a~1b~0c");
    expect(pointer("/views/")).toEqual(["views", ""]);
  });

  test("are none for a path that does not start at the board's top, or escapes nothing", () => {
    expect(pointer("")).toBeUndefined();
    expect(pointer("views/main")).toBeUndefined();
    expect(pointer("/views/a~2")).toBeUndefined();
  });

  test("overlap where one path lies at or under the other", () => {
    expect(overlap(["views"], ["views", "main"])).toBe(true);
    expect(overlap(["views", "main"], ["views", "main"])).toBe(true);
    expect(overlap(["views", "main"], ["views", "side"])).toBe(false);
  });
});

describe("applying a patch", () => {
  test("adds a member, and takes it back by removing it", () => {
    const done = apply({ op: "add", path: "/views/side", value: { title: "Side", panels: [] } });
    expect(done).toEqual({
      next: { ...board, views: { ...board.views, side: { title: "Side", panels: [] } } },
      inverse: { op: "remove", path: "/views/side" },
    });
    expect(roundTrip({ op: "add", path: "/views/side", value: {} })).toEqual(board);
  });

  test("adds a member that is there by replacing it, which a replace takes back", () => {
    expect(apply({ op: "add", path: "/zoom", value: 2 })).toEqual({
      next: { ...board, zoom: 2 },
      inverse: { op: "replace", path: "/zoom", value: 1 },
    });
  });

  test("inserts into an array at an index or at its end, and takes it back by removing the element", () => {
    const at = apply({ op: "add", path: "/views/main/panels/1", value: "c" });
    expect(at).toMatchObject({ inverse: { op: "remove", path: "/views/main/panels/1" } });
    expect("next" in at && at.next.views).toEqual({ main: { title: "Main", panels: ["a", "c", "b"] } });

    const end = apply({ op: "add", path: "/views/main/panels/-", value: "c" });
    expect(end).toMatchObject({ inverse: { op: "remove", path: "/views/main/panels/2" } });
    expect(roundTrip({ op: "add", path: "/views/main/panels/2", value: "c" })).toEqual(board);
  });

  test("removes a member or an element, and takes it back by adding it where it was", () => {
    expect(apply({ op: "remove", path: "/views/main/panels/0" })).toMatchObject({
      inverse: { op: "add", path: "/views/main/panels/0", value: "a" },
    });
    expect(roundTrip({ op: "remove", path: "/views/main/panels/0" })).toEqual(board);
    expect(roundTrip({ op: "remove", path: "/views/main/title" })).toEqual(board);
  });

  test("names the member that followed one it removes, and puts a member it adds before the one named", () => {
    const views = { views: { main: 1, side: 2, data: 3 } };
    const removed = apply({ op: "remove", path: "/views/side" }, views);
    expect(removed).toMatchObject({ place: "data" });
    expect(apply({ op: "remove", path: "/views/data" }, views)).toMatchObject({ place: null });
    if ("failure" in removed) throw new Error(removed.failure);

    // An add that takes the removal back, with the place it left.
    const order = (place: string | null) => {
      const back = applyPatch(removed.next, { op: "add", path: "/views/side", value: 2 }, ["views", "side"], place);
      if ("failure" in back) throw new Error(back.failure);
      return Object.keys(back.next.views as object);
    };
    expect(order("data")).toEqual(["main", "side", "data"]);
    expect(order("gone")).toEqual(["main", "data", "side"]);
    expect(order(null)).toEqual(["main", "data", "side"]);

    // An add of a member the object holds replaces it where it is.
    const replaced = applyPatch(views, { op: "add", path: "/views/main", value: 4 }, ["views", "main"], "data");
    expect("next" in replaced && Object.keys(replaced.next.views as object)).toEqual(["main", "side", "data"]);
  });

  test("replaces a value, and takes it back by replacing it again", () => {
    expect(apply({ op: "replace", path: "/views/main/panels/1", value: "c" })).toMatchObject({
      inverse: { op: "replace", path: "/views/main/panels/1", value: "b" },
    });
    expect(roundTrip({ op: "replace", path: "/views/main", value: { title: "Other", panels: [] } })).toEqual(board);
  });

  test("copies only what lies along its path, and changes nothing where it sets what is there", () => {
    const done = apply({ op: "replace", path: "/views/main/title", value: "Top" });
    if ("failure" in done) throw new Error(done.failure);
    expect(done.next.zoom).toBe(board.zoom);
    expect(done.next).not.toBe(board);

    expect(apply({ op: "replace", path: "/views/main/panels", value: ["a", "b"] })).toEqual({
      next: board,
      inverse: undefined,
    });
    expect(apply({ op: "add", path: "/views/main/title", value: "Main" })).toEqual({
      next: board,
      inverse: undefined,
    });
  });

  test("fails where its path finds nothing to change", () => {
    const failures: [Patch, string][] = [
      [{ op: "replace", path: "/views/side/title", value: "x" }, 'finds no "side" in "/views"'],
      [{ op: "replace", path: "/views/side", value: {} }, 'finds no "side" in "/views" to replace'],
      [{ op: "remove", path: "/views/side" }, 'finds no "side" in "/views" to remove'],
      [{ op: "remove", path: "/views/main/panels/2" }, 'finds no element "2" in the array at "/views/main/panels"'],
      [{ op: "add", path: "/views/main/panels/3", value: "c" }, 'finds no element "3" in the array at "/views/main/panels"'],
      [{ op: "add", path: "/views/main/panels/01", value: "c" }, 'finds no element "01" in the array at "/views/main/panels"'],
      [{ op: "remove", path: "/views/main/panels/-" }, 'finds no element "-" in the array at "/views/main/panels"'],
      [{ op: "add", path: "/zoom/level", value: 2 }, 'finds neither an object nor an array at "/zoom"'],
    ];
    for (const [patch, failure] of failures) {
      expect(apply(patch)).toEqual({ failure });
    }
  });
});

describe("the shape of a patch", () => {
  test("is checked, as a patch from plain JS or a model may get it wrong", () => {
    expect(malformed({ op: "add", path: "/views/main", value: {} })).toEqual([]);
    expect(malformed({ op: "remove", path: "/views/main", edit: 3 })).toEqual([]);
    expect(malformed("remove /views")).toEqual(["is no object"]);
    expect(malformed({ op: "move", path: "/views", from: "/zoom", edit: "3" })).toEqual([
      'has the op "move", where it takes "add", "remove" or "replace"',
      'has the edit id "3", which is no whole number',
      'holds "from", which a patch does not keep',
    ]);
    expect(malformed({ op: "replace", path: "views" })).toEqual([
      'has the path "views", which is no JSON Pointer below the board\'s top',
      "has no value",
    ]);
  });
});
