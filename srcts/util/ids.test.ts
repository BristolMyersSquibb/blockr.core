// @vitest-environment node

// The ids of what packages define, one object for each pair of a package and
// an id, and the records that hold values by package and then by id.

import { expect, test } from "vitest";
import type { Json } from "../types/json.ts";
import { idNumber, interned, internedRecord, nestedEntries, nestedRecord, nestedValue } from "./ids.ts";

test("interned() gives the same frozen object for the same pair, which compares as a string does and serializes as its record", () => {
  const head = interned("blockr.core", "head_block");

  expect(interned("blockr.core", "head_block")).toBe(head);
  expect(head).toStrictEqual({ package: "blockr.core", id: "head_block" });
  expect(Object.isFrozen(head)).toBe(true);
  expect(interned("blockr.core", "dataset_block")).not.toBe(head);
  expect(interned("blockr.dag", "head_block")).not.toBe(head);
  // A package of null, for what an app script defines, makes pairs of its own.
  expect(interned(null, "head_block")).toBe(interned(null, "head_block"));
  expect(interned(null, "head_block")).not.toBe(head);

  // A Set, a Map and includes() find the id by another call for the pair.
  expect(new Set([head, interned("blockr.core", "head_block")]).size).toBe(1);
  expect(new Map([[head, 1]]).get(interned("blockr.core", "head_block"))).toBe(1);
  expect([interned("blockr.core", "head_block")].includes(head)).toBe(true);

  // The record that JSON gives back is another object, whose id is the same.
  const text = JSON.stringify(head);
  expect(text).toBe('{"package":"blockr.core","id":"head_block"}');
  const record = JSON.parse(text) as { package: string; id: string };
  expect(record).not.toBe(head);
  expect(internedRecord(record)).toBe(head);
});

test("interned() refuses a package that is not named as R names one, and an id that is empty or no string", () => {
  for (const pkg of ["", "x", "1blockr", "blockr.", "blockr-core", "blockr_core"]) {
    expect(() => interned(pkg, "head_block")).toThrow(TypeError);
    expect(() => interned(pkg, "head_block")).toThrow(`The package ${JSON.stringify(pkg)} is not named as R names a package.`);
  }
  expect(() => interned("blockr.core", "")).toThrow(TypeError);
  expect(() => interned("blockr.core", "")).toThrow('The id "" of blockr.core is no non-empty string.');
  expect(() => interned(null, "")).toThrow('The id "" of an app script is no non-empty string.');
  // As a caller in plain JS may give one.
  expect(() => interned("blockr.core", 7 as unknown as string)).toThrow("The id 7 of blockr.core is no non-empty string.");
});

test("nestedEntries(), nestedValue() and nestedRecord() read and make a record by package and then by id, in its order", () => {
  const record: Readonly<Record<string, Readonly<Record<string, Json>>>> = {
    "blockr.dock": { dock_plugin: { views: {} }, tabs_plugin: 2 },
    "blockr.dag": { dag_plugin: { positions: {} } },
  };
  const dock = interned("blockr.dock", "dock_plugin");

  // Each entry holds the id as core's table holds it, and the record's value.
  const entries = nestedEntries(record);
  expect(entries).toStrictEqual([
    [dock, { views: {} }],
    [interned("blockr.dock", "tabs_plugin"), 2],
    [interned("blockr.dag", "dag_plugin"), { positions: {} }],
  ]);
  expect(entries[0]?.[0]).toBe(dock);
  expect(entries[0]?.[1]).toBe(record["blockr.dock"]?.dock_plugin);
  for (const [id, value] of entries) expect(nestedValue(record, id)).toBe(value);

  // The entries make the record again, its packages and their ids in order.
  const again = nestedRecord(entries);
  expect(again).toStrictEqual(record);
  expect(Object.keys(again)).toEqual(["blockr.dock", "blockr.dag"]);
  expect(Object.keys(again["blockr.dock"] ?? {})).toEqual(["dock_plugin", "tabs_plugin"]);
  expect(nestedRecord(nestedEntries({}))).toStrictEqual({});
  // A package goes where its first entry is.
  const grouped = nestedRecord([
    [dock, 1],
    [interned("blockr.dag", "dag_plugin"), 2],
    [interned("blockr.dock", "tabs_plugin"), 3],
  ]);
  expect(grouped).toStrictEqual({ "blockr.dock": { dock_plugin: 1, tabs_plugin: 3 }, "blockr.dag": { dag_plugin: 2 } });
  expect(Object.keys(grouped)).toEqual(["blockr.dock", "blockr.dag"]);

  // The record holds nothing under an id of a package that it holds, under a
  // package that it lacks, or under what every object inherits.
  expect(nestedValue(record, interned("blockr.dock", "grid_plugin"))).toBeUndefined();
  expect(nestedValue(record, interned("blockr.md", "md_plugin"))).toBeUndefined();
  expect(nestedValue(record, interned("blockr.dock", "toString"))).toBeUndefined();
  expect(nestedValue({}, interned("constructor", "name"))).toBeUndefined();
});

test("idNumber() gives each id one number, however often it is asked, and another id another", () => {
  const head = interned("blockr.core", "head_block");
  const first = idNumber(head);

  expect(Number.isInteger(first)).toBe(true);
  expect(idNumber(head)).toBe(first);
  expect(idNumber(interned("blockr.core", "head_block"))).toBe(first);
  const others = [interned("blockr.dag", "dag_plugin"), interned(null, "head_block"), interned("blockr.core", "dataset_block")];
  const numbers = others.map(idNumber);
  expect(new Set([first, ...numbers]).size).toBe(4);
  expect(others.map(idNumber)).toEqual(numbers);
});
