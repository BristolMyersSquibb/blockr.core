// How the SDK's dates and times read the values that a state holds and write
// those that they submit, as R reads and writes them.

import { CalendarDate, CalendarDateTime, Time } from "@internationalized/date";
import { describe, expect, test } from "vitest";
import type { Json } from "../types/json.ts";
import { dateFits, rangeFits, readDate, readRange, readTime, shownValue, takesTime, timeFits, writeDate, writeRange, writeTime } from "./values.ts";

describe("a date", () => {
  test("reads as its ISO string, and writes so", () => {
    const date = readDate("2026-11-19", false);
    expect(date?.toString()).toBe("2026-11-19");
    expect(writeDate(new CalendarDate(2026, 11, 19), false)).toBe("2026-11-19");
  });

  test("reads as none from a date with a time, a date out of range, or what is no string", () => {
    for (const value of ["2026-11-19 14:30:00", "2026-02-30", "2026-13-01", "19.11.2026", "", 20261119, null]) {
      expect(readDate(value, false)).toBeUndefined();
    }
  });

  test("writes the date of a date with a time alone, where the input takes no time", () => {
    expect(writeDate(new CalendarDateTime(2026, 11, 19, 14, 30), false)).toBe("2026-11-19");
  });
});

describe("a date with a time", () => {
  test("has a space in place of ISO's T, which R's as.POSIXct() reads, and writes its seconds", () => {
    expect(readDate("2026-11-19 14:30:05", true)?.toString()).toBe("2026-11-19T14:30:05");
    expect(writeDate(new CalendarDateTime(2026, 11, 19, 14, 30), true)).toBe("2026-11-19 14:30:00");
  });

  test("reads without its seconds, and a date alone as midnight, as R reads them", () => {
    expect(readDate("2026-11-19 14:30", true)?.toString()).toBe("2026-11-19T14:30:00");
    expect(readDate("2026-11-19", true)?.toString()).toBe("2026-11-19T00:00:00");
    expect(writeDate(new CalendarDate(2026, 11, 19), true)).toBe("2026-11-19 00:00:00");
  });

  test("reads as none with ISO's T, which R's as.POSIXct() reads as midnight, or with a time out of range", () => {
    for (const value of ["2026-11-19T14:30:00", "2026-11-19 25:00", "2026-11-19 14:30:00.5", "2026-11-19 14:30:00Z", "2026-11-19  14:30"]) {
      expect(readDate(value, true)).toBeUndefined();
    }
  });
});

describe("a time", () => {
  test("reads as hours, minutes and seconds, without its seconds too, and writes its seconds", () => {
    expect(readTime("14:30:05")?.toString()).toBe("14:30:05");
    expect(readTime("14:30")?.toString()).toBe("14:30:00");
    expect(writeTime(new Time(14, 30))).toBe("14:30:00");
    expect(writeTime(new CalendarDateTime(2026, 11, 19, 14, 30))).toBe("14:30:00");
  });

  test("reads as none out of range, with a fraction of a second, or as what is no time", () => {
    for (const value of ["24:00", "14:60", "14:30:00.5", "14", "2026-11-19 14:30:00", 1430]) {
      expect(readTime(value)).toBeUndefined();
    }
  });
});

describe("a range", () => {
  test("is an object of two such strings, which R reads as a named list", () => {
    const range = readRange({ start: "2026-11-19", end: "2026-11-26" }, false);
    expect([range?.start.toString(), range?.end.toString()]).toEqual(["2026-11-19", "2026-11-26"]);
    expect(writeRange({ start: new CalendarDate(2026, 11, 19), end: new CalendarDate(2026, 11, 26) }, false)).toEqual({
      start: "2026-11-19",
      end: "2026-11-26",
    });
    expect(writeRange({ start: new CalendarDateTime(2026, 11, 19, 9), end: new CalendarDateTime(2026, 11, 19, 17) }, true)).toEqual({
      start: "2026-11-19 09:00:00",
      end: "2026-11-19 17:00:00",
    });
  });

  test("reads as none where an end is missing or does not read as a date, or where it is no object", () => {
    const values: readonly Json[] = [{ start: "2026-11-19" }, { start: "2026-11-19", end: "next week" }, ["2026-11-19", "2026-11-26"], "2026-11-19"];
    for (const value of values) {
      expect(readRange(value, false)).toBeUndefined();
    }
  });
});

test("an input takes a time where its granularity is finer than a day", () => {
  expect([undefined, "day", "hour", "minute", "second"].map((granularity) => takesTime(granularity as "day" | undefined))).toEqual([
    false,
    false,
    true,
    true,
    true,
  ]);
});

test("an input takes a date in its range that isDateUnavailable leaves, and a range whose start comes no later than its end", () => {
  const bounds = { minValue: new CalendarDate(2026, 11, 1), maxValue: new CalendarDate(2026, 11, 30) };
  expect(dateFits(new CalendarDate(2026, 11, 1), bounds)).toBe(true);
  expect(dateFits(new CalendarDate(2026, 10, 31), bounds)).toBe(false);
  expect(dateFits(new CalendarDate(2026, 12, 1), bounds)).toBe(false);
  expect(dateFits(new CalendarDate(2026, 11, 7), { isDateUnavailable: (date) => date.day === 7 })).toBe(false);
  const range = (start: number, end: number) => ({ start: new CalendarDate(2026, 11, start), end: new CalendarDate(2026, 11, end) });
  expect(rangeFits(range(2, 9), bounds)).toBe(true);
  expect(rangeFits(range(9, 2), bounds)).toBe(false);
  expect(rangeFits(range(2, 30), { maxValue: new CalendarDate(2026, 11, 29) })).toBe(false);
  expect(timeFits(new Time(9), { minValue: new Time(8), maxValue: new Time(17) })).toBe(true);
  expect(timeFits(new Time(18), { minValue: new Time(8), maxValue: new Time(17) })).toBe(false);
});

test("a value that reads as none shows as the string, or as its JSON", () => {
  expect(shownValue("2026-11-19T14:30:00")).toBe("2026-11-19T14:30:00");
  expect(shownValue({ start: "2026-11-19" })).toBe('{"start":"2026-11-19"}');
});
