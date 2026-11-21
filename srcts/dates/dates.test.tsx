// The dates and times of core's React SDK, each bound with useValue() on the
// stub of a block's host, as a module draws them: what each shows of the
// state, and what it submits as the user types, picks and leaves it.

import { CalendarDate } from "@internationalized/date";
import { act, useState } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { PAUSE } from "../binding/index.ts";
import { useValue } from "../react/index.ts";
import { stubBlockHost } from "../stubs/index.ts";
import type { StubBlockHost } from "../stubs/index.ts";
import { press } from "../testing/inputs.ts";
import type { BlockHost } from "../types/hosts.ts";
import type { Json } from "../types/json.ts";
import { Calendar, DateField, DatePicker, DateRangePicker, RangeCalendar, TimeField } from "./index.ts";

type Dates = Readonly<{ on?: Json }>;

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
  document.body.replaceChildren();
});

// The UI of a block, which binds its one state arg, `on`, to an input.
type Input = (props: { readonly host: BlockHost<Dates> }) => ReactElement;

// A block whose one state arg, `on`, the input given binds, and which takes
// the values given, as its fixture lists them, beside the value it starts
// with.
function block(Form: Input, on: Json | undefined, takes: readonly (Json | undefined)[]) {
  const state = (value: Json | undefined): Dates => (value === undefined ? {} : { on: value });
  const host: StubBlockHost<Dates> = stubBlockHost<Dates>({ states: [on, ...takes].map(state), requests: [] }, { state: state(on) });
  const el = document.createElement("div");
  // Something outside the input, to which the focus goes as the user leaves
  // it.
  const away = document.createElement("button");
  document.body.append(el, away);
  const root = createRoot(el);
  act(() => {
    root.render(<Form host={host} />);
  });
  return {
    host,
    el,
    // The values that the input submitted, in the order it submitted them.
    submitted: () => host.submitted.map((submitted) => submitted.on),
    leave: () =>
      step(() => {
        away.focus();
      }),
    unmount: () => {
      act(() => {
        root.unmount();
      });
    },
  };
}

async function step(fn: () => void): Promise<void> {
  await act(async () => {
    fn();
    await Promise.resolve();
  });
}

// The text of each segment of the fields in `el`, as "11", or its placeholder
// where it holds no value, as "mm".
function segments(el: ParentNode): string[] {
  return [...el.querySelectorAll('[role="spinbutton"]')].map((segment) => segment.textContent);
}

function segment(el: ParentNode, type: string, nth = 0): HTMLElement {
  const found = el.querySelectorAll(`[role="spinbutton"][data-type="${type}"]`)[nth];
  if (!(found instanceof HTMLElement)) throw new Error(`Found no ${type} segment.`);
  return found;
}

// Types the text into the segment as a user does, one key at a time, as the
// browser hands React Aria each key before it changes the segment.
async function typeInto(target: HTMLElement, text: string): Promise<void> {
  await step(() => {
    target.focus();
  });
  for (const key of text) {
    await step(() => {
      target.dispatchEvent(new InputEvent("beforeinput", { data: key, inputType: "insertText", bubbles: true, cancelable: true }));
    });
  }
}

// The lines below the inputs in `el` that say what the inputs lack or why the
// host refused a submit.
function alerts(el: ParentNode): string[] {
  return [...el.querySelectorAll('[role="alert"]')].map((alert) => alert.textContent);
}

// The group of a field, which React Aria marks invalid.
function group(el: ParentNode): HTMLElement {
  const found = el.querySelector('[role="group"]');
  if (!(found instanceof HTMLElement)) throw new Error("Found no field.");
  return found;
}

// The day of the month given in the calendars on the page, or in the popover
// of a picker, which React Aria draws at the end of the page's body.
function day(n: number): HTMLElement {
  const found = [...document.querySelectorAll('[role="gridcell"] > [role="button"]')].find(
    (cell) => cell.textContent === String(n) && !cell.hasAttribute("data-outside-month"),
  );
  if (!(found instanceof HTMLElement)) throw new Error(`Found no day ${String(n)}.`);
  return found;
}

describe("a bound date field", () => {
  const field = (props: Partial<Parameters<typeof DateField>[0]> = {}): Input =>
    function Field({ host }) {
      return <DateField bound={useValue(host, "on")} label="On" {...props} />;
    };

  test("shows the date that the state holds", () => {
    const { el } = block(field(), "2026-11-19", []);
    expect(segments(el)).toEqual(["11", "19", "2026"]);
  });

  test("submits what the user types as the user presses Enter or leaves the field, and nothing as the user pauses", async () => {
    const { el, submitted, leave } = block(field(), "2026-11-19", ["2027-11-19", "2027-11-02"]);

    // Each key gives a date, as the year goes from 2 to 2027.
    await typeInto(segment(el, "year"), "202");
    await act(() => vi.advanceTimersByTimeAsync(PAUSE * 4));
    expect(submitted()).toEqual([]);
    await typeInto(segment(el, "year"), "7");
    expect(segments(el)).toEqual(["11", "19", "2027"]);
    await step(() => {
      press(segment(el, "year"), "Enter");
    });
    expect(submitted()).toEqual(["2027-11-19"]);

    await typeInto(segment(el, "day"), "02");
    expect(submitted()).toEqual(["2027-11-19"]);
    await leave();
    expect(submitted()).toEqual(["2027-11-19", "2027-11-02"]);
    expect(segments(el)).toEqual(["11", "2", "2027"]);
  });

  test("shows a date with a time where its granularity takes a time, and submits it with a space in place of ISO's T", async () => {
    const { el, submitted } = block(field({ granularity: "minute", hourCycle: 24 }), "2026-11-19 14:30:00", ["2026-11-19 15:30:00"]);
    expect(segments(el)).toEqual(["11", "19", "2026", "14", "30"]);

    await typeInto(segment(el, "hour"), "15");
    await step(() => {
      press(segment(el, "hour"), "Enter");
    });
    expect(submitted()).toEqual(["2026-11-19 15:30:00"]);
  });

  test("shows a value that does not read as a date as missing, and marks itself invalid", () => {
    const { el } = block(field({ granularity: "minute" }), "2026-11-19T14:30:00", []);
    // Each segment shows its placeholder, as before the user typed it.
    expect([...el.querySelectorAll('[role="spinbutton"]')].every((segment) => segment.hasAttribute("data-placeholder"))).toBe(true);
    expect(group(el).hasAttribute("data-invalid")).toBe(true);
    expect(alerts(el)).toEqual(["Not available: 2026-11-19T14:30:00"]);
  });

  test("shows the date that the host announces, as the assistant sets it", async () => {
    const { el, host } = block(field(), "2026-11-19", []);
    await step(() => {
      host.setState({ on: "2026-12-24" });
    });
    expect(segments(el)).toEqual(["12", "24", "2026"]);
    expect(group(el).hasAttribute("data-invalid")).toBe(false);
  });

  test("keeps the edit of the user as the host announces another date, and submits the edit as the user leaves", async () => {
    const { el, host, submitted, leave } = block(field(), "2026-11-19", ["2026-12-24", "2026-11-19"]);
    await typeInto(segment(el, "day"), "20");
    await step(() => {
      host.setState({ on: "2026-12-24" });
    });
    expect(segments(el)).toEqual(["11", "20", "2026"]);
    await leave();
    expect(submitted()).toEqual(["2026-11-20"]);
  });

  test("submits no date outside its range, and gives way to the state again as the user leaves", async () => {
    const { el, submitted, leave } = block(
      field({ minValue: new CalendarDate(2026, 1, 1), maxValue: new CalendarDate(2026, 12, 31) }),
      "2026-11-19",
      [],
    );
    await typeInto(segment(el, "year"), "2027");
    // React Aria marks the date as the user types it.
    expect(group(el).hasAttribute("data-invalid")).toBe(true);
    await leave();
    expect(submitted()).toEqual([]);
    expect(segments(el)).toEqual(["11", "19", "2026"]);
  });

  test("shows the state again where the host refuses the date, and says why below itself", async () => {
    const { el, submitted } = block(field(), "2026-11-19", []);
    await typeInto(segment(el, "day"), "20");
    await step(() => {
      press(segment(el, "day"), "Enter");
    });
    expect(submitted()).toEqual(["2026-11-20"]);
    expect(segments(el)).toEqual(["11", "19", "2026"]);
    expect(el.querySelector('[role="alert"]')?.textContent).toContain("which the fixture does not list");
  });

  test("unsets its state arg as the user clears the field and leaves it, but where it is required", async () => {
    for (const required of [false, true]) {
      const { el, submitted, leave } = block(field({ isRequired: required }), "2026-11-19", [undefined]);
      for (const type of ["year", "day", "month"]) {
        await step(() => {
          segment(el, type).focus();
        });
        // A segment clears one digit a time, and the year has four.
        for (let i = 0; i < 4; i++) {
          await step(() => {
            press(segment(el, type), "Backspace");
          });
        }
      }
      await leave();
      expect(submitted()).toEqual(required ? [] : [undefined]);
      document.body.replaceChildren();
    }
  });
});

describe("a bound time field", () => {
  test("shows the time that the state holds, and submits the time that the user steps to as the user leaves", async () => {
    function At({ host }: { readonly host: BlockHost<Dates> }): ReactElement {
      return <TimeField bound={useValue(host, "on")} label="At" hourCycle={24} />;
    }
    const { el, submitted, leave } = block(At, "14:30:00", ["15:30:00"]);
    expect(segments(el)).toEqual(["14", "30"]);

    await step(() => {
      segment(el, "hour").focus();
    });
    await step(() => {
      press(segment(el, "hour"), "ArrowUp");
    });
    expect(submitted()).toEqual([]);
    await leave();
    expect(submitted()).toEqual(["15:30:00"]);
  });
});

describe("a bound date picker", () => {
  function Picker({ host }: { readonly host: BlockHost<Dates> }): ReactElement {
    return <DatePicker bound={useValue(host, "on")} label="On" />;
  }

  test("submits a pick in its calendar at once, and what the user types as the user leaves", async () => {
    const { el, submitted, leave } = block(Picker, "2026-11-19", ["2026-11-20", "2026-11-25"]);
    expect(segments(el)).toEqual(["11", "19", "2026"]);

    const opener = el.querySelector("button");
    if (opener === null) throw new Error("Found no button that opens the calendar.");
    await step(() => {
      opener.click();
    });
    await step(() => {
      day(20).click();
    });
    expect(submitted()).toEqual(["2026-11-20"]);
    expect(segments(el)).toEqual(["11", "20", "2026"]);

    await typeInto(segment(el, "day"), "25");
    expect(submitted()).toEqual(["2026-11-20"]);
    await leave();
    expect(submitted()).toEqual(["2026-11-20", "2026-11-25"]);
  });
});

describe("a bound date range picker", () => {
  function During({ host }: { readonly host: BlockHost<Dates> }): ReactElement {
    return <DateRangePicker bound={useValue(host, "on")} label="During" />;
  }

  test("shows the range that the state holds, and submits the range that the user picks in its calendar at once", async () => {
    const { el, submitted } = block(During, { start: "2026-11-19", end: "2026-11-26" }, [{ start: "2026-11-10", end: "2026-11-12" }]);
    expect(segments(el)).toEqual(["11", "19", "2026", "11", "26", "2026"]);

    const opener = el.querySelector("button");
    if (opener === null) throw new Error("Found no button that opens the calendar.");
    await step(() => {
      opener.click();
    });
    await step(() => {
      day(10).click();
    });
    expect(submitted()).toEqual([]);
    await step(() => {
      day(12).click();
    });
    expect(submitted()).toEqual([{ start: "2026-11-10", end: "2026-11-12" }]);
  });

  test("shows a range that lacks an end as missing", () => {
    const { el } = block(During, { start: "2026-11-19" }, []);
    expect(alerts(el)).toEqual(['Not available: {"start":"2026-11-19"}']);
  });
});

describe("a bound calendar", () => {
  function On({ host }: { readonly host: BlockHost<Dates> }): ReactElement {
    return <Calendar bound={useValue(host, "on")} aria-label="On" />;
  }

  function During({ host }: { readonly host: BlockHost<Dates> }): ReactElement {
    return <RangeCalendar bound={useValue(host, "on")} aria-label="During" />;
  }

  test("submits the day that the user picks at once", async () => {
    const { submitted } = block(On, "2026-11-19", ["2026-11-21"]);
    expect(day(19).hasAttribute("data-selected")).toBe(true);
    await step(() => {
      day(21).click();
    });
    expect(submitted()).toEqual(["2026-11-21"]);
    expect(day(21).hasAttribute("data-selected")).toBe(true);
  });

  test("submits the range that the user picks at once, as a range calendar", async () => {
    const { submitted } = block(During, { start: "2026-11-19", end: "2026-11-26" }, [{ start: "2026-11-02", end: "2026-11-05" }]);
    await step(() => {
      day(2).click();
    });
    await step(() => {
      day(5).click();
    });
    expect(submitted()).toEqual([{ start: "2026-11-02", end: "2026-11-05" }]);
  });
});

test("a date picker without `bound` picks for its caller", async () => {
  const picked: string[] = [];
  function Picking(): ReactElement {
    const [value, setValue] = useState<CalendarDate | null>(new CalendarDate(2026, 11, 19));
    return (
      <DatePicker
        label="On"
        value={value}
        onChange={(next) => {
          setValue(next);
          picked.push(next?.toString() ?? "none");
        }}
      />
    );
  }
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  act(() => {
    root.render(<Picking />);
  });
  const opener = el.querySelector("button");
  if (opener === null) throw new Error("Found no button that opens the calendar.");
  await step(() => {
    opener.click();
  });
  await step(() => {
    day(3).click();
  });
  expect(picked).toEqual(["2026-11-03"]);
  expect(segments(el)).toEqual(["11", "3", "2026"]);
  act(() => {
    root.unmount();
  });
});
