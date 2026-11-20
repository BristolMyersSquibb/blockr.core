// The slider of core's React SDK, React Aria's Slider in core's theme: what it
// shows, and the binding of a block's state arg, on the stub of a block's
// host.
// The user moves a thumb by the keys here, which React Aria takes as a drag
// that starts and ends at once, since happy-dom measures no track to drag
// along.
// The rest, such as dragging and the focus, is React Aria's.

import { act } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { afterEach, describe, expect, test } from "vitest";
import { useValue } from "../react/index.ts";
import { stubBlockHost } from "../stubs/index.ts";
import type { StubBlockHost } from "../stubs/index.ts";
import { press, texts } from "../testing/inputs.ts";
import { step } from "../testing/react.ts";
import type { BlockHost } from "../types/hosts.ts";
import { Slider } from "./index.ts";

type Range = Readonly<{ size?: number; span?: readonly number[] }>;

// A host whose fixture lists the state given and those that a test submits.
function host(state: Range, ...submits: Range[]): StubBlockHost<Range> {
  return stubBlockHost<Range>({ states: [state, ...submits], requests: [] }, { state });
}

afterEach(() => {
  document.body.replaceChildren();
});

// Draws the component on the page, and unmounts it.
async function draw(node: ReactElement): Promise<{ el: HTMLElement; unmount: () => Promise<void> }> {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  await step(() => {
    root.render(node);
  });
  return {
    el,
    unmount: () =>
      step(() => {
        root.unmount();
      }),
  };
}

function Bound({ host, outdated = false }: { host: BlockHost<Range>; outdated?: boolean }): ReactElement {
  return (
    <>
      <Slider bound={useValue(host, "size")} label="Size" minValue={0} maxValue={100} step={5} outdated={outdated} />
      <Slider bound={useValue(host, "span")} label="Span" minValue={0} maxValue={10} thumbLabels={["From", "To"]} outdated={outdated} />
    </>
  );
}

// The slider whose label is the one given: what it shows at the right of the
// label, the numbers of its thumbs, and whether it marks itself invalid.
function slider(el: HTMLElement, label: string) {
  const found = [...el.querySelectorAll<HTMLElement>('[role="group"]')].find(
    (group) => el.ownerDocument.getElementById(group.getAttribute("aria-labelledby") ?? "")?.textContent === label,
  );
  if (found === undefined) throw new Error(`Found no slider ${label}.`);
  const thumbs = [...found.querySelectorAll<HTMLInputElement>('input[type="range"]')];
  return {
    shows: found.querySelector("output")?.textContent ?? null,
    thumbs: thumbs.map((thumb) => Number(thumb.value)),
    invalid: found.hasAttribute("data-invalid"),
    thumb: (i = 0): HTMLInputElement => {
      const thumb = thumbs[i];
      if (thumb === undefined) throw new Error(`Found no thumb ${String(i)} of ${label}.`);
      return thumb;
    },
  };
}

// Presses the key on the thumb, as the user steps it, and waits for the
// submit it makes to settle.
async function stepped(thumb: HTMLInputElement, key: string): Promise<void> {
  await act(async () => {
    thumb.focus();
    press(thumb, key);
    await Promise.resolve();
  });
}

describe("a slider", () => {
  test("shows its number at the right of its label, or the numbers of its thumbs", async () => {
    const { el } = await draw(
      <>
        <Slider label="Size" defaultValue={30} />
        <Slider label="Span" defaultValue={[2, 6]} maxValue={10} thumbLabels={["From", "To"]} />
      </>,
    );

    expect([slider(el, "Size").shows, slider(el, "Size").thumbs]).toEqual(["30", [30]]);
    expect([slider(el, "Span").shows, slider(el, "Span").thumbs]).toEqual(["2–6", [2, 6]]);
    expect(slider(el, "Span").thumb(1).getAttribute("aria-label")).toBe("To");
  });
});

describe("a bound slider", () => {
  test("shows the number that the state holds, and submits a step of a thumb by a key at once, as one edit", async () => {
    const stub = host({ size: 20, span: [2, 6] }, { size: 25, span: [2, 6] }, { size: 25, span: [2, 5] });
    const { el } = await draw(<Bound host={stub} />);
    expect([slider(el, "Size").shows, slider(el, "Span").shows]).toEqual(["20", "2–6"]);

    await stepped(slider(el, "Size").thumb(), "ArrowRight");
    await stepped(slider(el, "Span").thumb(1), "ArrowLeft");

    expect(stub.submitted).toEqual([
      { size: 25, span: [2, 6] },
      { size: 25, span: [2, 5] },
    ]);
    expect([slider(el, "Size").shows, slider(el, "Span").shows]).toEqual(["25", "2–5"]);
  });

  test("takes a state it did not make, and shows the state again where the host refused a step, saying why below itself", async () => {
    const stub = host({ size: 20, span: [2, 6] }, { size: 50, span: [2, 6] });
    const { el } = await draw(<Bound host={stub} />);

    await step(() => {
      stub.setState({ size: 50, span: [2, 6] });
    });
    expect(slider(el, "Size").thumbs).toEqual([50]);

    await stepped(slider(el, "Size").thumb(), "ArrowRight");
    expect(stub.submitted).toEqual([{ size: 55, span: [2, 6] }]);
    expect([slider(el, "Size").shows, slider(el, "Size").thumbs]).toEqual(["50", [50]]);
    expect(texts(el, '[role="alert"]')[0]).toContain("which the fixture does not list");
  });

  test("shows a number outside its range, or off its step, as missing, with the thumb at the nearest end, and marks itself invalid, but not while outdated", async () => {
    const state: Range = { size: 150, span: [2, 12] };
    const shown = async (outdated: boolean) => {
      const { el, unmount } = await draw(<Bound host={host(state)} outdated={outdated} />);
      const seen = [slider(el, "Size"), slider(el, "Span")].map(({ shows, thumbs, invalid }) => ({ shows, thumbs, invalid }));
      await unmount();
      return seen;
    };

    expect(await shown(false)).toEqual([
      { shows: "150 (not available)", thumbs: [100], invalid: true },
      { shows: "2–12 (not available)", thumbs: [2, 10], invalid: true },
    ]);
    expect(await shown(true)).toEqual([
      { shows: "150", thumbs: [100], invalid: false },
      { shows: "2–12", thumbs: [2, 10], invalid: false },
    ]);

    const { el } = await draw(<Bound host={host({ size: 22 })} />);
    expect([slider(el, "Size").shows, slider(el, "Size").thumbs, slider(el, "Size").invalid]).toEqual(["22 (not available)", [20], true]);
  });

  test("submits no number that it pulled into its range, as where the user steps a thumb against its end, but one that the user moves it to", async () => {
    const stub = host({ size: 150 }, { size: 95 });
    const { el } = await draw(<Bound host={stub} />);

    await stepped(slider(el, "Size").thumb(), "ArrowRight");
    await stepped(slider(el, "Size").thumb(), "End");
    expect(stub.submitted).toEqual([]);
    expect(slider(el, "Size").shows).toBe("150 (not available)");

    await stepped(slider(el, "Size").thumb(), "ArrowLeft");
    expect(stub.submitted).toEqual([{ size: 95 }]);
    expect([slider(el, "Size").shows, slider(el, "Size").invalid]).toEqual(["95", false]);
  });

  test("shows nothing at the right of its label while the state arg is unset, and submits the number that the user moves to", async () => {
    const stub = host({}, { size: 5 });
    const { el } = await draw(<Bound host={stub} />);
    expect([slider(el, "Size").shows, slider(el, "Size").thumbs]).toEqual(["", [0]]);

    await stepped(slider(el, "Size").thumb(), "ArrowRight");

    expect(stub.submitted).toEqual([{ size: 5 }]);
  });
});
