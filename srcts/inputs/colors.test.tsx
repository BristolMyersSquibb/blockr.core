// The colours of core's React SDK, each bound with useValue() on the stub of a
// block's host, whose fixture lists the states that the host takes, and the
// swatch and the picker drawn for their caller.

import { act } from "react";
import type { ReactElement } from "react";
import { createRoot } from "react-dom/client";
import { afterEach, describe, expect, test } from "vitest";
import { parseColor } from "../aria/index.ts";
import { useValue } from "../react/index.ts";
import { stubBlockHost } from "../stubs/index.ts";
import type { StubBlockHost } from "../stubs/index.ts";
import { find, press, texts, type } from "../testing/inputs.ts";
import { openDialog } from "../testing/overlays.ts";
import { step } from "../testing/react.ts";
import type { BlockHost } from "../types/hosts.ts";
import {
  ColorArea,
  ColorField,
  ColorPicker,
  ColorSlider,
  ColorSwatch,
  ColorSwatchPicker,
  ColorSwatchPickerItem,
  ColorWheel,
} from "./index.ts";

type Fill = Readonly<{ fill: string }>;

// What unmounts each component that a test drew, which runs as the test ends.
const drawn: (() => void)[] = [];

afterEach(() => {
  act(() => {
    for (const unmount of drawn.splice(0)) unmount();
  });
  document.body.replaceChildren();
});

// Draws the component on the page, until the test ends.
function draw(node: ReactElement): HTMLElement {
  const el = document.createElement("div");
  document.body.append(el);
  const root = createRoot(el);
  drawn.push(() => {
    root.unmount();
  });
  act(() => {
    root.render(node);
  });
  return el;
}

// A block whose state holds the colour given, and whose host takes the
// colours given, the hex that a bound colour submits.
function block(fill: string, takes: readonly string[], ui: (host: BlockHost<Fill>) => ReactElement) {
  const host: StubBlockHost<Fill> = stubBlockHost<Fill>({ states: [fill, ...takes].map((value) => ({ fill: value })), requests: [] }, { state: { fill } });
  function Ui(): ReactElement {
    return ui(host);
  }
  return { host, el: draw(<Ui />) };
}

// The hex of a colour after a step of one of its channels in the colour space
// of HSB, as a part writes it.
// React Aria puts the channel on its step, which is 1 for each channel of HSB.
function stepped(hex: string, channel: "hue" | "brightness", by: number): string {
  const color = parseColor(hex).toFormat("hsb");
  return color.withChannelValue(channel, Math.round(color.getChannelValue(channel) + by)).toString("hex");
}

// The options of a swatch picker, by the names of their colours, which the
// swatch inside each gives, and the one picked.
function swatches(el: ParentNode): { names: string[]; picked: string[] } {
  const options = [...el.querySelectorAll('[role="option"]')];
  const name = (option: Element): string => option.querySelector('[role="img"]')?.getAttribute("aria-label") ?? "";
  return {
    names: options.map(name),
    picked: options.filter((option) => option.getAttribute("aria-selected") === "true").map(name),
  };
}

// The option of a swatch picker whose swatch shows the colour given.
function swatchOption(el: ParentNode, hex: string): HTMLElement {
  const found = [...el.querySelectorAll<HTMLElement>('[role="option"]')].find(
    (option) => option.querySelector('[role="img"]')?.getAttribute("aria-label") === named(hex),
  );
  if (found === undefined) throw new Error(`Found no swatch of ${hex}.`);
  return found;
}

// The name that React Aria gives a colour, by which a swatch is labelled.
const named = (hex: string): string => parseColor(hex).getColorName("en-US");

describe("a swatch", () => {
  test("is an image named by its colour, which it draws over a checkerboard", () => {
    const el = draw(<ColorSwatch color="#66c2a5" />);
    const swatch = find(el, '[role="img"]');

    expect(swatch.getAttribute("aria-label")).toBe("light pale green cyan");
    expect(swatch.getAttribute("aria-roledescription")).toBe("color swatch");
    expect(swatch.style.background).toContain("repeating-conic-gradient");
    expect(swatch.className).toContain("bc:size-8");
  });

  test("puts its caller's classes after its own, as the SDK's other components do", () => {
    const el = draw(<ColorSwatch color="#66c2a5" className="bc:size-3! bc:rounded-full!" />);
    const swatch = find(el, '[role="img"]');

    expect(swatch.className).toMatch(/bc:size-8 .*bc:size-3! bc:rounded-full!$/);
    expect(swatch.className).toContain("bc:border-border-default");
  });
});

describe("a bound colour field", () => {
  test("shows the state's colour as hex, and submits what the user types as the user presses Enter or leaves it", async () => {
    const { host, el } = block("#66c2a5", ["#8DA0CB", "#E78AC3"], (h) => <ColorField bound={useValue(h, "fill")} label="Fill" />);
    const input = find(el, "input", HTMLInputElement);
    expect(input.value).toBe("#66C2A5");

    await step(() => {
      type(input, "#8da0cb");
    });
    expect(host.submitted).toEqual([]);
    await step(() => {
      press(input, "Enter");
    });
    expect(host.submitted).toEqual([{ fill: "#8DA0CB" }]);
    expect(input.value).toBe("#8DA0CB");

    await step(() => {
      type(input, "e78ac3");
    });
    await step(() => {
      input.blur();
    });
    expect(host.submitted).toEqual([{ fill: "#8DA0CB" }, { fill: "#E78AC3" }]);
  });

  test("keeps the alpha of the state's colour, which its hex does not show", async () => {
    const { host, el } = block("#66c2a580", ["#8DA0CB80"], (h) => <ColorField bound={useValue(h, "fill")} label="Fill" />);
    const input = find(el, "input", HTMLInputElement);
    expect(input.value).toBe("#66C2A5");

    await step(() => {
      type(input, "#8da0cb");
      press(input, "Enter");
    });

    expect(host.submitted).toEqual([{ fill: "#8DA0CB80" }]);
  });

  test("keeps what the user types while the state changes, until the user leaves it", async () => {
    const { host, el } = block("#66c2a5", ["#FC8D62", "#8DA0CB"], (h) => <ColorField bound={useValue(h, "fill")} label="Fill" />);
    const input = find(el, "input", HTMLInputElement);

    await step(() => {
      type(input, "#8da0cb");
    });
    // The assistant sets another colour while the user types.
    await step(() => {
      host.setState({ fill: "#FC8D62" });
    });
    expect(input.value).toBe("#8da0cb");

    await step(() => {
      input.blur();
    });
    expect(host.submitted).toEqual([{ fill: "#8DA0CB" }]);
  });

  test("submits nothing where the user clears it or types no colour, and shows the state again", async () => {
    const { host, el } = block("#66c2a5", [], (h) => <ColorField bound={useValue(h, "fill")} label="Fill" />);
    const input = find(el, "input", HTMLInputElement);

    await step(() => {
      type(input, "");
      press(input, "Enter");
    });
    expect(input.value).toBe("#66C2A5");
    await step(() => {
      type(input, "#zz");
    });
    await step(() => {
      input.blur();
    });

    expect(input.value).toBe("#66C2A5");
    expect(host.submitted).toEqual([]);
  });

  test("shows the state again where the host refused the colour, and says why below itself", async () => {
    const { host, el } = block("#66c2a5", [], (h) => <ColorField bound={useValue(h, "fill")} label="Fill" />);
    const input = find(el, "input", HTMLInputElement);

    await act(async () => {
      type(input, "#8da0cb");
      press(input, "Enter");
      await Promise.resolve();
    });

    expect(host.submitted).toEqual([{ fill: "#8DA0CB" }]);
    expect(input.value).toBe("#66C2A5");
    expect(texts(el, '[role="alert"]')[0]).toContain("which the fixture does not list");
  });
});

describe("the bound parts of a picker", () => {
  test("submit the colour that a step of the user gives as the user ends it, an arrow key on the slider among them", async () => {
    const hue = stepped("#66c2a5", "hue", 1);
    const { host, el } = block("#66c2a5", [hue], (h) => <ColorSlider bound={useValue(h, "fill")} colorSpace="hsb" channel="hue" label="Hue" />);
    const slider = find(el, 'input[type="range"]', HTMLInputElement);
    expect(el.querySelector("output")?.textContent).toBe("161.09°");

    await step(() => {
      slider.focus();
      press(slider, "ArrowRight");
    });

    expect(host.submitted).toEqual([{ fill: hue }]);
    expect(el.querySelector("output")?.textContent).toBe("162°");
  });

  test("submit the colour of a step on the area and on the wheel at once, each as hex", async () => {
    const brighter = stepped("#66c2a5", "brightness", 1);
    const turned = stepped("#66c2a5", "hue", 1);
    const { host, el } = block("#66c2a5", [brighter, turned], (h) => {
      const fill = useValue(h, "fill");
      return (
        <>
          <ColorArea bound={fill} colorSpace="hsb" xChannel="saturation" yChannel="brightness" />
          <ColorWheel bound={fill} />
        </>
      );
    });
    const [, brightness, wheel] = [...el.querySelectorAll<HTMLInputElement>('input[type="range"]')];

    await step(() => {
      brightness?.focus();
      if (brightness !== undefined) press(brightness, "ArrowUp");
    });
    expect(host.submitted).toEqual([{ fill: brighter }]);

    await step(() => {
      host.setState({ fill: "#66c2a5" });
    });
    await step(() => {
      wheel?.focus();
      if (wheel !== undefined) press(wheel, "ArrowRight");
    });
    expect(host.submitted).toEqual([{ fill: brighter }, { fill: turned }]);
  });

  test("show the state again where the host refused a step, and say why below themselves", async () => {
    const { host, el } = block("#66c2a5", [], (h) => <ColorSlider bound={useValue(h, "fill")} colorSpace="hsb" channel="hue" label="Hue" />);
    const slider = find(el, 'input[type="range"]', HTMLInputElement);

    await act(async () => {
      slider.focus();
      press(slider, "ArrowRight");
      await Promise.resolve();
    });

    expect(host.submitted).toEqual([{ fill: stepped("#66c2a5", "hue", 1) }]);
    expect(el.querySelector("output")?.textContent).toBe("161.09°");
    expect(texts(el, '[role="alert"]')[0]).toContain("which the fixture does not list");
  });
});

describe("a bound swatch picker", () => {
  const palette = ["#66c2a5", "#fc8d62", "#8da0cb"] as const;

  test("marks the state's colour, and submits the colour that the user picks at once", async () => {
    const { host, el } = block("#66c2a5", ["#FC8D62"], (h) => (
      <ColorSwatchPicker bound={useValue(h, "fill")} aria-label="Palette">
        {palette.map((hex) => (
          <ColorSwatchPickerItem key={hex} color={hex} />
        ))}
      </ColorSwatchPicker>
    ));
    expect(swatches(el)).toEqual({ names: palette.map(named), picked: [named("#66c2a5")] });

    await step(() => {
      swatchOption(el, "#fc8d62").click();
    });

    expect(host.submitted).toEqual([{ fill: "#FC8D62" }]);
    expect(swatches(el).picked).toEqual([named("#fc8d62")]);
  });
});

describe("a colour picker", () => {
  test("shows its colour in a swatch on its button, which opens the starter kit's parts in a popover", async () => {
    const el = draw(<ColorPicker label="Fill" defaultValue="#66c2a5" />);
    const button = find(el, "button");
    expect(find(button, '[role="img"]').getAttribute("aria-label")).toBe("light pale green cyan");
    expect(button.textContent).toBe("Fill");

    await step(() => {
      button.click();
    });

    const dialog = openDialog();
    // The area holds two sliders, of the saturation and of the brightness, and
    // the slider of the hue takes the channel's name as its label.
    expect(dialog.querySelectorAll('input[type="range"]')).toHaveLength(3);
    expect(texts(dialog, "label")).toEqual(["Hue", "Hex"]);
    expect(find(dialog, 'input:not([type="range"])', HTMLInputElement).value).toBe("#66C2A5");
  });

  test("opens no popover while it is disabled", async () => {
    const el = draw(<ColorPicker label="Fill" defaultValue="#66c2a5" isDisabled />);
    const button = find(el, "button", HTMLButtonElement);
    expect(button.disabled).toBe(true);

    await step(() => {
      button.click();
    });

    expect(document.querySelector('[role="dialog"]')).toBeNull();
  });

  test("bound, submits the colour as the user ends a change in one of its parts: a step of the hue, a swatch picked or the hex typed", async () => {
    const hue = stepped("#66c2a5", "hue", 1);
    const { host, el } = block("#66c2a5", [hue, "#FC8D62", "#8DA0CB"], (h) => (
      <ColorPicker bound={useValue(h, "fill")} label="Fill">
        <ColorSlider colorSpace="hsb" channel="hue" label="Hue" />
        <ColorField label="Hex" />
        <ColorSwatchPicker aria-label="Palette">
          <ColorSwatchPickerItem color="#fc8d62" />
        </ColorSwatchPicker>
      </ColorPicker>
    ));
    await step(() => {
      find(el, "button").click();
    });
    const dialog = openDialog();

    const slider = find(dialog, 'input[type="range"]', HTMLInputElement);
    await step(() => {
      slider.focus();
      press(slider, "ArrowRight");
    });
    expect(host.submitted).toEqual([{ fill: hue }]);

    await step(() => {
      find(dialog, '[role="option"]').click();
    });
    expect(host.submitted).toEqual([{ fill: hue }, { fill: "#FC8D62" }]);
    expect(find(el, 'button [role="img"]').getAttribute("aria-label")).toBe(named("#fc8d62"));

    const field = find(dialog, 'input:not([type="range"])', HTMLInputElement);
    await step(() => {
      type(field, "#8da0cb");
      press(field, "Enter");
    });
    expect(host.submitted).toEqual([{ fill: hue }, { fill: "#FC8D62" }, { fill: "#8DA0CB" }]);
  });

  test("bound, shows the state again where the host refused a colour, and says why beside its button", async () => {
    const { host, el } = block("#66c2a5", [], (h) => (
      <ColorPicker bound={useValue(h, "fill")} label="Fill">
        <ColorSwatchPicker aria-label="Palette">
          <ColorSwatchPickerItem color="#fc8d62" />
        </ColorSwatchPicker>
      </ColorPicker>
    ));
    await step(() => {
      find(el, "button").click();
    });

    await act(async () => {
      find(openDialog(), '[role="option"]').click();
      await Promise.resolve();
    });

    expect(host.submitted).toEqual([{ fill: "#FC8D62" }]);
    expect(find(el, 'button [role="img"]').getAttribute("aria-label")).toBe(named("#66c2a5"));
    expect(texts(el, '[role="alert"]')[0]).toContain("which the fixture does not list");
  });
});
