// What the colours of core's React SDK share to bind a colour, a block's
// state arg or a board option's value, which useValue() binds, as the SDK's
// other inputs do (../inputs/inputs.tsx).
// A bound colour is a string of hex: "#RRGGBB", as React Aria's
// color.toString("hex") writes it, or "#RRGGBBAA" where the colour is not
// opaque, as toString("hexa") writes it.
// React Aria's parseColor() reads either back, in either case, and R reads
// both as colours, as ggplot2's `colour` does.
// So a part that changes no alpha, such as the slider of the hue, keeps the
// alpha of the colour that it shows.
// Where the value is no colour that parseColor() reads, a part shows black,
// as React Aria's picker starts, and the field shows nothing.

import { createContext, useState } from "react";
import type { ReactElement } from "react";
import { parseColor } from "../aria-colors/index.ts";
import type { Color } from "../aria-colors/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import type { Json } from "../types/json.ts";

// The colour that a part shows where the value holds none.
export const FALLBACK = "#000000";

// The hex of a colour, as a bound colour holds it.
export function hex(color: Color): string {
  return color.getChannelValue("alpha") < 1 ? color.toString("hexa") : color.toString("hex");
}

// The value, where it is a colour that parseColor() reads.
export function valueColor(value: Json | undefined): string | undefined {
  if (typeof value !== "string") return undefined;
  try {
    parseColor(value);
  } catch {
    return undefined;
  }
  return value;
}

// What a bound ColorPicker does as the user ends a change in one of its parts:
// lets go of its area, a slider or its wheel, picks a swatch, or leaves its
// field or presses Enter there. The SDK's parts tell it through this context,
// so that the picker submits the colour then, whichever parts it holds.
export const Ended = createContext<((color: Color) => void) | null>(null);

// The colour that a part shows as the user drags it, and lets go of it:
// 1. While the user drags the part, it shows the colour that the user dragged
//    it to.
// 2. As the user lets go, the binding submits that colour at once, and the
//    part keeps showing it.
// 3. Once the host announces another colour, the part shows the host's.
//    Where the host refused the colour, the part shows the host's colour
//    again.
// The part keeps the colour as React Aria gave it, in the part's colour
// space, rather than as the hex that the binding submits.
// The hex of a grey holds no hue, so the hue stays where the user left it.
export function useDragged(bound: Bound): {
  readonly shown: Color | string;
  readonly change: (color: Color) => void;
  readonly end: (color: Color) => void;
} {
  const value = valueColor(bound.value);
  const [kept, setKept] = useState<{ readonly color: Color; readonly dragging: boolean } | null>(null);
  const [seen, setSeen] = useState(value);
  if (seen !== value) {
    setSeen(value);
    if (kept !== null && !kept.dragging && (value === undefined || hex(parseColor(value)) !== hex(kept.color))) setKept(null);
  }
  return {
    shown: kept?.color ?? value ?? FALLBACK,
    change: (color) => {
      setKept({ color, dragging: true });
    },
    end: (color) => {
      setKept({ color, dragging: false });
      void bound.commit(hex(color)).then((took) => {
        if (!took) setKept((now) => (now?.dragging === true ? now : null));
      });
    },
  };
}

// Says why the host refused what a part submitted, as the SDK's other inputs
// say it below themselves.
// The colours reach the SDK only through what `@blockr/core/react` exports,
// which leaves out the inputs' own, so they draw it themselves.
export function Refusal({ error }: { error: string | null }): ReactElement | null {
  return error === null ? null : (
    <p role="alert" className={styles.alert}>
      {error}
    </p>
  );
}
