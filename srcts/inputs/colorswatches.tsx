// The swatches of core's React SDK: React Aria's ColorSwatch
// (https://react-aria.adobe.com/ColorSwatch), which shows a colour, and
// ColorSwatchPicker (https://react-aria.adobe.com/ColorSwatchPicker), which
// picks one of the colours that its items show, in core's theme.
// They are drawn after the ColorSwatch and the ColorSwatchPicker of React
// Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/ColorSwatch.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/ColorSwatchPicker.tsx),
// and take the props of React Aria's parts.
// A swatch draws its colour over a checkerboard, which shows through a colour
// that is not opaque.
// React Aria names it by its colour, such as "light pale green cyan", so that
// a screen reader says which colour it shows.
// A swatch is a square of 2rem with rounded corners.
// The classes that its caller gives go after its own, as on the SDK's other
// components, so a class that sets what one of its own sets, such as another
// size, wins only with Tailwind's important modifier, as `bc:size-3!`.
// The picker lays its items out in a row that wraps, and marks the item of
// its colour with a ring.
//
// Given `bound`, the picker shows and picks a colour, a block's state arg or
// a board option's value, which useValue() binds, as the SDK's other inputs
// do: a string of hex (./colors.ts).
// It submits the colour that the user picks at once.
// It says below itself why the host refused what it submitted.
// Inside a bound ColorPicker, a pick is one that the picker submits.

import { useContext } from "react";
import type { ReactElement } from "react";
import {
  ColorSwatch as AriaColorSwatch,
  ColorSwatchPicker as AriaColorSwatchPicker,
  ColorSwatchPickerItem as AriaColorSwatchPickerItem,
  composeRenderProps,
} from "../aria/index.ts";
import type { ColorSwatchPickerItemProps, ColorSwatchPickerProps as AriaColorSwatchPickerProps, ColorSwatchProps } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
import { Ended, FALLBACK, hex, valueColor } from "./colors.ts";
import { Refusal } from "./inputs.tsx";

export function ColorSwatch({ className, style, ...props }: ColorSwatchProps): ReactElement {
  return (
    <AriaColorSwatch
      {...props}
      className={composeRenderProps(className, (given) => `${SWATCH} ${given ?? ""}`)}
      style={composeRenderProps(style, (given, { color }) => ({
        ...given,
        background: `linear-gradient(${color.toString()}, ${color.toString()}), ${CHECKERBOARD}`,
      }))}
    />
  );
}

export interface ColorSwatchPickerProps extends AriaColorSwatchPickerProps {
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
}

export function ColorSwatchPicker({ bound, ...props }: ColorSwatchPickerProps): ReactElement {
  return bound === undefined ? <Drawn {...props} /> : <BoundSwatches {...props} bound={bound} />;
}

function BoundSwatches({ bound, onChange, ...props }: AriaColorSwatchPickerProps & { readonly bound: Bound }): ReactElement {
  return (
    <>
      <Drawn
        {...props}
        value={valueColor(bound.value) ?? FALLBACK}
        onChange={(color) => {
          void bound.commit(hex(color));
          onChange?.(color);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// The picker, which tells a bound ColorPicker around it of each pick.
function Drawn({ className, onChange, ...props }: AriaColorSwatchPickerProps): ReactElement {
  const ended = useContext(Ended);
  return (
    <AriaColorSwatchPicker
      {...props}
      onChange={(color) => {
        onChange?.(color);
        ended?.(color);
      }}
      className={composeRenderProps(className, (given, { layout }) => `${PICKER} ${layout === "stack" ? "bc:flex-col" : "bc:flex-wrap"} ${given ?? ""}`)}
    />
  );
}

export function ColorSwatchPickerItem({ className, ...props }: ColorSwatchPickerItemProps): ReactElement {
  return (
    <AriaColorSwatchPickerItem {...props} className={composeRenderProps(className, (given) => `${ITEM} ${given ?? ""}`)}>
      {({ isSelected }) => (
        <>
          <ColorSwatch />
          {isSelected ? <div className={SELECTED} /> : null}
        </>
      )}
    </AriaColorSwatchPickerItem>
  );
}

// The classes of a swatch, of the picker and of its items. The checkerboard
// is the starter kit's, two greys of 8 pixels.
const SWATCH = "bc:box-border bc:size-8 bc:shrink-0 bc:rounded-md bc:border bc:border-border-default";
const CHECKERBOARD = "repeating-conic-gradient(#ccc 0% 25%, white 0% 50%) 50% / 16px 16px";
const PICKER = "bc:flex bc:gap-1";
const ITEM =
  "bc:relative bc:cursor-pointer bc:rounded-xs bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
// The ring of the item picked: a dark ring inside a light one, which shows on
// any colour, and their colours swapped under the dark theme.
const SELECTED =
  "bc:absolute bc:top-0 bc:left-0 bc:box-border bc:size-full bc:rounded-md bc:border-2 bc:border-black bc:outline-2 bc:-outline-offset-4 bc:outline-white bc:dark:border-white bc:dark:outline-black bc:forced-color-adjust-none";
