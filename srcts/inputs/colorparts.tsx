// The parts of a colour picker in core's React SDK, React Aria's in core's
// theme: the area, ColorArea (https://react-aria.adobe.com/ColorArea), which
// changes two channels of a colour, such as its saturation and its
// brightness, the slider, ColorSlider
// (https://react-aria.adobe.com/ColorSlider), which changes one, such as its
// hue, the wheel, ColorWheel (https://react-aria.adobe.com/ColorWheel), which
// changes its hue around a ring, and the thumb, ColorThumb, which the user
// drags in each of them.
// They are drawn after the ColorArea, the ColorSlider, the ColorWheel and the
// ColorThumb of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/tree/react-aria-components%401.22.1/starters/tailwind/src),
// and take the props of React Aria's parts, and the slider the label that the
// starter kit adds.
// The area is a square of at most 14rem, the slider a track of 14rem, with
// its label and the value of its channel above it, and the wheel a ring of
// 200 pixels across.
// The track of a slider draws the colours of its channel over a
// checkerboard, which shows through the colours of a channel of alpha.
//
// Given `bound`, each shows and changes a colour, a block's state arg or a
// board option's value, which useValue() binds, as the SDK's other inputs do:
// a string of hex (./colors.ts).
// It shows the colour that the user drags it to, and submits that colour as
// the user lets go, through React Aria's onChangeEnd, which a step by an arrow
// key ends too.
// It says below itself why the host refused what it submitted.
// Inside a bound ColorPicker, the user letting go of a part is a change that
// the picker submits.

import { useContext } from "react";
import type { ReactElement } from "react";
import {
  ColorArea as AriaColorArea,
  ColorSlider as AriaColorSlider,
  ColorThumb as AriaColorThumb,
  ColorWheel as AriaColorWheel,
  ColorWheelTrack,
  composeRenderProps,
  Label,
  SliderOutput,
  SliderTrack,
} from "../aria/index.ts";
import type {
  Color,
  ColorAreaProps as AriaColorAreaProps,
  ColorSliderProps as AriaColorSliderProps,
  ColorThumbProps,
  ColorWheelProps as AriaColorWheelProps,
} from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import { Ended, useDragged } from "./colors.ts";
import { Refusal } from "./inputs.tsx";

export function ColorThumb({ className, style, ...props }: ColorThumbProps): ReactElement {
  return (
    <AriaColorThumb
      {...props}
      className={composeRenderProps(className, (given) => `${THUMB} ${given ?? ""}`)}
      style={composeRenderProps(style, (given, { defaultStyle, isDisabled }) => ({
        ...defaultStyle,
        ...given,
        backgroundColor: isDisabled ? undefined : defaultStyle.backgroundColor,
        boxShadow: "0 0 0 1px black, inset 0 0 0 1px black",
      }))}
    />
  );
}

export interface ColorAreaProps extends AriaColorAreaProps {
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
}

export function ColorArea({ bound, ...props }: ColorAreaProps): ReactElement {
  return bound === undefined ? <Area {...props} /> : <BoundPart Part={Area} props={props} bound={bound} />;
}

export interface ColorSliderProps extends AriaColorSliderProps {
  // What the starter kit adds.
  readonly label?: string;
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
}

export function ColorSlider({ bound, ...props }: ColorSliderProps): ReactElement {
  return bound === undefined ? <Slider {...props} /> : <BoundPart Part={Slider} props={props} bound={bound} />;
}

export interface ColorWheelProps extends Omit<AriaColorWheelProps, "outerRadius" | "innerRadius"> {
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
}

export function ColorWheel({ bound, ...props }: ColorWheelProps): ReactElement {
  return bound === undefined ? <Wheel {...props} /> : <BoundPart Part={Wheel} props={props} bound={bound} />;
}

// What a part that the user drags takes, of what React Aria's take.
interface Dragged {
  readonly value?: string | Color;
  readonly onChange?: (color: Color) => void;
  readonly onChangeEnd?: (color: Color) => void;
}

// A part bound to the value, which shows the colour that the user drags it
// to, and submits it as the user lets go.
function BoundPart<P extends Dragged>({
  Part,
  props,
  bound,
}: {
  Part: (props: P) => ReactElement;
  props: P;
  bound: Bound;
}): ReactElement {
  const dragged = useDragged(bound);
  return (
    <>
      <Part
        {...props}
        value={dragged.shown}
        onChange={(color: Color) => {
          dragged.change(color);
          props.onChange?.(color);
        }}
        onChangeEnd={(color: Color) => {
          dragged.end(color);
          props.onChangeEnd?.(color);
        }}
      />
      <Refusal error={bound.error} />
    </>
  );
}

// Calls onChangeEnd, and tells a bound ColorPicker around the part that the
// user let go of it.
function useEnd(onChangeEnd: ((color: Color) => void) | undefined): (color: Color) => void {
  const ended = useContext(Ended);
  return (color) => {
    onChangeEnd?.(color);
    ended?.(color);
  };
}

function Area({ className, style, onChangeEnd, ...props }: AriaColorAreaProps): ReactElement {
  return (
    <AriaColorArea
      {...props}
      onChangeEnd={useEnd(onChangeEnd)}
      className={composeRenderProps(className, (given) => `${AREA} ${given ?? ""}`)}
      style={composeRenderProps(style, (given, { defaultStyle, isDisabled }) => ({
        ...defaultStyle,
        ...given,
        background: isDisabled ? undefined : defaultStyle.background,
      }))}
    >
      <ColorThumb />
    </AriaColorArea>
  );
}

function Slider({ label, className, onChangeEnd, ...props }: Omit<ColorSliderProps, "bound">): ReactElement {
  return (
    <AriaColorSlider
      {...props}
      onChangeEnd={useEnd(onChangeEnd)}
      className={composeRenderProps(className, (given) => `${SLIDER} ${given ?? ""}`)}
    >
      <Label className={styles.label}>{label}</Label>
      <SliderOutput className={OUTPUT} />
      <SliderTrack
        className={({ orientation, isDisabled }) =>
          `${TRACK} ${orientation === "vertical" ? "bc:h-50 bc:w-6" : "bc:h-6 bc:w-full"} ${isDisabled ? DISABLED : ""}`
        }
        style={({ defaultStyle, isDisabled }) => ({
          ...defaultStyle,
          background: isDisabled ? undefined : `${String(defaultStyle.background)}, ${CHECKERBOARD}`,
        })}
      >
        <ColorThumb />
      </SliderTrack>
    </AriaColorSlider>
  );
}

function Wheel({ onChangeEnd, ...props }: Omit<ColorWheelProps, "bound">): ReactElement {
  return (
    <AriaColorWheel {...props} onChangeEnd={useEnd(onChangeEnd)} outerRadius={100} innerRadius={74}>
      <ColorWheelTrack
        className={WHEEL_TRACK}
        style={({ defaultStyle, isDisabled }) => ({
          ...defaultStyle,
          background: isDisabled ? undefined : `${String(defaultStyle.background)}, ${CHECKERBOARD}`,
        })}
      />
      <ColorThumb />
    </AriaColorWheel>
  );
}

// The classes of the parts. The thumb grows while it shows the focus of the
// keyboard, and a disabled part draws in greys, through the attributes that
// React Aria sets on it.
const THUMB =
  "bc:box-border bc:size-4.5 bc:top-[50%] bc:left-[50%] bc:rounded-full bc:border-2 bc:border-white bc:data-focus-visible:size-8 bc:data-dragging:bg-gray-700 bc:dark:data-dragging:bg-gray-300 bc:forced-colors:data-dragging:bg-[ButtonBorder] bc:data-disabled:border-gray-300 bc:data-disabled:bg-gray-300 bc:dark:data-disabled:border-gray-700 bc:dark:data-disabled:bg-gray-800 bc:forced-colors:data-disabled:border-[GrayText] bc:forced-colors:data-disabled:bg-[GrayText]";
const AREA =
  "bc:aspect-square bc:w-full bc:max-w-56 bc:rounded-lg bc:bg-gray-300 bc:dark:bg-gray-800 bc:forced-colors:bg-[GrayText]";
const SLIDER =
  "bc:grid-cols-[1fr_auto] bc:flex-col bc:items-center bc:gap-2 bc:font-sans bc:text-text-default bc:data-[orientation=horizontal]:grid bc:data-[orientation=horizontal]:w-56 bc:data-[orientation=vertical]:flex";
const OUTPUT = "bc:text-sm bc:font-medium bc:text-text-muted bc:data-[orientation=vertical]:hidden";
const TRACK = "bc:group bc:col-span-2 bc:rounded-md";
const DISABLED = "bc:bg-gray-300 bc:dark:bg-gray-800 bc:forced-colors:bg-[GrayText]";
const WHEEL_TRACK = "bc:data-disabled:bg-gray-300 bc:dark:data-disabled:bg-gray-800 bc:forced-colors:data-disabled:bg-[GrayText]";
const CHECKERBOARD = "repeating-conic-gradient(#ccc 0% 25%, white 0% 50%) 50% / 16px 16px";
