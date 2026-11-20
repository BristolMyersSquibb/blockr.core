// The slider of core's React SDK: React Aria's Slider
// (https://react-aria.adobe.com/Slider), in core's theme, after the Slider of
// React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Slider.tsx).
// It takes what that takes: the props of React Aria's Slider, such as
// minValue, maxValue, step and formatOptions, and the label, the labels of
// the thumbs and the offset of the fill that the starter kit adds.
// It has one thumb, or one for each number where its value is an array.
// The number, or the numbers, show at the right of the label.
//
// Given `bound`, it shows and edits a number, or an array of numbers where it
// has several thumbs, that useValue() binds, a block's state arg, a board
// option's value or a value that a function reads from a block's state, as
// the SDK's other inputs do (./inputs.tsx).
// It shows the number that it moves to while the user drags a thumb, and
// submits it as the user lets go, through onChangeEnd, so that a drag is one
// edit and one undo step, as it submits a step of a thumb by a key.
// It keeps the number that the user let go of until the binding shows a value
// again, or the host refuses it.
// It submits only a number that the user gave it, so never one that it
// pulled into its range or onto its step, as where the user lets go of a
// thumb without moving it.
// So where the user moves one thumb of several, it submits the numbers of the
// others as the value holds them.
// React Aria draws a thumb only within the range, and on the step, so a
// thumb of a number outside them sits at the end, or on the step, nearest to
// it.
// The slider then shows the number at the right of the label as it is, marks
// itself invalid, and says below itself that the number is not available, as
// the select says of a value that its items lack (./combobox.tsx), but not
// while it is `outdated`, as while the request that gives its maxValue asks
// again.
// The user drops such a number by moving its thumb.
// It says below itself why the host refused what it submitted.

import { useContext, useLayoutEffect, useRef, useState } from "react";
import type { ReactElement } from "react";
import {
  composeRenderProps,
  Label,
  Slider as AriaSlider,
  SliderFill,
  SliderOutput,
  SliderStateContext,
  SliderThumb,
  SliderTrack,
  useLocale,
} from "../aria/index.ts";
import type { SliderProps as AriaSliderProps } from "../aria/index.ts";
import type { Bound } from "../react/index.ts";
import { styles } from "../styles/index.ts";
import type { Json } from "../types/json.ts";
import { jsonEqual } from "../util/index.ts";
import { NotAvailable, Refusal } from "./inputs.tsx";
import { inRange } from "./numberfield.tsx";

type Value = number | number[];

export interface SliderProps<T extends Value = Value> extends AriaSliderProps<T> {
  // What the starter kit adds.
  readonly label?: string;
  readonly thumbLabels?: readonly string[];
  readonly fillOffset?: number;
  // The binding of a block's state arg or of a board option's value.
  readonly bound?: Bound;
  readonly outdated?: boolean;
}

export function Slider<T extends Value = Value>({ bound, outdated = false, ...props }: SliderProps<T>): ReactElement {
  return bound === undefined ? (
    <Drawn {...(props as DrawnProps)} />
  ) : (
    <BoundSlider {...(props as DrawnProps)} bound={bound} outdated={outdated} />
  );
}

type DrawnProps = Omit<SliderProps, "bound" | "outdated"> & {
  // What the output says in place of React Aria's numbers, as where the
  // slider holds a number that it lacks, and whether it marks the slider
  // invalid.
  readonly said?: (format: (value: number) => string) => string;
  readonly invalid?: boolean;
  // What reads the numbers that React Aria shows, inside the slider.
  readonly watch?: ReactElement;
};

function BoundSlider({
  bound,
  outdated,
  onChange,
  onChangeEnd,
  ...props
}: DrawnProps & { readonly bound: Bound; readonly outdated: boolean }): ReactElement {
  const several = Array.isArray(bound.value) || Array.isArray(props.defaultValue);
  const held = numbers(bound.value);
  // The numbers that the user moves the thumbs to, from the first move of a
  // drag until the binding shows a value again, or the host refused them.
  const [moving, setMoving] = useState<readonly number[] | null>(null);
  const [seen, setSeen] = useState<Json | undefined>(bound.value);
  if (!jsonEqual(seen, bound.value)) {
    setSeen(bound.value);
    setMoving(null);
  }
  // The numbers that React Aria showed while no thumb moved, and those it
  // showed as the drag began, which the numbers let go of must differ from.
  const shown = useRef<readonly number[]>([]);
  const start = useRef<readonly number[] | null>(null);

  const min = props.minValue ?? 0;
  const fits = (value: number): boolean => inRange(value, min, props.maxValue ?? 100, props.step ?? 1);
  const lacked = moving === null ? held.filter((value) => !fits(value)) : [];
  const marking = !outdated && lacked.length > 0;
  const drawn = moving ?? (held.length > 0 ? held : [min]);
  const value = several ? [...drawn] : (drawn[0] ?? min);
  const { locale } = useLocale();

  return (
    <>
      <Drawn
        {...props}
        value={value}
        invalid={marking}
        said={
          moving === null && held.length === 0
            ? () => ""
            : lacked.length > 0
              ? (formatted) => held.map(formatted).join(held.length === 2 ? "–" : ", ")
              : undefined
        }
        onChange={(next) => {
          start.current ??= shown.current;
          setMoving(numbers(next));
          onChange?.(next);
        }}
        onChangeEnd={(next) => {
          const ended = numbers(next);
          const began = start.current ?? shown.current;
          start.current = null;
          if (jsonEqual(ended, began)) {
            setMoving(null);
          } else {
            // A thumb that the user did not move keeps the number that the
            // value holds, as one that the slider pulled in.
            const given = ended.map((x, i) => (Object.is(x, began[i]) ? (held[i] ?? x) : x));
            setMoving(ended);
            void bound.commit(several ? given : (given[0] ?? min)).then((took) => {
              if (!took) setMoving(null);
            });
          }
          onChangeEnd?.(next);
        }}
        watch={
          <Watch
            onShown={(values) => {
              shown.current = values;
            }}
          />
        }
      />
      {marking ? <NotAvailable values={lacked.map((x) => new Intl.NumberFormat(locale, props.formatOptions).format(x))} /> : null}
      <Refusal error={bound.error} />
    </>
  );
}

// Hands `onShown` the numbers that React Aria shows, each time it has drawn
// them while no thumb moves.
function Watch({ onShown }: { onShown: (values: readonly number[]) => void }): null {
  const state = useContext(SliderStateContext);
  useLayoutEffect(() => {
    if (state !== null && !state.values.some((_, i) => state.isThumbDragging(i))) onShown(state.values);
  });
  return null;
}

// The numbers of a value, as a slider holds them.
function numbers(value: Json | undefined | Value): number[] {
  if (typeof value === "number") return [value];
  return Array.isArray(value) ? value.filter((x): x is number => typeof x === "number") : [];
}

// The label and the numbers, the track with its fill and its thumbs.
function Drawn({ label, thumbLabels, fillOffset, said, invalid = false, watch, className, ...props }: DrawnProps): ReactElement {
  return (
    <AriaSlider
      {...props}
      data-invalid={invalid || undefined}
      className={composeRenderProps(className, (given) => `bc:group ${SLIDER} ${given ?? ""}`)}
    >
      {label === undefined ? <span /> : <Label className={styles.label}>{label}</Label>}
      <SliderOutput className={OUTPUT}>
        {({ state }) => (said === undefined ? state.getFormattedValue() : said((value) => state.getFormattedValue(value)))}
      </SliderOutput>
      <SliderTrack className={TRACK}>
        {({ state }) => (
          <>
            <div className={BAR}>
              <SliderFill offset={fillOffset} className={FILL} />
            </div>
            {state.values.map((_, i) => (
              <SliderThumb key={i} index={i} aria-label={thumbLabels?.[i]} className={THUMB} />
            ))}
          </>
        )}
      </SliderTrack>
      {watch}
    </AriaSlider>
  );
}

// The classes of the slider and its parts.
// The label and the numbers share a row above the track, which spans the
// slider.
// The numbers, the fill and the thumbs take the colour of danger where the
// slider is invalid, as where it shows a number that it lacks.
const SLIDER =
  "bc:grid bc:grid-cols-[1fr_auto] bc:items-center bc:gap-x-2 bc:gap-y-1 bc:font-sans bc:text-text-default bc:data-disabled:opacity-50";
const OUTPUT = "bc:text-sm bc:text-text-muted bc:tabular-nums bc:group-data-invalid:text-text-danger";
const TRACK = "bc:relative bc:col-span-2 bc:flex bc:h-5 bc:items-center bc:mx-2";
const BAR = "bc:relative bc:h-1.5 bc:w-full bc:rounded-full bc:bg-border-default";
const FILL = "bc:rounded-full bc:bg-bg-accent bc:group-data-invalid:bg-border-danger";
const THUMB =
  "bc:top-1/2 bc:box-border bc:size-4 bc:cursor-grab bc:rounded-full bc:border-2 bc:border-bg-accent bc:bg-bg-raised bc:shadow-xs bc:outline-offset-2 bc:outline-border-accent bc:data-dragging:cursor-grabbing bc:data-focus-visible:outline-2 bc:data-disabled:cursor-default bc:group-data-invalid:border-border-danger";
