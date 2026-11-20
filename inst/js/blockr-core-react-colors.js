// srcts/colors/field.tsx
import { useContext, useState as useState2 } from "react";
import { ColorField as AriaColorField, parseColor as parseColor2 } from "./vendor-colors-D2MAOI3F.js";
import { composeRenderProps, FieldError, Input, Label, Text } from "./vendor-P3O6DBKR.js";

// srcts/styles/styles.ts
var styles = {
  stack: "bc:grid bc:gap-3 bc:font-sans bc:text-text-default",
  field: "bc:grid bc:gap-1 bc:font-sans bc:text-text-default",
  label: "bc:text-sm bc:font-medium",
  box: "bc:min-w-0 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:focus:outline-2 bc:focus:outline-offset-1 bc:focus:outline-border-accent bc:aria-invalid:border-border-danger",
  check: "bc:flex bc:items-center bc:gap-2 bc:font-sans bc:text-sm bc:text-text-default",
  checkbox: "bc:accent-bg-accent bc:aria-invalid:accent-border-danger",
  fieldset: "bc:m-0 bc:grid bc:min-w-0 bc:gap-1 bc:border-0 bc:p-0 bc:font-sans bc:text-text-default",
  legend: "bc:text-sm bc:font-medium bc:mb-1 bc:p-0",
  list: "bc:grid bc:max-h-48 bc:gap-1 bc:overflow-y-auto",
  hint: "bc:m-0 bc:text-sm bc:text-text-muted",
  alert: "bc:m-0 bc:text-sm bc:text-text-danger bc:whitespace-pre-wrap"
};

// srcts/colors/bound.tsx
import { createContext, useState } from "react";
import { parseColor } from "./vendor-colors-D2MAOI3F.js";
import { jsx } from "react/jsx-runtime";
var FALLBACK = "#000000";
function hex(color) {
  return color.getChannelValue("alpha") < 1 ? color.toString("hexa") : color.toString("hex");
}
function valueColor(value) {
  if (typeof value !== "string") return void 0;
  try {
    parseColor(value);
  } catch {
    return void 0;
  }
  return value;
}
var Ended = createContext(null);
function useDragged(bound) {
  const value = valueColor(bound.value);
  const [kept, setKept] = useState(null);
  const [seen, setSeen] = useState(value);
  if (seen !== value) {
    setSeen(value);
    if (kept !== null && !kept.dragging && (value === void 0 || hex(parseColor(value)) !== hex(kept.color))) setKept(null);
  }
  return {
    shown: kept?.color ?? value ?? FALLBACK,
    change: (color) => {
      setKept({ color, dragging: true });
    },
    end: (color) => {
      setKept({ color, dragging: false });
      void bound.commit(hex(color)).then((took) => {
        if (!took) setKept((now) => now?.dragging === true ? now : null);
      });
    }
  };
}
function Refusal({ error }) {
  return error === null ? null : /* @__PURE__ */ jsx("p", { role: "alert", className: styles.alert, children: error });
}

// srcts/colors/field.tsx
import { Fragment, jsx as jsx2, jsxs } from "react/jsx-runtime";
function ColorField({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx2(Drawn, { ...props }) : /* @__PURE__ */ jsx2(BoundField, { ...props, bound });
}
function BoundField({ bound, onChange, onBlur, ...props }) {
  const value = valueColor(bound.value);
  const [typing, setTyping] = useState2(null);
  const shown = typing === null ? value : typing.value;
  return /* @__PURE__ */ jsxs(Fragment, { children: [
    /* @__PURE__ */ jsx2(
      Drawn,
      {
        ...props,
        value: shown ?? null,
        onInput: () => {
          setTyping((now) => now ?? { value: shown });
        },
        onChange: (color) => {
          setTyping(null);
          if (color !== null) {
            const alpha = shown === void 0 ? 1 : parseColor2(shown).getChannelValue("alpha");
            void bound.commit(hex(color.withChannelValue("alpha", alpha)));
          }
          onChange?.(color);
        },
        onBlur: (event) => {
          setTyping(null);
          onBlur?.(event);
        }
      }
    ),
    /* @__PURE__ */ jsx2(Refusal, { error: bound.error })
  ] });
}
function Drawn({ label, description, errorMessage, className, onChange, onInput, ...props }) {
  const ended = useContext(Ended);
  return /* @__PURE__ */ jsxs(
    AriaColorField,
    {
      ...props,
      onChange: (color) => {
        onChange?.(color);
        if (color !== null) ended?.(color);
      },
      className: composeRenderProps(className, (given) => `${styles.field} ${given ?? ""}`),
      children: [
        label === void 0 ? null : /* @__PURE__ */ jsx2(Label, { className: styles.label, children: label }),
        /* @__PURE__ */ jsx2(Input, { onInput, className: styles.box }),
        /* @__PURE__ */ jsx2(Text, { slot: "description", className: `${styles.hint} bc:empty:hidden`, children: description }),
        /* @__PURE__ */ jsx2(FieldError, { className: styles.alert, children: errorMessage })
      ]
    }
  );
}

// srcts/colors/parts.tsx
import { useContext as useContext2 } from "react";
import {
  ColorArea as AriaColorArea,
  ColorSlider as AriaColorSlider,
  ColorThumb as AriaColorThumb,
  ColorWheel as AriaColorWheel,
  ColorWheelTrack,
  SliderOutput,
  SliderTrack
} from "./vendor-colors-D2MAOI3F.js";
import { composeRenderProps as composeRenderProps2, Label as Label2 } from "./vendor-P3O6DBKR.js";
import { Fragment as Fragment2, jsx as jsx3, jsxs as jsxs2 } from "react/jsx-runtime";
function ColorThumb({ className, style, ...props }) {
  return /* @__PURE__ */ jsx3(
    AriaColorThumb,
    {
      ...props,
      className: composeRenderProps2(className, (given) => `${THUMB} ${given ?? ""}`),
      style: composeRenderProps2(style, (given, { defaultStyle, isDisabled }) => ({
        ...defaultStyle,
        ...given,
        backgroundColor: isDisabled ? void 0 : defaultStyle.backgroundColor,
        boxShadow: "0 0 0 1px black, inset 0 0 0 1px black"
      }))
    }
  );
}
function ColorArea({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx3(Area, { ...props }) : /* @__PURE__ */ jsx3(BoundPart, { Part: Area, props, bound });
}
function ColorSlider({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx3(Slider, { ...props }) : /* @__PURE__ */ jsx3(BoundPart, { Part: Slider, props, bound });
}
function ColorWheel({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx3(Wheel, { ...props }) : /* @__PURE__ */ jsx3(BoundPart, { Part: Wheel, props, bound });
}
function BoundPart({
  Part,
  props,
  bound
}) {
  const dragged = useDragged(bound);
  return /* @__PURE__ */ jsxs2(Fragment2, { children: [
    /* @__PURE__ */ jsx3(
      Part,
      {
        ...props,
        value: dragged.shown,
        onChange: (color) => {
          dragged.change(color);
          props.onChange?.(color);
        },
        onChangeEnd: (color) => {
          dragged.end(color);
          props.onChangeEnd?.(color);
        }
      }
    ),
    /* @__PURE__ */ jsx3(Refusal, { error: bound.error })
  ] });
}
function useEnd(onChangeEnd) {
  const ended = useContext2(Ended);
  return (color) => {
    onChangeEnd?.(color);
    ended?.(color);
  };
}
function Area({ className, style, onChangeEnd, ...props }) {
  return /* @__PURE__ */ jsx3(
    AriaColorArea,
    {
      ...props,
      onChangeEnd: useEnd(onChangeEnd),
      className: composeRenderProps2(className, (given) => `${AREA} ${given ?? ""}`),
      style: composeRenderProps2(style, (given, { defaultStyle, isDisabled }) => ({
        ...defaultStyle,
        ...given,
        background: isDisabled ? void 0 : defaultStyle.background
      })),
      children: /* @__PURE__ */ jsx3(ColorThumb, {})
    }
  );
}
function Slider({ label, className, onChangeEnd, ...props }) {
  return /* @__PURE__ */ jsxs2(
    AriaColorSlider,
    {
      ...props,
      onChangeEnd: useEnd(onChangeEnd),
      className: composeRenderProps2(className, (given) => `${SLIDER} ${given ?? ""}`),
      children: [
        /* @__PURE__ */ jsx3(Label2, { className: styles.label, children: label }),
        /* @__PURE__ */ jsx3(SliderOutput, { className: OUTPUT }),
        /* @__PURE__ */ jsx3(
          SliderTrack,
          {
            className: ({ orientation, isDisabled }) => `${TRACK} ${orientation === "vertical" ? "bc:h-50 bc:w-6" : "bc:h-6 bc:w-full"} ${isDisabled ? DISABLED : ""}`,
            style: ({ defaultStyle, isDisabled }) => ({
              ...defaultStyle,
              background: isDisabled ? void 0 : `${String(defaultStyle.background)}, ${CHECKERBOARD}`
            }),
            children: /* @__PURE__ */ jsx3(ColorThumb, {})
          }
        )
      ]
    }
  );
}
function Wheel({ onChangeEnd, ...props }) {
  return /* @__PURE__ */ jsxs2(AriaColorWheel, { ...props, onChangeEnd: useEnd(onChangeEnd), outerRadius: 100, innerRadius: 74, children: [
    /* @__PURE__ */ jsx3(
      ColorWheelTrack,
      {
        className: WHEEL_TRACK,
        style: ({ defaultStyle, isDisabled }) => ({
          ...defaultStyle,
          background: isDisabled ? void 0 : `${String(defaultStyle.background)}, ${CHECKERBOARD}`
        })
      }
    ),
    /* @__PURE__ */ jsx3(ColorThumb, {})
  ] });
}
var THUMB = "bc:box-border bc:size-4.5 bc:top-[50%] bc:left-[50%] bc:rounded-full bc:border-2 bc:border-white bc:data-focus-visible:size-8 bc:data-dragging:bg-gray-700 bc:dark:data-dragging:bg-gray-300 bc:forced-colors:data-dragging:bg-[ButtonBorder] bc:data-disabled:border-gray-300 bc:data-disabled:bg-gray-300 bc:dark:data-disabled:border-gray-700 bc:dark:data-disabled:bg-gray-800 bc:forced-colors:data-disabled:border-[GrayText] bc:forced-colors:data-disabled:bg-[GrayText]";
var AREA = "bc:aspect-square bc:w-full bc:max-w-56 bc:rounded-lg bc:bg-gray-300 bc:dark:bg-gray-800 bc:forced-colors:bg-[GrayText]";
var SLIDER = "bc:grid-cols-[1fr_auto] bc:flex-col bc:items-center bc:gap-2 bc:font-sans bc:text-text-default bc:data-[orientation=horizontal]:grid bc:data-[orientation=horizontal]:w-56 bc:data-[orientation=vertical]:flex";
var OUTPUT = "bc:text-sm bc:font-medium bc:text-text-muted bc:data-[orientation=vertical]:hidden";
var TRACK = "bc:group bc:col-span-2 bc:rounded-md";
var DISABLED = "bc:bg-gray-300 bc:dark:bg-gray-800 bc:forced-colors:bg-[GrayText]";
var WHEEL_TRACK = "bc:data-disabled:bg-gray-300 bc:dark:data-disabled:bg-gray-800 bc:forced-colors:data-disabled:bg-[GrayText]";
var CHECKERBOARD = "repeating-conic-gradient(#ccc 0% 25%, white 0% 50%) 50% / 16px 16px";

// srcts/colors/picker.tsx
import { ColorPicker as AriaColorPicker } from "./vendor-colors-D2MAOI3F.js";
import { Button } from "./vendor-P3O6DBKR.js";
import { Dialog, DialogTrigger, Popover } from "@blockr/core/react";

// srcts/colors/swatches.tsx
import { useContext as useContext3 } from "react";
import {
  ColorSwatch as AriaColorSwatch,
  ColorSwatchPicker as AriaColorSwatchPicker,
  ColorSwatchPickerItem as AriaColorSwatchPickerItem
} from "./vendor-colors-D2MAOI3F.js";
import { composeRenderProps as composeRenderProps3 } from "./vendor-P3O6DBKR.js";
import { Fragment as Fragment3, jsx as jsx4, jsxs as jsxs3 } from "react/jsx-runtime";
function ColorSwatch({ className, style, ...props }) {
  return /* @__PURE__ */ jsx4(
    AriaColorSwatch,
    {
      ...props,
      className: composeRenderProps3(className, (given) => `${SWATCH} ${given ?? SQUARE}`),
      style: composeRenderProps3(style, (given, { color }) => ({
        ...given,
        background: `linear-gradient(${color.toString()}, ${color.toString()}), ${CHECKERBOARD2}`
      }))
    }
  );
}
function ColorSwatchPicker({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx4(Drawn2, { ...props }) : /* @__PURE__ */ jsx4(BoundSwatches, { ...props, bound });
}
function BoundSwatches({ bound, onChange, ...props }) {
  return /* @__PURE__ */ jsxs3(Fragment3, { children: [
    /* @__PURE__ */ jsx4(
      Drawn2,
      {
        ...props,
        value: valueColor(bound.value) ?? FALLBACK,
        onChange: (color) => {
          void bound.commit(hex(color));
          onChange?.(color);
        }
      }
    ),
    /* @__PURE__ */ jsx4(Refusal, { error: bound.error })
  ] });
}
function Drawn2({ className, onChange, ...props }) {
  const ended = useContext3(Ended);
  return /* @__PURE__ */ jsx4(
    AriaColorSwatchPicker,
    {
      ...props,
      onChange: (color) => {
        onChange?.(color);
        ended?.(color);
      },
      className: composeRenderProps3(className, (given, { layout }) => `${PICKER} ${layout === "stack" ? "bc:flex-col" : "bc:flex-wrap"} ${given ?? ""}`)
    }
  );
}
function ColorSwatchPickerItem({ className, ...props }) {
  return /* @__PURE__ */ jsx4(AriaColorSwatchPickerItem, { ...props, className: composeRenderProps3(className, (given) => `${ITEM} ${given ?? ""}`), children: ({ isSelected }) => /* @__PURE__ */ jsxs3(Fragment3, { children: [
    /* @__PURE__ */ jsx4(ColorSwatch, {}),
    isSelected ? /* @__PURE__ */ jsx4("div", { className: SELECTED }) : null
  ] }) });
}
var SWATCH = "bc:box-border bc:shrink-0 bc:border bc:border-border-default";
var SQUARE = "bc:size-8 bc:rounded-md";
var CHECKERBOARD2 = "repeating-conic-gradient(#ccc 0% 25%, white 0% 50%) 50% / 16px 16px";
var PICKER = "bc:flex bc:gap-1";
var ITEM = "bc:relative bc:cursor-pointer bc:rounded-xs bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var SELECTED = "bc:absolute bc:top-0 bc:left-0 bc:box-border bc:size-full bc:rounded-md bc:border-2 bc:border-black bc:outline-2 bc:-outline-offset-4 bc:outline-white bc:dark:border-white bc:dark:outline-black bc:forced-color-adjust-none";

// srcts/colors/picker.tsx
import { Fragment as Fragment4, jsx as jsx5, jsxs as jsxs4 } from "react/jsx-runtime";
function ColorPicker({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx5(Drawn3, { ...props }) : /* @__PURE__ */ jsx5(BoundPicker, { ...props, bound });
}
function BoundPicker({ bound, onChange, ...props }) {
  const dragged = useDragged(bound);
  return /* @__PURE__ */ jsxs4(Fragment4, { children: [
    /* @__PURE__ */ jsx5(Ended, { value: dragged.end, children: /* @__PURE__ */ jsx5(
      Drawn3,
      {
        ...props,
        value: dragged.shown,
        onChange: (color) => {
          dragged.change(color);
          onChange?.(color);
        }
      }
    ) }),
    /* @__PURE__ */ jsx5(Refusal, { error: bound.error })
  ] });
}
function Drawn3({ label, children, isDisabled, ...props }) {
  return /* @__PURE__ */ jsx5(AriaColorPicker, { ...props, children: /* @__PURE__ */ jsxs4(DialogTrigger, { children: [
    /* @__PURE__ */ jsxs4(Button, { isDisabled, className: BUTTON, children: [
      /* @__PURE__ */ jsx5(ColorSwatch, {}),
      label === void 0 ? null : /* @__PURE__ */ jsx5("span", { children: label })
    ] }),
    /* @__PURE__ */ jsx5(Popover, { placement: "bottom start", children: /* @__PURE__ */ jsx5(Dialog, { className: "bc:flex bc:flex-col bc:gap-2", children: children ?? /* @__PURE__ */ jsxs4(Fragment4, { children: [
      /* @__PURE__ */ jsx5(ColorArea, { colorSpace: "hsb", xChannel: "saturation", yChannel: "brightness" }),
      /* @__PURE__ */ jsx5(ColorSlider, { colorSpace: "hsb", channel: "hue" }),
      /* @__PURE__ */ jsx5(ColorField, { label: "Hex" })
    ] }) }) })
  ] }) });
}
var BUTTON = "bc:flex bc:cursor-pointer bc:items-center bc:gap-2 bc:rounded-xs bc:border-0 bc:bg-transparent bc:p-0 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
export {
  ColorArea,
  ColorField,
  ColorPicker,
  ColorSlider,
  ColorSwatch,
  ColorSwatchPicker,
  ColorSwatchPickerItem,
  ColorThumb,
  ColorWheel
};
