// srcts/colors/field.tsx
import { useContext, useState as useState2 } from "react";

// srcts/aria-colors/index.ts
import { ColorArea } from "react-aria-components/ColorArea";
import { ColorField } from "react-aria-components/ColorField";
import { ColorPicker } from "react-aria-components/ColorPicker";
import { ColorSlider, SliderOutput, SliderTrack } from "react-aria-components/ColorSlider";
import { ColorSwatch } from "react-aria-components/ColorSwatch";
import { ColorSwatchPicker, ColorSwatchPickerItem } from "react-aria-components/ColorSwatchPicker";
import { ColorThumb } from "react-aria-components/ColorThumb";
import { ColorWheel, ColorWheelTrack } from "react-aria-components/ColorWheel";
import { parseColor } from "react-aria-components/ColorPicker";

// srcts/aria/index.ts
import { Button } from "react-aria-components/Button";
import { composeRenderProps } from "react-aria-components/composeRenderProps";
import { Dialog, DialogTrigger } from "react-aria-components/Dialog";
import { FieldError } from "react-aria-components/FieldError";
import { Input } from "react-aria-components/Input";
import { Label } from "react-aria-components/Label";
import { Popover } from "react-aria-components/Popover";
import { Text } from "react-aria-components/Text";

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
function ColorField2({ bound, ...props }) {
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
            const alpha = shown === void 0 ? 1 : parseColor(shown).getChannelValue("alpha");
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
    ColorField,
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
import { Fragment as Fragment2, jsx as jsx3, jsxs as jsxs2 } from "react/jsx-runtime";
function ColorThumb2({ className, style, ...props }) {
  return /* @__PURE__ */ jsx3(
    ColorThumb,
    {
      ...props,
      className: composeRenderProps(className, (given) => `${THUMB} ${given ?? ""}`),
      style: composeRenderProps(style, (given, { defaultStyle, isDisabled }) => ({
        ...defaultStyle,
        ...given,
        backgroundColor: isDisabled ? void 0 : defaultStyle.backgroundColor,
        boxShadow: "0 0 0 1px black, inset 0 0 0 1px black"
      }))
    }
  );
}
function ColorArea2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx3(Area, { ...props }) : /* @__PURE__ */ jsx3(BoundPart, { Part: Area, props, bound });
}
function ColorSlider2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx3(Slider, { ...props }) : /* @__PURE__ */ jsx3(BoundPart, { Part: Slider, props, bound });
}
function ColorWheel2({ bound, ...props }) {
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
    ColorArea,
    {
      ...props,
      onChangeEnd: useEnd(onChangeEnd),
      className: composeRenderProps(className, (given) => `${AREA} ${given ?? ""}`),
      style: composeRenderProps(style, (given, { defaultStyle, isDisabled }) => ({
        ...defaultStyle,
        ...given,
        background: isDisabled ? void 0 : defaultStyle.background
      })),
      children: /* @__PURE__ */ jsx3(ColorThumb2, {})
    }
  );
}
function Slider({ label, className, onChangeEnd, ...props }) {
  return /* @__PURE__ */ jsxs2(
    ColorSlider,
    {
      ...props,
      onChangeEnd: useEnd(onChangeEnd),
      className: composeRenderProps(className, (given) => `${SLIDER} ${given ?? ""}`),
      children: [
        /* @__PURE__ */ jsx3(Label, { className: styles.label, children: label }),
        /* @__PURE__ */ jsx3(SliderOutput, { className: OUTPUT }),
        /* @__PURE__ */ jsx3(
          SliderTrack,
          {
            className: ({ orientation, isDisabled }) => `${TRACK} ${orientation === "vertical" ? "bc:h-50 bc:w-6" : "bc:h-6 bc:w-full"} ${isDisabled ? DISABLED : ""}`,
            style: ({ defaultStyle, isDisabled }) => ({
              ...defaultStyle,
              background: isDisabled ? void 0 : `${String(defaultStyle.background)}, ${CHECKERBOARD}`
            }),
            children: /* @__PURE__ */ jsx3(ColorThumb2, {})
          }
        )
      ]
    }
  );
}
function Wheel({ onChangeEnd, ...props }) {
  return /* @__PURE__ */ jsxs2(ColorWheel, { ...props, onChangeEnd: useEnd(onChangeEnd), outerRadius: 100, innerRadius: 74, children: [
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
    /* @__PURE__ */ jsx3(ColorThumb2, {})
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

// srcts/overlays/dialog.tsx
import { jsx as jsx4 } from "react/jsx-runtime";
function Dialog2({ className, ...props }) {
  return /* @__PURE__ */ jsx4(Dialog, { ...props, className: `${DIALOG} ${className ?? ""}` });
}
var DIALOG = "bc:relative bc:box-border bc:max-h-[inherit] bc:p-4 bc:outline-none";

// srcts/overlays/menu.tsx
import { Children } from "react";

// srcts/overlays/popover.tsx
import { jsx as jsx5 } from "react/jsx-runtime";
function Popover2({ className, offset = 4, ...props }) {
  return /* @__PURE__ */ jsx5(Popover, { ...props, offset, className: composeRenderProps(className, (given) => `${POPOVER} ${given ?? ""}`) });
}
var POPOVER = "bc:overflow-auto bc:rounded-md bc:border bc:border-border-default bc:bg-bg-raised bc:font-sans bc:text-sm bc:text-gray-900 bc:shadow-lg bc:outline-none bc:dark:text-gray-100";

// srcts/overlays/menu.tsx
import { Fragment as Fragment3, jsx as jsx6, jsxs as jsxs3 } from "react/jsx-runtime";

// srcts/colors/swatches.tsx
import { useContext as useContext3 } from "react";
import { Fragment as Fragment4, jsx as jsx7, jsxs as jsxs4 } from "react/jsx-runtime";
function ColorSwatch2({ className, style, ...props }) {
  return /* @__PURE__ */ jsx7(
    ColorSwatch,
    {
      ...props,
      className: composeRenderProps(className, (given) => `${SWATCH} ${given ?? SQUARE}`),
      style: composeRenderProps(style, (given, { color }) => ({
        ...given,
        background: `linear-gradient(${color.toString()}, ${color.toString()}), ${CHECKERBOARD2}`
      }))
    }
  );
}
function ColorSwatchPicker2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx7(Drawn2, { ...props }) : /* @__PURE__ */ jsx7(BoundSwatches, { ...props, bound });
}
function BoundSwatches({ bound, onChange, ...props }) {
  return /* @__PURE__ */ jsxs4(Fragment4, { children: [
    /* @__PURE__ */ jsx7(
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
    /* @__PURE__ */ jsx7(Refusal, { error: bound.error })
  ] });
}
function Drawn2({ className, onChange, ...props }) {
  const ended = useContext3(Ended);
  return /* @__PURE__ */ jsx7(
    ColorSwatchPicker,
    {
      ...props,
      onChange: (color) => {
        onChange?.(color);
        ended?.(color);
      },
      className: composeRenderProps(className, (given, { layout }) => `${PICKER} ${layout === "stack" ? "bc:flex-col" : "bc:flex-wrap"} ${given ?? ""}`)
    }
  );
}
function ColorSwatchPickerItem2({ className, ...props }) {
  return /* @__PURE__ */ jsx7(ColorSwatchPickerItem, { ...props, className: composeRenderProps(className, (given) => `${ITEM} ${given ?? ""}`), children: ({ isSelected }) => /* @__PURE__ */ jsxs4(Fragment4, { children: [
    /* @__PURE__ */ jsx7(ColorSwatch2, {}),
    isSelected ? /* @__PURE__ */ jsx7("div", { className: SELECTED }) : null
  ] }) });
}
var SWATCH = "bc:box-border bc:shrink-0 bc:border bc:border-border-default";
var SQUARE = "bc:size-8 bc:rounded-md";
var CHECKERBOARD2 = "repeating-conic-gradient(#ccc 0% 25%, white 0% 50%) 50% / 16px 16px";
var PICKER = "bc:flex bc:gap-1";
var ITEM = "bc:relative bc:cursor-pointer bc:rounded-xs bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
var SELECTED = "bc:absolute bc:top-0 bc:left-0 bc:box-border bc:size-full bc:rounded-md bc:border-2 bc:border-black bc:outline-2 bc:-outline-offset-4 bc:outline-white bc:dark:border-white bc:dark:outline-black bc:forced-color-adjust-none";

// srcts/colors/picker.tsx
import { Fragment as Fragment5, jsx as jsx8, jsxs as jsxs5 } from "react/jsx-runtime";
function ColorPicker2({ bound, ...props }) {
  return bound === void 0 ? /* @__PURE__ */ jsx8(Drawn3, { ...props }) : /* @__PURE__ */ jsx8(BoundPicker, { ...props, bound });
}
function BoundPicker({ bound, onChange, ...props }) {
  const dragged = useDragged(bound);
  return /* @__PURE__ */ jsxs5(Fragment5, { children: [
    /* @__PURE__ */ jsx8(Ended, { value: dragged.end, children: /* @__PURE__ */ jsx8(
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
    /* @__PURE__ */ jsx8(Refusal, { error: bound.error })
  ] });
}
function Drawn3({ label, children, isDisabled, ...props }) {
  return /* @__PURE__ */ jsx8(ColorPicker, { ...props, children: /* @__PURE__ */ jsxs5(DialogTrigger, { children: [
    /* @__PURE__ */ jsxs5(Button, { isDisabled, className: BUTTON, children: [
      /* @__PURE__ */ jsx8(ColorSwatch2, {}),
      label === void 0 ? null : /* @__PURE__ */ jsx8("span", { children: label })
    ] }),
    /* @__PURE__ */ jsx8(Popover2, { placement: "bottom start", children: /* @__PURE__ */ jsx8(Dialog2, { className: "bc:flex bc:flex-col bc:gap-2", children: children ?? /* @__PURE__ */ jsxs5(Fragment5, { children: [
      /* @__PURE__ */ jsx8(ColorArea2, { colorSpace: "hsb", xChannel: "saturation", yChannel: "brightness" }),
      /* @__PURE__ */ jsx8(ColorSlider2, { colorSpace: "hsb", channel: "hue" }),
      /* @__PURE__ */ jsx8(ColorField2, { label: "Hex" })
    ] }) }) })
  ] }) });
}
var BUTTON = "bc:flex bc:cursor-pointer bc:items-center bc:gap-2 bc:rounded-xs bc:border-0 bc:bg-transparent bc:p-0 bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-focus-visible:outline-2 bc:data-focus-visible:outline-offset-2 bc:data-focus-visible:outline-border-accent bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
export {
  ColorArea2 as ColorArea,
  ColorField2 as ColorField,
  ColorPicker2 as ColorPicker,
  ColorSlider2 as ColorSlider,
  ColorSwatch2 as ColorSwatch,
  ColorSwatchPicker2 as ColorSwatchPicker,
  ColorSwatchPickerItem2 as ColorSwatchPickerItem,
  ColorThumb2 as ColorThumb,
  ColorWheel2 as ColorWheel
};
