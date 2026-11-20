// The colours of core's React SDK, which core ships to packages as
// `@blockr/core/react/colors`. They are React Aria's in core's theme, drawn
// after React Aria's Tailwind starter kit, as the rest of the SDK is
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#react):
// - A field and a picker: ColorField and ColorPicker.
// - Swatches: ColorSwatch and ColorSwatchPicker, with its items,
//   ColorSwatchPickerItem.
// - The parts of a picker: ColorArea, ColorSlider, ColorWheel and ColorThumb.
// Each input takes `bound` too, which binds a colour as a string of hex
// (./bound.tsx).
// They sit in a file of their own, apart from the rest of the SDK, and so do
// the parts of React Aria that they draw with (../aria-colors).
// So a page loads them only once a module first imports them, as the stacks
// of the Edit board plugin do as they show.

export { ColorField } from "./field.tsx";
export type { ColorFieldProps } from "./field.tsx";
export { ColorArea, ColorSlider, ColorThumb, ColorWheel } from "./parts.tsx";
export type { ColorAreaProps, ColorSliderProps, ColorWheelProps } from "./parts.tsx";
export { ColorPicker } from "./picker.tsx";
export type { ColorPickerProps } from "./picker.tsx";
export { ColorSwatch, ColorSwatchPicker, ColorSwatchPickerItem } from "./swatches.tsx";
export type { ColorSwatchPickerProps } from "./swatches.tsx";
export type { Color, ColorSwatchPickerItemProps, ColorSwatchProps, ColorThumbProps } from "../aria-colors/index.ts";
