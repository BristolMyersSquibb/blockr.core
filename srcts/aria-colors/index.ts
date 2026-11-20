// The parts of React Aria (https://react-aria.adobe.com/) that the colours of
// core's React SDK draw with, the one place that imports them, as ../aria is
// for the parts that core's own UIs draw with as the page starts.
// The build bundles what this file exports into a file of its own,
// vendor-colors-<hash>.js, which the page loads only once a module first
// imports the colours, `@blockr/core/react/colors` (../build/build.ts).
// The code of React Aria that the colours share with the parts of ../aria, as
// its contexts, sits in one more file, which the vendor file imports too.
// So the page still runs one copy of React Aria.
// Each part comes from the entry point of its component, as in ../aria.

export { ColorArea } from "react-aria-components/ColorArea";
export { ColorField } from "react-aria-components/ColorField";
export { ColorPicker } from "react-aria-components/ColorPicker";
export { ColorSlider, SliderOutput, SliderTrack } from "react-aria-components/ColorSlider";
export { ColorSwatch } from "react-aria-components/ColorSwatch";
export { ColorSwatchPicker, ColorSwatchPickerItem } from "react-aria-components/ColorSwatchPicker";
export { ColorThumb } from "react-aria-components/ColorThumb";
export { ColorWheel, ColorWheelTrack } from "react-aria-components/ColorWheel";
export { parseColor } from "react-aria-components/ColorPicker";
export type { ColorAreaProps } from "react-aria-components/ColorArea";
export type { ColorFieldProps } from "react-aria-components/ColorField";
export type { Color, ColorPickerProps } from "react-aria-components/ColorPicker";
export type { ColorSliderProps } from "react-aria-components/ColorSlider";
export type { ColorSwatchProps } from "react-aria-components/ColorSwatch";
export type { ColorSwatchPickerItemProps, ColorSwatchPickerProps } from "react-aria-components/ColorSwatchPicker";
export type { ColorThumbProps } from "react-aria-components/ColorThumb";
export type { ColorWheelProps } from "react-aria-components/ColorWheel";
