// The parts of React Aria (https://react-aria.adobe.com/) that core's code
// draws with, the one place that imports them.
// The build bundles what this file exports into core's vendor file, with
// React, and leaves each import of this file an import of the vendor file
// (../build/build.ts).
// So the page runs one copy of React Aria, which holds the parts named here
// alone, since the whole of React Aria is several times the size of React.
// A part that core starts to draw with goes here.
//
// The portal provider comes from react-aria, the package of hooks under React
// Aria's components, whose copy must be the one that the components use, for
// them to read the container that it gives (./aria.test.ts).

export {
  Button,
  Collection,
  ComboBox,
  ComboBoxStateContext,
  composeRenderProps,
  FieldError,
  Group,
  Header,
  Input,
  Label,
  ListBox,
  ListBoxItem,
  ListBoxSection,
  Popover,
  Tag,
  TagGroup,
  TagList,
  Text,
  useFilter,
} from "react-aria-components";
export type {
  ComboBoxProps,
  Key,
  ListBoxItemProps,
  ListBoxSectionProps,
  ValidationResult,
} from "react-aria-components";
export { UNSAFE_PortalProvider } from "react-aria";
