// The React SDK: what core offers modules written in React, which core's own
// UIs draw with and which core ships to packages as `@blockr/core/react`.
// It holds the hooks through which a component follows its host or CoreApi,
// the helpers that make a module or a renderer from a component, the inputs,
// each on one value that useValue() binds or for its caller, the one select,
// which every UI draws with, bound or not, the button, and the overlays: the
// modal and the dialog, the menu and the popover.
// The inputs, the button and the overlays are React Aria's, in core's theme.

export { useBoard, useNotified, useOutput, useRequest, useStatus, useValue } from "./hooks.ts";
export type { Answered, Bound, OutputSnapshot, RequestOptions } from "./hooks.ts";
export { outputRenderer, reactModule } from "./module.tsx";
export type { ModuleProps, OutputProps } from "./module.tsx";
export {
  Button,
  Checkbox,
  ComboBox,
  ComboBoxItem,
  ComboBoxSection,
  Field,
  NumberField,
  Stack,
  Text,
  TextField,
} from "../inputs/index.ts";
export type { ButtonProps, ComboBoxProps, ComboBoxSectionProps, NumberFieldProps, TextFieldProps } from "../inputs/index.ts";
export { Dialog, DialogTrigger, Heading, Keyboard, Menu, MenuItem, MenuSection, MenuTrigger, Modal, Popover } from "../overlays/index.ts";
export type { MenuSectionProps, MenuTriggerProps } from "../overlays/index.ts";
