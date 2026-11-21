// The React SDK: what core offers modules written in React, which core's own
// UIs draw with and which core ships to packages as `@blockr/core/react`.
// It holds the hooks through which a component follows its host or CoreApi,
// the helpers that make a module or a renderer from a component, the inputs,
// each on one value that useValue() binds or for its caller, the selects,
// which every UI draws with, bound or not, the parts that take no binding,
// such as the tags, the list box, the form and the drop zone, the buttons,
// and the overlays: the modal and the dialog, the menu, the popover and the
// command palette.
// The inputs, the buttons and the overlays are React Aria's, in core's theme,
// and React Aria's FileTrigger and Autocomplete come unstyled.
// It also holds the list in which a UI shows notifications, as useNotified()
// gives them, in the message style of blockr.ui's design system.
// It holds the line below an input that names the values that it lacks, and
// the line that says why the host refused what it submitted, which its inputs
// draw, and which its dates draw from here as well, from a file of their own
// (../dates/index.ts).

export { useBoard, useNotified, useOutput, useRequest, useStatus, useValue } from "./hooks.ts";
export type { Answered, Bound, OutputSnapshot, RequestOptions } from "./hooks.ts";
export { outputRenderer, reactModule } from "./module.tsx";
export type { ModuleProps, OutputProps } from "./module.tsx";
export {
  Autocomplete,
  Button,
  Checkbox,
  CheckboxGroup,
  ComboBox,
  ComboBoxItem,
  ComboBoxSection,
  DropZone,
  Field,
  FieldButton,
  FileTrigger,
  Form,
  ListBox,
  ListBoxItem,
  NotAvailable,
  NumberField,
  Radio,
  RadioGroup,
  Refusal,
  SearchField,
  Select,
  SelectItem,
  SelectSection,
  Slider,
  Stack,
  Switch,
  Tag,
  TagGroup,
  Text,
  TextField,
  ToggleButton,
  ToggleButtonGroup,
  Token,
  TokenField,
  useFilter,
} from "../inputs/index.ts";
export type {
  ButtonProps,
  CheckboxGroupProps,
  CheckboxProps,
  ComboBoxProps,
  ComboBoxSectionProps,
  NumberFieldProps,
  RadioGroupProps,
  RadioProps,
  SearchFieldProps,
  SelectProps,
  SelectSectionProps,
  SliderProps,
  SwitchProps,
  TagGroupProps,
  TagProps,
  TextFieldProps,
  ToggleButtonGroupProps,
  ToggleButtonProps,
  TokenFieldProps,
} from "../inputs/index.ts";
export {
  CommandPalette,
  Dialog,
  DialogTrigger,
  Heading,
  Keyboard,
  Menu,
  MenuItem,
  MenuSection,
  MenuTrigger,
  Modal,
  Popover,
} from "../overlays/index.ts";
export type { CommandPaletteProps, MenuSectionProps, MenuTriggerProps } from "../overlays/index.ts";
export { NotificationList } from "./notifications.tsx";
export type { NotificationListProps } from "./notifications.tsx";
