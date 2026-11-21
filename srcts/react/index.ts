// The React SDK: what core offers modules written in React, which core's own
// UIs draw with and which core ships to packages as `@blockr/core/react`.
// It holds the hooks through which a component follows its host or CoreApi,
// the helpers that make a module or a renderer from a component, the inputs,
// each on one value that useValue() binds or for its caller, the selects,
// which every UI draws with, bound or not, the parts that take no binding,
// such as the tags, the list box, the form and the drop zone, the buttons,
// the overlays: the modal and the dialog, the dialog that asks, the sheet,
// the menu and its submenus, the popover, the command palette, the tooltip
// and the toasts, and the layout: the disclosure, the tabs, the toolbar and
// its separator, the breadcrumbs, the link, the meter and the progress bar.
// The inputs, the buttons, the overlays and the layout are React Aria's, in
// core's theme, and React Aria's FileTrigger and Autocomplete come unstyled.
// It also holds the list in which a UI shows notifications, as useNotified()
// gives them, in the message style of blockr.ui's design system.

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
  NumberField,
  Radio,
  RadioGroup,
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
  AlertDialog,
  CommandPalette,
  Dialog,
  DialogTrigger,
  Focusable,
  Heading,
  Keyboard,
  Menu,
  MenuItem,
  MenuSection,
  MenuTrigger,
  Modal,
  Popover,
  Sheet,
  SubmenuTrigger,
  Toast,
  ToastContent,
  ToastQueue,
  ToastRegion,
  Tooltip,
  TooltipTrigger,
} from "../overlays/index.ts";
export type {
  AlertDialogProps,
  CommandPaletteProps,
  MenuSectionProps,
  MenuTriggerProps,
  QueuedToast,
  SheetProps,
  TooltipProps,
} from "../overlays/index.ts";
export {
  Breadcrumb,
  Breadcrumbs,
  Disclosure,
  DisclosureGroup,
  DisclosureHeader,
  DisclosurePanel,
  Link,
  Meter,
  ProgressBar,
  Separator,
  Tab,
  TabList,
  TabPanel,
  Tabs,
  Toolbar,
} from "../layout/index.ts";
export type { BreadcrumbProps, DisclosureHeaderProps, LinkProps, MeterProps, ProgressBarProps } from "../layout/index.ts";
export { NotificationList } from "./notifications.tsx";
export type { NotificationListProps } from "./notifications.tsx";
