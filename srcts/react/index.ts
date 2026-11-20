// The React SDK: what core offers modules written in React, which core's own
// UIs draw with and which core ships to packages as `@blockr/core/react`.
// It holds the hooks through which a component follows its host or CoreApi,
// the helpers that make a module or a renderer from a component, the inputs,
// each on one value that useValue() binds or for its caller, the one select,
// which every UI draws with, bound or not, the button, the overlays: the
// modal and the dialog, the dialog that asks, the sheet, the menu and its
// submenus, the popover, the tooltip and the toasts, and the layout: the
// disclosure, the tabs, the toolbar and its separator, the breadcrumbs, the
// link, the meter and the progress bar.
// The inputs, the button, the overlays and the layout are React Aria's, in
// core's theme.

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
export {
  AlertDialog,
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
export type { AlertDialogProps, MenuSectionProps, MenuTriggerProps, QueuedToast, SheetProps, TooltipProps } from "../overlays/index.ts";
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
