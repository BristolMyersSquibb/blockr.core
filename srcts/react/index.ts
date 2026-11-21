// The React SDK: what core offers modules written in React, which core's own
// UIs draw with and which core ships to packages as `@blockr/core/react`.
// It holds the hooks through which a component follows its host or CoreApi,
// the helpers that make a module or a renderer from a component, the inputs,
// each on one value that useValue() binds or for its caller, among them the
// dates and times, the selects,
// which every UI draws with, bound or not, the parts that take no binding,
// such as the tags, the list box, the form and the drop zone, the buttons,
// and the overlays: the modal and the dialog, the menu, the popover and the
// command palette.
// It holds the table, the trees and the grid list, which bind no value, and
// what drags the items of the last three and drops onto them.
// The inputs, the buttons, the overlays, the table, the trees and the grid
// list are React Aria's, in core's theme, and React Aria's FileTrigger and
// Autocomplete come unstyled.
// It also holds the list in which a UI shows notifications, as useNotified()
// gives them, in the message style of blockr.ui's design system.

export { useBoard, useNotified, useOutput, useRequest, useStatus, useValue } from "./hooks.ts";
export type { Answered, Bound, OutputSnapshot, RequestOptions } from "./hooks.ts";
export { outputRenderer, reactModule } from "./module.tsx";
export type { ModuleProps, OutputProps } from "./module.tsx";
export {
  Autocomplete,
  Button,
  Calendar,
  Checkbox,
  CheckboxGroup,
  ComboBox,
  ComboBoxItem,
  ComboBoxSection,
  DateField,
  DatePicker,
  DateRangePicker,
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
  RangeCalendar,
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
  TimeField,
  ToggleButton,
  ToggleButtonGroup,
  Token,
  TokenField,
  useFilter,
} from "../inputs/index.ts";
export type {
  ButtonProps,
  CalendarProps,
  CheckboxGroupProps,
  CheckboxProps,
  ComboBoxProps,
  ComboBoxSectionProps,
  DateFieldProps,
  DatePickerProps,
  DateRangePickerProps,
  NumberFieldProps,
  RadioGroupProps,
  RadioProps,
  RangeCalendarProps,
  SearchFieldProps,
  SelectProps,
  SelectSectionProps,
  SliderProps,
  SwitchProps,
  TagGroupProps,
  TagProps,
  TextFieldProps,
  TimeFieldProps,
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
export { DropIndicator, useDragAndDrop } from "../dnd/index.ts";
export type { DragAndDropHooks, DragAndDropOptions, DropIndicatorProps } from "../dnd/index.ts";
export { GridList, GridListHeader, GridListItem, GridListSection } from "../gridlist/index.ts";
export { Cell, Column, Row, Table, TableBody, TableHeader } from "../table/index.ts";
export type { ColumnProps, TableProps } from "../table/index.ts";
export {
  NavigationTree,
  NavigationTreeHeader,
  NavigationTreeItem,
  NavigationTreeItemContent,
  NavigationTreeItemLink,
  NavigationTreeSection,
  Tree,
  TreeItem,
} from "../tree/index.ts";
export type { NavigationTreeItemLinkProps, NavigationTreeProps, TreeItemProps } from "../tree/index.ts";
