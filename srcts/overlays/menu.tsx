// The menu of core's React SDK: React Aria's MenuTrigger, Menu, MenuItem,
// MenuSection and SubmenuTrigger (https://react-aria.adobe.com/Menu), in
// core's theme, after the Menu of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Menu.tsx).
// Each takes the props of its part of React Aria, and the trigger the
// starter kit's `placement` too.
// The trigger holds the button that opens the menu, the SDK's Button, and
// the menu, which it opens in the SDK's Popover beside the button.
// The submenu's trigger holds an item of a menu and the menu that opens from
// it, in the SDK's Popover beside the item's row, as the pointer rests on the
// item, as the user presses it, or on the arrow key that points away from the
// menu. The item shows a chevron at its right.
// React Aria closes the menu as the user picks an item, on Escape and on a
// press outside it, and moves the focus through its items by the arrow keys.
// An item shows a tick where the menu picks, and the item is picked.
// An item holds its text, or its label in a Text of the slot "label", with
// its description, in a Text of the slot "description", in a muted line below
// it, and its shortcut, in React Aria's Keyboard, at its right.
// React Aria names both the description and the shortcut to assistive
// technology, in the item's aria-describedby.
// A section groups items under its title.

import { Children } from "react";
import type { ReactElement, ReactNode } from "react";
import {
  Collection,
  composeRenderProps,
  Header,
  Menu as AriaMenu,
  MenuItem as AriaMenuItem,
  MenuSection as AriaMenuSection,
  MenuTrigger as AriaMenuTrigger,
  SubmenuTrigger as AriaSubmenuTrigger,
} from "../aria/index.ts";
import type {
  MenuItemProps,
  MenuProps,
  MenuSectionProps as AriaMenuSectionProps,
  MenuTriggerProps as AriaMenuTriggerProps,
  PopoverProps,
  SubmenuTriggerProps,
} from "../aria/index.ts";
import { ChevronRight, Tick } from "../inputs/index.ts";
import { Popover } from "./popover.tsx";

export interface MenuTriggerProps extends AriaMenuTriggerProps {
  readonly placement?: PopoverProps["placement"];
}

export function MenuTrigger({ placement, children, ...props }: MenuTriggerProps): ReactElement {
  const [trigger, menu] = Children.toArray(children);
  return (
    <AriaMenuTrigger {...props}>
      {trigger}
      <Popover placement={placement} className="bc:min-w-44">
        {menu}
      </Popover>
    </AriaMenuTrigger>
  );
}

export function SubmenuTrigger({ children, ...props }: SubmenuTriggerProps): ReactElement {
  // React Aria's SubmenuTrigger takes its item and its menu as elements.
  const [trigger, menu] = Children.toArray(children) as [ReactElement, ReactElement];
  return (
    <AriaSubmenuTrigger {...props}>
      {trigger}
      <Popover offset={-2} crossOffset={-4} className="bc:min-w-44">
        {menu}
      </Popover>
    </AriaSubmenuTrigger>
  );
}

export function Menu<T extends object>({ className, ...props }: MenuProps<T>): ReactElement {
  return <AriaMenu {...props} className={composeRenderProps(className, (given) => `${MENU} ${given ?? ""}`)} />;
}

export function MenuItem(props: MenuItemProps): ReactElement {
  const textValue = props.textValue ?? (typeof props.children === "string" ? props.children : undefined);
  return (
    <AriaMenuItem {...props} textValue={textValue} className={composeRenderProps(props.className, (given) => `${ITEM} ${given ?? ""}`)}>
      {composeRenderProps(props.children, (children, { selectionMode, isSelected, hasSubmenu }) => (
        <>
          {selectionMode === "none" ? null : (
            <span className="bc:flex bc:size-3.5 bc:shrink-0 bc:text-bg-accent">{isSelected ? <Tick /> : null}</span>
          )}
          <span className={CONTENT}>{children}</span>
          {hasSubmenu ? <ChevronRight size={14} /> : null}
        </>
      ))}
    </AriaMenuItem>
  );
}

export interface MenuSectionProps<T> extends AriaMenuSectionProps<T> {
  readonly title?: string;
  readonly items?: Iterable<T>;
  readonly children?: ReactNode | ((item: T) => ReactElement);
}

export function MenuSection<T extends object>({ title, items, children, ...props }: MenuSectionProps<T>): ReactElement {
  return (
    <AriaMenuSection {...props}>
      {title === undefined ? null : <Header className={HEADING}>{title}</Header>}
      <Collection items={items}>{children}</Collection>
    </AriaMenuSection>
  );
}

const MENU = "bc:grid bc:max-h-[inherit] bc:gap-0.5 bc:overflow-auto bc:p-1 bc:outline-none";
const ITEM =
  "bc:box-border bc:flex bc:cursor-pointer bc:items-center bc:gap-2 bc:rounded bc:px-2 bc:py-1 bc:text-left bc:font-sans bc:text-sm bc:text-text-default bc:outline-none bc:data-hovered:bg-bg-subtle bc:data-focused:bg-bg-subtle bc:data-disabled:cursor-default bc:data-disabled:opacity-50";
const HEADING = "bc:block bc:px-2 bc:pt-1 bc:font-sans bc:text-xs bc:font-semibold bc:text-text-muted";
// The item's text or label, its description in a second row, and its
// shortcut in a column of its own at its right, each cut to one line.
const CONTENT =
  "bc:grid bc:min-w-0 bc:flex-1 bc:grid-cols-[minmax(0,1fr)_auto] bc:items-center bc:gap-x-4 bc:[&>*]:truncate bc:[&>[slot=description]]:col-start-1 bc:[&>[slot=description]]:text-xs bc:[&>[slot=description]]:text-text-muted bc:[&>kbd]:col-start-2 bc:[&>kbd]:row-start-1 bc:[&>kbd]:font-sans bc:[&>kbd]:text-xs bc:[&>kbd]:text-text-muted";
