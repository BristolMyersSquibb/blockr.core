// The command palette of core's React SDK, after the CommandPalette of React
// Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/CommandPalette.tsx):
// a menu whose items React Aria's Autocomplete narrows to those that hold
// what the user types into a search field above it, in a dialog of the SDK's
// modal.
// It takes the props of the Autocomplete and of the menu, and opens while
// `isOpen`, which its caller holds.
// It keeps no listener on the page: its caller opens it, as on a shortcut of
// its own, and the modal closes it on Escape, once the search field holds no
// text, and on a press outside it.
// It closes as the user runs a command, as a menu that opens from a button
// does, unless shouldCloseOnSelect is false.
// React Aria draws it in a portal at the end of the page's body, as every
// overlay of the SDK.

import type { ReactElement } from "react";
import { Autocomplete, useFilter } from "../aria/index.ts";
import type { AutocompleteProps, MenuProps } from "../aria/index.ts";
import { SearchField } from "../inputs/index.ts";
import { Dialog, Modal } from "./dialog.tsx";
import { Menu } from "./menu.tsx";

export interface CommandPaletteProps<T> extends Omit<AutocompleteProps, "children">, MenuProps<T> {
  readonly isOpen: boolean;
  readonly onOpenChange: (isOpen: boolean) => void;
  // What the search field says while it is empty, and names it.
  readonly placeholder?: string;
}

export function CommandPalette<T extends object>({
  isOpen,
  onOpenChange,
  placeholder = "Search commands",
  inputValue,
  defaultInputValue,
  onInputChange,
  filter,
  disableAutoFocusFirst,
  disableVirtualFocus,
  onAction,
  shouldCloseOnSelect = true,
  ...menu
}: CommandPaletteProps<T>): ReactElement {
  const { contains } = useFilter({ sensitivity: "base" });
  return (
    <Modal isDismissable isOpen={isOpen} onOpenChange={onOpenChange}>
      <Dialog aria-label={placeholder} className="bc:flex bc:max-h-[inherit] bc:flex-col bc:gap-2 bc:p-2">
        <Autocomplete
          filter={filter ?? contains}
          inputValue={inputValue}
          defaultInputValue={defaultInputValue}
          onInputChange={onInputChange}
          disableAutoFocusFirst={disableAutoFocusFirst}
          disableVirtualFocus={disableVirtualFocus}
        >
          <SearchField autoFocus aria-label={placeholder} placeholder={placeholder} />
          <Menu
            renderEmptyState={() => <p className={NOTE}>No results found.</p>}
            {...menu}
            onAction={(key, value) => {
              onAction?.(key, value);
              if (shouldCloseOnSelect) onOpenChange(false);
            }}
            className="bc:min-h-0 bc:flex-1"
          />
        </Autocomplete>
      </Dialog>
    </Modal>
  );
}

const NOTE = "bc:m-0 bc:px-2.5 bc:py-2 bc:text-center bc:text-sm bc:italic bc:text-text-muted";
