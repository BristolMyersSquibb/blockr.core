// The overlays of core's React SDK, React Aria's in core's theme, which
// core's own UIs, frontends and the UIs of plugins and blocks draw with:
// the modal and the dialog, React Aria's DialogTrigger and Heading, which
// open a dialog from a button and title it, the menu, and the popover.
// React Aria draws each in a portal at the end of the page's body.

export { Dialog, Modal } from "./dialog.tsx";
export { Menu, MenuItem, MenuSection, MenuTrigger } from "./menu.tsx";
export type { MenuSectionProps, MenuTriggerProps } from "./menu.tsx";
export { Popover } from "./popover.tsx";
export { DialogTrigger, Heading } from "../aria/index.ts";
