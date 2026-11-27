// The icons of Lucide that core's UIs draw, from @blockr/ui/icons
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.3#the-icons),
// the one place in core's code that imports them.
// The build bundles what this file exports, minified, into core's vendor
// file, vendor-<hash>.js, and leaves each import of this file an import of
// the vendor file (../build/build.ts).
// So core's own files hold none of the icons, as the vendor check asks, and
// the vendor file holds these icons alone, of the 2,100 or so that the entry
// offers.
//
// - Box, Link and Layers on the board actions that add a block, a link and a
//   stack, and Trash on those that remove (../actions/icons.ts).
// - SquareArrowUpRightExit as the open icon of core's menu of board actions.
// - CircleX, TriangleExclamationPoint and Info for an error, a warning and a
//   message, on the pills, the banners and the toasts.
// - Ellipsis on the button that opens the menu of a board object.
// - X on the buttons of Edit inputs that remove, and on the button that
//   closes a toast.
// - ChevronDown after the label of a storage control's menu.
// - ArrowUp, Folder and File in the file browser block.
// An icon that core starts to draw goes here.

export {
  ArrowUp,
  Box,
  ChevronDown,
  CircleX,
  Ellipsis,
  File,
  Folder,
  Info,
  Layers,
  Link,
  SquareArrowUpRightExit,
  Trash,
  TriangleExclamationPoint,
  X,
} from "@blockr/ui/icons";
export type { IconNode } from "@blockr/ui/icons";
