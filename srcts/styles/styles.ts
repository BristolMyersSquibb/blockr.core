// The classes of what core's UIs draw beside blockr.ui's components, with
// React or without: the layout of the controls that controls() draws, a
// group of checkboxes with its legend, a label, the hint and the alert below a
// control, and the looks of core's buttons that are not blockr.ui's.
// They are Tailwind CSS utilities under core's prefix, which core's stylesheet
// holds, and they draw with the theme variables alone, as blockr.ui's own
// classes do (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.2).
// Each takes the look of blockr.ui's class of the same name, so that what core
// draws beside a component looks as the component does.
// The components bring their own classes, under blockr.ui's prefix, which no
// other package writes
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4#prefixes).
// A browser passes its colour on to no button, so a button takes the text's
// colour of what holds it, through `bc:text-inherit`, while a frontend gives
// its page, its cards and its overlays the colour of their text.

export const styles = {
  stack: "bc:grid bc:gap-3 bc:font-sans",
  label: "bc:text-sm bc:font-medium",
  fieldset: "bc:m-0 bc:grid bc:min-w-0 bc:gap-1 bc:border-0 bc:p-0 bc:font-sans",
  legend: "bc:text-sm bc:font-medium bc:mb-1 bc:p-0",
  hint: "bc:m-0 bc:text-sm bc:text-muted-foreground",
  alert: "bc:m-0 bc:text-sm bc:text-destructive-text bc:whitespace-pre-wrap",
} as const;

// The looks of a button: "primary", in the brand colour, "secondary", with a
// border, and "quiet", with no border, as blockr.ui's Button takes them by its
// `variant`, for core's buttons that are not blockr.ui's.
// A browser passes its font and colour on to no button, so each look sets
// them itself.
// A hovered button that draws no fill of its own takes the fill of a hover,
// `accent`, and its text.
export const buttons = {
  primary:
    "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-primary bc:bg-primary bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-primary-foreground bc:cursor-pointer bc:disabled:cursor-default bc:disabled:opacity-50",
  secondary:
    "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border bc:border-border bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-inherit bc:cursor-pointer bc:hover:bg-accent bc:hover:text-accent-foreground bc:disabled:cursor-default bc:disabled:opacity-50",
  quiet:
    "bc:inline-flex bc:items-center bc:gap-1 bc:rounded-md bc:border-0 bc:bg-transparent bc:px-1.5 bc:py-0.5 bc:font-sans bc:text-xs bc:text-muted-foreground bc:cursor-pointer bc:hover:bg-accent bc:hover:text-accent-foreground",
} as const;
