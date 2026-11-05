// The classes of the inputs that core's UIs draw,
// whether with React or with the DOM alone,
// and of what a module draws beside them.
// They are Tailwind CSS utilities under core's prefix,
// which core's stylesheet holds.
// An input takes its colors from the variables of core's theme,
// such as --bc-color-border-default, whose defaults follow the text around it,
// so that it suits a light and a dark theme alike,
// while the radii and the sizes come from Tailwind's theme.
// Core's stylesheet leaves out Tailwind's reset of the page,
// so an input fills its label by stretching in the label's grid,
// border included, rather than by its width,
// and an element that a class displays hides through a wrapper of its own
// rather than through its `hidden` attribute, which the class overrides.

export const styles = {
  stack: "bc:grid bc:gap-3 bc:text-text-default",
  field: "bc:grid bc:gap-1 bc:text-text-default",
  label: "bc:text-sm bc:font-medium",
  box: "bc:min-w-0 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:text-sm bc:text-text-default bc:[font-family:inherit] bc:focus:outline-2 bc:focus:outline-offset-1 bc:focus:outline-border-accent bc:aria-invalid:border-border-danger",
  check: "bc:flex bc:items-center bc:gap-2 bc:text-sm bc:text-text-default",
  checkbox: "bc:accent-bg-accent",
  fieldset: "bc:m-0 bc:grid bc:min-w-0 bc:gap-1 bc:border-0 bc:p-0 bc:text-text-default",
  legend: "bc:text-sm bc:font-medium bc:mb-1 bc:p-0",
  list: "bc:grid bc:max-h-48 bc:gap-1 bc:overflow-y-auto",
  hint: "bc:m-0 bc:text-sm bc:text-text-muted",
  alert: "bc:m-0 bc:text-sm bc:text-text-danger bc:whitespace-pre-wrap",
} as const;
