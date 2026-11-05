// The classes of the inputs that core's UIs draw,
// whether with React or with the DOM alone,
// and of what a module draws beside them.
// They are Tailwind CSS utilities under core's prefix,
// which core's stylesheet holds.
// An input takes its colour from the text around it,
// so that it suits a light and a dark theme alike,
// while the radii, the sizes and the accents come from Tailwind's theme.
// Core's stylesheet leaves out Tailwind's reset of the page,
// so an input fills its label by stretching in the label's grid,
// border included, rather than by its width,
// and an element that a class displays hides through a wrapper of its own
// rather than through its `hidden` attribute, which the class overrides.

export const styles = {
  stack: "bc:grid bc:gap-3",
  field: "bc:grid bc:gap-1",
  label: "bc:text-sm bc:font-medium",
  box: "bc:min-w-0 bc:justify-self-stretch bc:rounded-md bc:border bc:border-current/25 bc:bg-transparent bc:px-2 bc:py-1 bc:text-sm bc:text-inherit bc:[font-family:inherit] bc:focus:outline-2 bc:focus:outline-offset-1 bc:focus:outline-blue-500 bc:aria-invalid:border-red-500",
  check: "bc:flex bc:items-center bc:gap-2 bc:text-sm",
  checkbox: "bc:accent-blue-600",
  fieldset: "bc:m-0 bc:grid bc:min-w-0 bc:gap-1 bc:border-0 bc:p-0",
  legend: "bc:text-sm bc:font-medium bc:mb-1 bc:p-0",
  list: "bc:grid bc:max-h-48 bc:gap-1 bc:overflow-y-auto",
  hint: "bc:m-0 bc:text-sm bc:opacity-70",
  alert: "bc:m-0 bc:text-sm bc:text-red-600 bc:whitespace-pre-wrap",
} as const;
