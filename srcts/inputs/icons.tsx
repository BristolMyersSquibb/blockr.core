// The icons of the SDK's inputs and overlays, the design system's, each drawn
// in the colour of the text around it and hidden from assistive technology,
// since the element that holds one names what it does.

import type { ReactElement } from "react";

// The one chevron of the design system, which turns up while the list of a
// select is open, as the select marks itself open.
export function Chevron(): ReactElement {
  return (
    <svg
      width="12"
      height="12"
      viewBox="0 0 12 12"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.4"
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
      className="bc:shrink-0 bc:text-text-muted bc:transition-transform bc:group-data-open:rotate-180"
    >
      <polyline points="3 4.5 6 7.5 9 4.5" />
    </svg>
  );
}

// The tick of a pick, of the size given.
export function Tick({ size = 14 }: { size?: number }): ReactElement {
  return (
    <svg
      width={size}
      height={size}
      viewBox="0 0 16 16"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.5"
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
    >
      <path d="M3.5 8.5l3 3 6-7" />
    </svg>
  );
}

// The dash of a checkbox that is neither checked nor clear.
export function Dash(): ReactElement {
  return (
    <svg width="12" height="12" viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" aria-hidden="true">
      <path d="M4 8h8" />
    </svg>
  );
}

// The cross of a button that removes or clears.
export function Cross(): ReactElement {
  return (
    <svg
      width="10"
      height="10"
      viewBox="0 0 10 10"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.4"
      strokeLinecap="round"
      aria-hidden="true"
    >
      <path d="M2.5 2.5l5 5M7.5 2.5l-5 5" />
    </svg>
  );
}

// The magnifier of a search field.
export function Magnifier(): ReactElement {
  return (
    <svg
      width="14"
      height="14"
      viewBox="0 0 16 16"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.5"
      strokeLinecap="round"
      aria-hidden="true"
      className="bc:shrink-0 bc:text-text-muted"
    >
      <circle cx="7" cy="7" r="4.5" />
      <path d="M10.5 10.5L14 14" />
    </svg>
  );
}
