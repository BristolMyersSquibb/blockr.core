// The meter and the progress bar of core's React SDK: React Aria's Meter
// (https://react-aria.adobe.com/Meter) and ProgressBar
// (https://react-aria.adobe.com/ProgressBar), in core's theme, after the
// Meter and the ProgressBar of React Aria's Tailwind starter kit
// (https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/Meter.tsx,
// https://github.com/adobe/react-spectrum/blob/react-aria-components%401.22.1/starters/tailwind/src/ProgressBar.tsx).
// Each takes the props of its part of React Aria, such as `value` between
// `minValue` and `maxValue`, 0 and 100 by default, and the starter kit's
// `label`.
// Each shows its label and its value, as text, over a bar that fills as far
// as the value goes.
// The meter shows how much of something is taken, such as storage: its bar is
// green below 70%, orange from 70%, and red from 80%, where its text turns
// red as well and shows a warning sign.
// The progress bar shows how far a task has come, in the accent colour.
// Where it is `isIndeterminate`, as where the task cannot tell, it shows no
// value, and a part of the bar slides across it, but not while the user
// prefers reduced motion.

import type { ReactElement } from "react";
import { composeRenderProps, Label, Meter as AriaMeter, ProgressBar as AriaProgressBar } from "../aria/index.ts";
import type { MeterProps as AriaMeterProps, ProgressBarProps as AriaProgressBarProps } from "../aria/index.ts";
import { styles } from "../styles/index.ts";

export interface MeterProps extends AriaMeterProps {
  readonly label?: string;
}

export function Meter({ label, className, ...props }: MeterProps): ReactElement {
  return (
    <AriaMeter {...props} className={composeRenderProps(className, (given) => `${METER} ${given ?? ""}`)}>
      {({ percentage, valueText }) => (
        <>
          <div className="bc:flex bc:justify-between bc:gap-2">
            <Label className={styles.label}>{label}</Label>
            <span className={`bc:text-sm ${percentage >= 80 ? "bc:text-text-danger" : "bc:text-text-muted"}`}>
              {percentage >= 80 ? <Alert /> : null}
              {` ${valueText ?? ""}`}
            </span>
          </div>
          <div className={`${TRACK} bc:w-64`}>
            <div className={`${FILL} bc:left-0 ${fill(percentage)}`} style={{ width: `${String(percentage)}%` }} />
          </div>
        </>
      )}
    </AriaMeter>
  );
}

export interface ProgressBarProps extends AriaProgressBarProps {
  readonly label?: string;
}

export function ProgressBar({ label, className, ...props }: ProgressBarProps): ReactElement {
  return (
    <AriaProgressBar {...props} className={composeRenderProps(className, (given) => `${PROGRESS} ${given ?? ""}`)}>
      {({ percentage, valueText, isIndeterminate }) => (
        <>
          <div className="bc:flex bc:justify-between bc:gap-2">
            <Label className={styles.label}>{label}</Label>
            <span className="bc:text-sm bc:text-text-muted">{valueText}</span>
          </div>
          <div className={`${TRACK} bc:overflow-hidden`}>
            <div
              className={`${FILL} bc:left-0 bc:bg-bg-accent ${isIndeterminate ? "bc:motion-safe:animate-[bc-progress-slide_1s_ease-out_infinite]" : ""}`}
              style={{ width: `${String(isIndeterminate ? 40 : (percentage ?? 0))}%` }}
            />
          </div>
        </>
      )}
    </AriaProgressBar>
  );
}

// The colour of the meter's bar at the share given, in percent.
function fill(percentage: number): string {
  if (percentage < 70) return "bc:bg-green-600";
  if (percentage < 80) return "bc:bg-orange-500";
  return "bc:bg-red-600";
}

// The warning sign of a meter from 80%.
function Alert(): ReactElement {
  return (
    <svg
      role="img"
      aria-label="Alert"
      viewBox="0 0 16 16"
      className="bc:inline-block bc:size-4 bc:align-text-bottom"
      fill="none"
      stroke="currentColor"
      strokeWidth={1.4}
      strokeLinecap="round"
      strokeLinejoin="round"
    >
      <path d="M8 1.8 14.6 13.6H1.4Z" />
      <path d="M8 6.2v3.2M8 11.6v.1" />
    </svg>
  );
}

const METER = "bc:flex bc:max-w-full bc:flex-col bc:gap-2 bc:font-sans bc:text-text-default";
const PROGRESS = "bc:flex bc:w-64 bc:max-w-full bc:flex-col bc:gap-2 bc:font-sans bc:text-text-default";
// The track keeps a transparent outline, which forced colours draw.
const TRACK =
  "bc:relative bc:h-2 bc:max-w-full bc:rounded-full bc:bg-border-default bc:outline bc:outline-1 bc:-outline-offset-1 bc:outline-transparent";
const FILL = "bc:absolute bc:top-0 bc:h-full bc:rounded-full bc:forced-colors:bg-[Highlight]";
