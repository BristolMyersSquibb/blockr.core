// Core's plain renderer of text outputs, blockr.core-text_output, which draws
// every text output unless an app names another renderer for the class in the
// blockr option `renderers`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#renderers).
// It shows a result as print() shows it, at R's own width, in a box that
// scrolls rather than wraps its lines, and that is no taller than the element
// it draws in, where a front-end gives that element a height. It takes its
// colours from that element.

import type { ReactElement } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { useOutput } from "../../react/index.ts";
import type { OutputHost, OutputModule } from "../../types/hosts.ts";
import { isObject } from "../../util/index.ts";

export const api = 1;

const renderer: OutputModule = {
  render({ el, host }) {
    const root = createRoot(el);
    flushSync(() => {
      root.render(<TextOutput host={host} />);
    });
    return () => {
      root.unmount();
    };
  },
};

export default renderer;

function TextOutput({ host }: { host: OutputHost }): ReactElement {
  const { output, loading, error } = useOutput(host);
  const text = isObject(output) && typeof output.text === "string" ? output.text : "";
  return (
    <div className="bc:flex bc:max-h-full bc:flex-col bc:gap-2" aria-busy={loading}>
      {error !== null && (
        <p role="alert" className="bc:m-0 bc:text-sm bc:text-red-600 bc:whitespace-pre-wrap">
          {error.message}
        </p>
      )}
      {/* A box that scrolls takes the focus, so that the keyboard scrolls it
          too. */}
      <pre
        tabIndex={0}
        className={`bc:m-0 bc:min-h-0 bc:overflow-auto bc:rounded bc:bg-current/5 bc:p-2 bc:font-mono bc:text-xs bc:leading-snug bc:whitespace-pre ${loading ? "bc:opacity-60" : ""}`}
      >
        {text}
      </pre>
    </div>
  );
}
