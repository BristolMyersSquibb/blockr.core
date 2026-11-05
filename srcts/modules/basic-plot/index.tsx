// Core's plain renderer of plot outputs, blockr.core-plot_output, which draws
// every plot output unless an app names another renderer for the class in the
// blockr option `renderers`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#renderers).
// It shows the PNG that the output's body holds at the size it was drawn at,
// in CSS pixels, which R took from the output's info, so that a PNG drawn for
// a display of twice the density shows sharp on it.

import { useEffect, useMemo } from "react";
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
      root.render(<PlotOutput host={host} />);
    });
    return () => {
      root.unmount();
    };
  },
};

export default renderer;

function PlotOutput({ host }: { host: OutputHost }): ReactElement {
  const { output, body, loading, error } = useOutput(host);
  const { width, height } = isObject(output) ? output : {};

  // The body shows through a URL of its own, which goes once another body or
  // the unmount replaces it.
  const url = useMemo(() => (body === null ? null : URL.createObjectURL(body)), [body]);
  useEffect(
    () => () => {
      if (url !== null) URL.revokeObjectURL(url);
    },
    [url],
  );

  return (
    <div className="bc:flex bc:flex-col bc:gap-2" aria-busy={loading}>
      {error !== null && (
        <p role="alert" className="bc:m-0 bc:text-sm bc:text-red-600 bc:whitespace-pre-wrap">
          {error.message}
        </p>
      )}
      {url !== null && (
        <img
          src={url}
          alt="Plot"
          width={typeof width === "number" ? width : undefined}
          height={typeof height === "number" ? height : undefined}
          className={`bc:block bc:max-w-none ${loading ? "bc:opacity-60" : ""}`}
        />
      )}
    </div>
  );
}
