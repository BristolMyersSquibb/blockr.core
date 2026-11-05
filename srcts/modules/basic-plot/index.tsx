// Core's plain renderer of plot outputs, blockr.core-plot_output, which draws
// every plot output unless an app names another renderer for the class in the
// blockr option `renderers`
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#renderers).
// It shows the PNG that the output's body holds at the size it was drawn at,
// in CSS pixels, which R took from the output's info, so that a PNG drawn for
// a display of twice the density shows sharp on it.

import { useEffect, useMemo } from "react";
import type { ReactElement } from "react";
import { outputRenderer } from "../../react/index.ts";
import type { OutputProps } from "../../react/index.ts";
import type { PlotOutput } from "../../types/outputs.ts";

export const api = 1;

export default outputRenderer<PlotOutput>(Plot);

function Plot({ output, body }: OutputProps<PlotOutput>): ReactElement | null {
  // The body shows through a URL of its own, which goes once another body or
  // the unmount replaces it.
  const url = useMemo(() => (body === null ? null : URL.createObjectURL(body)), [body]);
  useEffect(
    () => () => {
      if (url !== null) URL.revokeObjectURL(url);
    },
    [url],
  );
  return url === null ? null : (
    <img src={url} alt="Plot" width={output.width} height={output.height} className="bc:block bc:max-w-none" />
  );
}
