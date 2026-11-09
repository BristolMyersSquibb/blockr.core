// srcts/modules/basic-plot/index.tsx
import { useEffect, useMemo } from "react";
import { outputRenderer } from "@blockr/core/react";
import { jsx } from "react/jsx-runtime";
var api = 1;
var basic_plot_default = outputRenderer(Plot);
function Plot({ output, body }) {
  const url = useMemo(() => body === null ? null : URL.createObjectURL(body), [body]);
  useEffect(
    () => () => {
      if (url !== null) URL.revokeObjectURL(url);
    },
    [url]
  );
  return url === null ? null : /* @__PURE__ */ jsx("img", { src: url, alt: "Plot", width: output.width, height: output.height, className: "bc:block bc:max-w-none" });
}
export {
  api,
  basic_plot_default as default
};
