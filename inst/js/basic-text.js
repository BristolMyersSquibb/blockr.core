// srcts/modules/basic-text/index.tsx
import { outputRenderer } from "@blockr/core/react";
import { jsx } from "react/jsx-runtime";
var api = 1;
var basic_text_default = outputRenderer(({ output }) => /* @__PURE__ */ jsx(
  "pre",
  {
    tabIndex: 0,
    className: "bc:m-0 bc:min-h-0 bc:overflow-auto bc:rounded bc:bg-bg-subtle bc:p-2 bc:font-mono bc:text-xs bc:leading-snug bc:whitespace-pre",
    children: output.text
  }
));
export {
  api,
  basic_text_default as default
};
