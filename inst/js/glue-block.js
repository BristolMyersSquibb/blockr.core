// srcts/modules/glue-block/index.tsx
import { reactModule, Stack, TextInput, useValue } from "@blockr/core/react";
import { jsx } from "react/jsx-runtime";
var api = 1;
function GlueBlock({ host }) {
  return /* @__PURE__ */ jsx(Stack, { children: /* @__PURE__ */ jsx(TextInput, { bound: useValue(host, "text"), label: "Text", placeholder: "{nrow(data)} rows", lines: 4 }) });
}
var glue_block_default = reactModule(GlueBlock);
export {
  api,
  glue_block_default as default
};
