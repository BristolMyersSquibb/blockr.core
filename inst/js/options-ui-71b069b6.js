// srcts/options/ui.tsx
import { NumberField, ToggleButton, ToggleButtonGroup } from "@blockr/ui";
import { reactModule, useValue } from "@blockr/core/react";
import { jsx } from "react/jsx-runtime";
function numberUi(label, min) {
  function NumberUi({ host }) {
    return /* @__PURE__ */ jsx(NumberField, { bound: useValue(host), label, minValue: min, step: 1 });
  }
  return reactModule(NumberUi);
}
function toggleButtonGroupUi(label, items) {
  function ToggleButtonGroupUi({ host }) {
    return /* @__PURE__ */ jsx(ToggleButtonGroup, { bound: useValue(host), label, disallowEmptySelection: true, children: items.map((item) => /* @__PURE__ */ jsx(ToggleButton, { id: item.id, children: item.name }, item.id)) });
  }
  return reactModule(ToggleButtonGroupUi);
}
export {
  numberUi,
  toggleButtonGroupUi
};
