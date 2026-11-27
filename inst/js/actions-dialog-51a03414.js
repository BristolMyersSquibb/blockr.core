// srcts/actions/dialog.tsx
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import { Button, Dialog, Heading, Modal } from "@blockr/ui";

// srcts/card/footer.tsx
import { useCallback, useId, useState } from "react";
import { Fragment, jsx, jsxs } from "react/jsx-runtime";

// srcts/card/mounted.tsx
import { useLayoutEffect, useRef } from "react";
import { jsx as jsx2 } from "react/jsx-runtime";
function Mounted({
  mount,
  label,
  className,
  data
}) {
  const ref = useRef(null);
  useLayoutEffect(() => {
    const el = ref.current;
    if (el === null) return;
    const cleanup = mount(el);
    return () => {
      queueMicrotask(cleanup);
    };
  }, [mount]);
  return /* @__PURE__ */ jsx2("div", { ref, role: label === void 0 ? void 0 : "region", "aria-label": label, className, ...data });
}

// srcts/card/status.ts
import { useState as useState2 } from "react";

// srcts/actions/dialog.tsx
import { jsx as jsx3, jsxs as jsxs2 } from "react/jsx-runtime";
function drawDialog(el, args, onClose) {
  const root = createRoot(el);
  flushSync(() => {
    root.render(/* @__PURE__ */ jsx3(CoreDialog, { args, onClose }));
  });
  return () => {
    root.unmount();
  };
}
function CoreDialog({ args, onClose }) {
  const { mount } = args;
  return /* @__PURE__ */ jsx3(
    Modal,
    {
      isOpen: true,
      onOpenChange: (isOpen) => {
        if (!isOpen) onClose();
      },
      className: WIDTH,
      children: /* @__PURE__ */ jsx3(Dialog, { children: /* @__PURE__ */ jsxs2("div", { className: "bc:grid bc:gap-3", children: [
        /* @__PURE__ */ jsxs2("div", { className: "bc:flex bc:items-center bc:justify-between bc:gap-2", children: [
          /* @__PURE__ */ jsx3(Heading, { slot: "title", className: "bc:m-0 bc:font-sans bc:text-base bc:font-semibold", children: args.title }),
          /* @__PURE__ */ jsx3(Button, { variant: "secondary", onPress: onClose, children: "Close" })
        ] }),
        /* @__PURE__ */ jsx3(Mounted, { mount, className: "bc:min-w-0" })
      ] }) })
    }
  );
}
var WIDTH = "bc:min-w-[min(420px,100%)]";
export {
  drawDialog
};
