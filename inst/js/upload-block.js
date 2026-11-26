// srcts/modules/upload-block/index.tsx
import { useEffect, useRef, useState } from "react";
import { Button, DropZone, FileTrigger, FormLayout, Text } from "@blockr/ui";
import { strings } from "@blockr/core/binding";
import { reactModule, useValue } from "@blockr/core/react";

// srcts/styles/styles.ts
var styles = {
  stack: "bc:grid bc:gap-3 bc:font-sans",
  label: "bc:text-sm bc:font-medium",
  fieldset: "bc:m-0 bc:grid bc:min-w-0 bc:gap-1 bc:border-0 bc:p-0 bc:font-sans",
  legend: "bc:text-sm bc:font-medium bc:mb-1 bc:p-0",
  hint: "bc:m-0 bc:text-sm bc:text-muted-foreground",
  alert: "bc:m-0 bc:text-sm bc:text-destructive-text bc:whitespace-pre-wrap"
};

// srcts/modules/upload-block/index.tsx
import { jsx, jsxs } from "react/jsx-runtime";
var api = 1;
function UploadBlock({ host }) {
  const path = useValue(host, "file_path");
  const [uploading, setUploading] = useState(null);
  const sending = useRef(null);
  const unanswered = useRef(null);
  useEffect(
    () => () => {
      sending.current?.abort();
    },
    []
  );
  async function upload(file) {
    const controller = new AbortController();
    sending.current = controller;
    setUploading(file.name);
    unanswered.current?.();
    unanswered.current = null;
    try {
      const [kept] = strings(await host.request("upload", {}, { file, signal: controller.signal }));
      if (kept === void 0) {
        unanswered.current = host.notify({
          severity: "error",
          code: "upload_unanswered",
          message: `R kept ${file.name} but gave no path for it.`
        });
      } else {
        await path.commit(kept);
      }
    } catch {
    } finally {
      setUploading(null);
    }
  }
  const held = typeof path.value === "string" && path.value !== "" ? path.value : void 0;
  const busy = uploading !== null;
  const said = uploading !== null ? `Uploading ${uploading}\u2026` : held === void 0 ? null : `Uploaded ${basename(held)}`;
  return /* @__PURE__ */ jsxs(FormLayout, { children: [
    /* @__PURE__ */ jsxs(
      DropZone,
      {
        isDisabled: busy,
        onDrop: (event) => {
          const item = event.items.find((dropped) => dropped.kind === "file");
          if (item?.kind === "file") void item.getFile().then(upload);
        },
        children: [
          /* @__PURE__ */ jsx(Text, { slot: "label", className: styles.label, children: "Upload data" }),
          /* @__PURE__ */ jsx("span", { className: styles.hint, children: "Drop a file here, or" }),
          /* @__PURE__ */ jsx(
            FileTrigger,
            {
              onSelect: (files) => {
                const file = files?.[0];
                if (file !== void 0) void upload(file);
              },
              children: /* @__PURE__ */ jsx(Button, { variant: "secondary", isDisabled: busy, children: "Choose a file" })
            }
          )
        ]
      }
    ),
    said !== null && /* @__PURE__ */ jsx("p", { className: styles.hint, children: said }),
    path.error !== null && /* @__PURE__ */ jsx("p", { role: "alert", className: styles.alert, children: path.error })
  ] });
}
var upload_block_default = reactModule(UploadBlock);
function basename(path) {
  return path.replace(/[\\/]+$/, "").split(/[\\/]/).pop() ?? path;
}
export {
  api,
  upload_block_default as default
};
