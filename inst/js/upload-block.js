// srcts/modules/upload-block/index.tsx
import { useEffect, useRef, useState } from "react";
import { strings } from "@blockr/core/binding";
import { Field, reactModule, Stack, useValue } from "@blockr/core/react";

// srcts/styles/styles.ts
var styles = {
  stack: "bc:grid bc:gap-3 bc:font-sans bc:text-text-default",
  field: "bc:grid bc:gap-1 bc:font-sans bc:text-text-default",
  label: "bc:text-sm bc:font-medium",
  box: "bc:min-w-0 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:focus:outline-2 bc:focus:outline-offset-1 bc:focus:outline-border-accent bc:aria-invalid:border-border-danger",
  check: "bc:flex bc:items-center bc:gap-2 bc:font-sans bc:text-sm bc:text-text-default",
  checkbox: "bc:accent-bg-accent bc:aria-invalid:accent-border-danger",
  fieldset: "bc:m-0 bc:grid bc:min-w-0 bc:gap-1 bc:border-0 bc:p-0 bc:font-sans bc:text-text-default",
  legend: "bc:text-sm bc:font-medium bc:mb-1 bc:p-0",
  list: "bc:grid bc:max-h-48 bc:gap-1 bc:overflow-y-auto",
  hint: "bc:m-0 bc:text-sm bc:text-text-muted",
  alert: "bc:m-0 bc:text-sm bc:text-text-danger bc:whitespace-pre-wrap"
};

// srcts/util/util.ts
function errorMessage(error) {
  return error instanceof Error ? error.message : String(error);
}

// srcts/modules/upload-block/index.tsx
import { jsx, jsxs } from "react/jsx-runtime";
var api = 1;
function UploadBlock({ host }) {
  const path = useValue(host, "file_path");
  const [uploading, setUploading] = useState(null);
  const [failed, setFailed] = useState(null);
  const sending = useRef(null);
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
    setFailed(null);
    try {
      const [kept] = strings(await host.request("upload", {}, { file, signal: controller.signal }));
      if (kept === void 0) throw new Error(`R kept ${file.name} but gave no path for it.`);
      await path.commit(kept);
    } catch (error2) {
      if (!controller.signal.aborted) setFailed(errorMessage(error2));
    } finally {
      setUploading(null);
    }
  }
  const held = typeof path.value === "string" && path.value !== "" ? path.value : void 0;
  const error = failed ?? path.error;
  return /* @__PURE__ */ jsxs(Stack, { children: [
    /* @__PURE__ */ jsx(Field, { label: "Upload data", children: /* @__PURE__ */ jsx(
      "input",
      {
        type: "file",
        className: styles.box,
        disabled: uploading !== null,
        onChange: (event) => {
          const file = event.target.files?.[0];
          event.target.value = "";
          if (file !== void 0) void upload(file);
        }
      }
    ) }),
    /* @__PURE__ */ jsx("p", { className: styles.hint, children: uploading !== null ? `Uploading ${uploading}\u2026` : held !== void 0 ? `Uploaded ${basename(held)}` : "No file uploaded yet." }),
    error !== null && /* @__PURE__ */ jsx("p", { role: "alert", className: styles.alert, children: error })
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
