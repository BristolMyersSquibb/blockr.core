// srcts/modules/filebrowser-block/index.tsx
import { Fragment, useState } from "react";
import { Field, reactModule, Stack, useRequest, useValue } from "@blockr/core/react";

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
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}
function isObject(x) {
  return typeof x === "object" && x !== null && !Array.isArray(x);
}
function jsonEqual(a, b) {
  if (a === b) return true;
  if (Array.isArray(a)) {
    return Array.isArray(b) && a.length === b.length && a.every((x, i) => jsonEqual(x, b[i]));
  }
  if (isObject(a) && isObject(b)) {
    const keys = Object.keys(a);
    return keys.length === Object.keys(b).length && keys.every((key) => Object.hasOwn(b, key) && jsonEqual(a[key], b[key]));
  }
  return false;
}

// srcts/modules/filebrowser-block/index.tsx
import { Fragment as Fragment2, jsx, jsxs } from "react/jsx-runtime";
var api = 1;
var link = "bc:cursor-pointer bc:rounded bc:border-0 bc:bg-transparent bc:px-2 bc:py-0.5 bc:text-left bc:text-sm bc:text-inherit bc:[font-family:inherit] bc:hover:bg-bg-subtle bc:aria-current:font-semibold";
var entry = `${link} bc:justify-self-stretch`;
function FileBrowserBlock({ host }) {
  const shared = useValue(host, "volumes").value;
  const volumes = asVolumes(shared);
  const file = useValue(host, "file_path");
  const held = typeof file.value === "string" && file.value !== "" ? file.value : void 0;
  const [place, setPlace] = useState(() => locate(file.value, volumes));
  const volume = place.volume !== void 0 && own(volumes, place.volume) !== void 0 ? place.volume : Object.keys(volumes)[0];
  const dirs = volume === place.volume ? place.dirs : [];
  const root = volume === void 0 ? void 0 : own(volumes, volume);
  const listed = useRequest(host, "list_dir", {
    args: { ...volume === void 0 ? {} : { volume }, path: dirs.join("/") },
    reads: shared ?? null
  });
  const loading = listed.loading;
  const listing2 = listed.error === null ? asListing(listed.answer) : { dirs: [], files: [] };
  const error = (loading ? null : listed.error) ?? file.error;
  const go = (next) => {
    if (next.volume === volume && jsonEqual(next.dirs, dirs)) return;
    setPlace(next);
  };
  const names = Object.keys(volumes);
  return /* @__PURE__ */ jsxs(Stack, { children: [
    names.length > 1 && /* @__PURE__ */ jsx(Field, { label: "Volume", children: /* @__PURE__ */ jsx(
      "select",
      {
        className: styles.box,
        value: volume ?? "",
        onChange: (event) => {
          go({ volume: event.target.value, dirs: [] });
        },
        children: names.map((name) => /* @__PURE__ */ jsx("option", { value: name, children: name }, name))
      }
    ) }),
    /* @__PURE__ */ jsx("nav", { "aria-label": "Directory", className: "bc:flex bc:flex-wrap bc:items-center bc:gap-1 bc:text-sm", children: [volume ?? "", ...dirs].map((name, i) => /* @__PURE__ */ jsxs(Fragment, { children: [
      i > 0 && /* @__PURE__ */ jsx("span", { className: "bc:text-text-muted", "aria-hidden": "true", children: "/" }),
      /* @__PURE__ */ jsx(
        "button",
        {
          type: "button",
          className: link,
          "aria-current": i === dirs.length ? "true" : void 0,
          onClick: () => {
            go({ volume, dirs: dirs.slice(0, i) });
          },
          children: name
        }
      )
    ] }, i)) }),
    /* @__PURE__ */ jsx("ul", { className: "bc:m-0 bc:grid bc:max-h-64 bc:list-none bc:gap-0.5 bc:overflow-y-auto bc:p-0", children: loading ? /* @__PURE__ */ jsx(Note, { text: "Loading\u2026" }) : /* @__PURE__ */ jsxs(Fragment2, { children: [
      dirs.length > 0 && /* @__PURE__ */ jsx(
        Entry,
        {
          text: "..",
          onClick: () => {
            go({ volume, dirs: dirs.slice(0, -1) });
          }
        }
      ),
      listing2.dirs.map((dir) => /* @__PURE__ */ jsx(
        Entry,
        {
          text: `${dir}/`,
          onClick: () => {
            go({ volume, dirs: [...dirs, dir] });
          }
        },
        `dir ${dir}`
      )),
      listing2.files.map((name) => {
        const at = root === void 0 ? void 0 : join(root, [...dirs, name]);
        return /* @__PURE__ */ jsx(
          Entry,
          {
            text: name,
            current: at !== void 0 && at === held,
            onClick: () => {
              if (at !== void 0) void file.commit(at);
            }
          },
          `file ${name}`
        );
      }),
      listing2.dirs.length + listing2.files.length === 0 && /* @__PURE__ */ jsx(Note, { text: "This directory is empty." })
    ] }) }),
    held !== void 0 && /* @__PURE__ */ jsx("p", { className: styles.hint, children: `Chosen: ${held}` }),
    error !== null && /* @__PURE__ */ jsx("p", { role: "alert", className: styles.alert, children: error })
  ] });
}
function Entry({ text, current = false, onClick }) {
  return /* @__PURE__ */ jsx("li", { className: "bc:grid", children: /* @__PURE__ */ jsx("button", { type: "button", className: entry, "aria-current": current ? "true" : void 0, onClick, children: text }) });
}
function Note({ text }) {
  return /* @__PURE__ */ jsx("li", { className: "bc:grid", children: /* @__PURE__ */ jsx("p", { className: styles.hint, children: text }) });
}
var filebrowser_block_default = reactModule(FileBrowserBlock);
function locate(file, roots) {
  if (typeof file === "string") {
    for (const [name, root] of Object.entries(roots)) {
      const prefix = `${join(root, [])}/`;
      if (file.startsWith(prefix)) {
        return { volume: name, dirs: file.slice(prefix.length).split("/").slice(0, -1) };
      }
    }
  }
  return { volume: Object.keys(roots)[0], dirs: [] };
}
function asVolumes(volumes) {
  if (volumes === void 0) return {};
  if (isObject(volumes) && Object.values(volumes).every((root) => typeof root === "string")) {
    return volumes;
  }
  throw new TypeError(`The state arg volumes is ${JSON.stringify(volumes)}, where the file browser takes the path of each volume by its name.`);
}
function join(root, names) {
  return [root.replace(/\/+$/, ""), ...names].join("/");
}
function asListing(answer) {
  if (answer === void 0) return { dirs: [], files: [] };
  if (isObject(answer) && isStrings(answer.dirs) && isStrings(answer.files)) {
    return { dirs: answer.dirs, files: answer.files };
  }
  throw new TypeError(`R answered list_dir with ${JSON.stringify(answer)}, where the file browser takes the dirs and the files that it lists.`);
}
function isStrings(value) {
  return Array.isArray(value) && value.every((item) => typeof item === "string");
}
export {
  api,
  asListing,
  asVolumes,
  filebrowser_block_default as default
};
