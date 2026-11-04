// srcts/util/util.ts
function isObject(x) {
  return typeof x === "object" && x !== null && !Array.isArray(x);
}

// srcts/modules/basic-text/index.ts
var api = 1;
var renderer = {
  render({ el, host }) {
    const alert = document.createElement("div");
    alert.setAttribute("role", "alert");
    alert.className = "bc:rounded bc:border bc:border-red-300 bc:bg-red-50 bc:p-2 bc:text-sm bc:text-red-800 bc:whitespace-pre-wrap bc:dark:border-red-800 bc:dark:bg-red-950 bc:dark:text-red-200";
    const pre = document.createElement("pre");
    pre.tabIndex = 0;
    pre.className = "bc:m-0 bc:min-h-0 bc:overflow-auto bc:rounded bc:bg-gray-50 bc:p-2 bc:font-mono bc:text-xs bc:leading-snug bc:whitespace-pre bc:text-gray-900 bc:dark:bg-gray-900 bc:dark:text-gray-100";
    const root = document.createElement("div");
    root.className = "bc:flex bc:max-h-full bc:flex-col bc:gap-2";
    root.append(alert, pre);
    el.replaceChildren(root);
    const draw = () => {
      const output = host.output();
      const text = isObject(output) ? output.text : void 0;
      pre.textContent = typeof text === "string" ? text : "";
      const loading = host.loading();
      root.setAttribute("aria-busy", String(loading));
      pre.classList.toggle("bc:opacity-60", loading);
      const error = host.error();
      alert.hidden = error === null;
      alert.textContent = error?.message ?? "";
    };
    draw();
    return host.on("output", draw);
  }
};
var basic_text_default = renderer;
export {
  api,
  basic_text_default as default
};
