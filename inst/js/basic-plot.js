// srcts/util/util.ts
function isObject(x) {
  return typeof x === "object" && x !== null && !Array.isArray(x);
}

// srcts/modules/basic-plot/index.ts
var api = 1;
var renderer = {
  render({ el, host }) {
    const alert = document.createElement("div");
    alert.setAttribute("role", "alert");
    alert.className = "bc:rounded bc:border bc:border-red-300 bc:bg-red-50 bc:p-2 bc:text-sm bc:text-red-800 bc:whitespace-pre-wrap bc:dark:border-red-800 bc:dark:bg-red-950 bc:dark:text-red-200";
    const img = document.createElement("img");
    img.alt = "Plot";
    img.className = "bc:max-w-none bc:align-top";
    const root = document.createElement("div");
    root.className = "bc:flex bc:flex-col bc:gap-2";
    root.append(alert, img);
    el.replaceChildren(root);
    let shown = null;
    let url = null;
    const draw = () => {
      const body = host.body();
      if (body !== shown) {
        if (url !== null) URL.revokeObjectURL(url);
        shown = body;
        url = body === null ? null : URL.createObjectURL(body);
      }
      if (url === null) img.removeAttribute("src");
      else img.src = url;
      img.hidden = url === null;
      const output = host.output();
      const { width, height } = isObject(output) ? output : {};
      if (typeof width === "number" && typeof height === "number") {
        img.width = width;
        img.height = height;
      }
      const loading = host.loading();
      root.setAttribute("aria-busy", String(loading));
      img.classList.toggle("bc:opacity-60", loading);
      const error = host.error();
      alert.hidden = error === null;
      alert.textContent = error?.message ?? "";
    };
    draw();
    const off = host.on("output", draw);
    return () => {
      off();
      if (url !== null) URL.revokeObjectURL(url);
    };
  }
};
var basic_plot_default = renderer;
export {
  api,
  basic_plot_default as default
};
