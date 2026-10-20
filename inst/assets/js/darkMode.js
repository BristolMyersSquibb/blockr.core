// The dark mode board option. Its value is "light", "dark" or unset, which
// follows the system setting, and its box reports TRUE or FALSE, which the
// option maps to "dark" and "light". The scheme is `data-bs-theme` on <html>:
// a line in <head> sets it before the first paint, and this script from then
// on.
(function() {

  var system = window.matchMedia("(prefers-color-scheme: dark)");

  // Whether the page follows the system setting: while the option is unset,
  // until a value is chosen on the page or set on the server.
  var follow = false;

  function boxes() {
    return document.querySelectorAll("input.blockr-dark-mode");
  }

  function scheme() {
    return document.documentElement.getAttribute("data-bs-theme");
  }

  function apply(mode) {
    document.documentElement.setAttribute("data-bs-theme", mode);
  }

  // A value from a box or from the server. One that differs from the page's
  // scheme was chosen, and the page stops following the system; the server's
  // echo of a change made on the page does not differ.
  function choose(mode) {
    if (mode !== scheme()) {
      follow = false;
      apply(mode);
    }
  }

  // Run before Shiny reads its inputs, which it does once the document is
  // ready, so that the box of an unset option reports the system setting.
  function init() {
    boxes().forEach(function(box) {
      if (box.hasAttribute("data-follow-system")) {
        follow = true;
        box.checked = system.matches;
      }
    });
  }

  // Shiny's binding reports the change as it would a click, so the option
  // takes the system's value, as it did with bslib's toggle.
  system.addEventListener("change", function(e) {
    if (follow) {
      apply(e.matches ? "dark" : "light");
      $(boxes()).prop("checked", e.matches).trigger("change");
    }
  });

  $(document).on("change", "input.blockr-dark-mode", function() {
    choose(this.checked ? "dark" : "light");
  });

  Shiny.addCustomMessageHandler("blockr-dark-mode", choose);

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
