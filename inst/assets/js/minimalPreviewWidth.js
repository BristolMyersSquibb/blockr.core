(function() {

  function previewCols(el) {
    var cs = getComputedStyle(el);
    var probe = document.createElement("span");
    probe.style.font = cs.font;
    probe.style.whiteSpace = "pre";
    probe.style.visibility = "hidden";
    probe.style.position = "absolute";
    probe.textContent = Array(41).join("x");
    document.body.appendChild(probe);
    var per = probe.getBoundingClientRect().width / 40;
    document.body.removeChild(probe);
    var pad = parseFloat(cs.paddingLeft) + parseFloat(cs.paddingRight);
    return Math.max(20, Math.floor((el.clientWidth - pad) / per));
  }

  function observe(el) {
    if (el.dataset.blockrWidthObserved) return;
    el.dataset.blockrWidthObserved = "1";

    var timer;
    function report() {
      clearTimeout(timer);
      timer = setTimeout(function() {
        if (window.Shiny) {
          Shiny.setInputValue(el.id + "_cols", previewCols(el));
        }
      }, 150);
    }

    report();
    if (window.ResizeObserver) {
      new ResizeObserver(report).observe(el);
    }
  }

  function scan() {
    document.querySelectorAll("pre.blockr-minimal-preview").forEach(observe);
  }

  $(document).on("shiny:connected shiny:value", scan);
})();
