.onLoad <- function(libname, pkgname) { # nocov start

  register_module("basic", js_module("js/basic.js"))

  # Core's inputs for modules written without React,
  # which other packages' modules reach as blockr.core-controls.
  register_module("controls", js_module("js/controls.js"))

  register_core_blocks()

  trace_block_classes(pkgname)

  invisible(NULL)
} # nocov end
