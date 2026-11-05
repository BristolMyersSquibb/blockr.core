.onLoad <- function(libname, pkgname) { # nocov start

  register_module("basic", js_module("js/basic.js"))

  register_core_blocks()

  trace_block_classes(pkgname)

  invisible(NULL)
} # nocov end
