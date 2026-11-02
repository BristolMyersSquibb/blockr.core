.onLoad <- function(libname, pkgname) { # nocov start

  register_front_end("basic", js_module("js/basic.js"), class = "board")

  invisible(NULL)
} # nocov end
