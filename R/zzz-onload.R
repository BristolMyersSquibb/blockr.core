.onLoad <- function(libname, pkgname) { # nocov start

  register_front_end(
    "basic",
    js_module("js/basic.js"),
    class = "board",
    options = default_board_options
  )

  invisible(NULL)
} # nocov end
