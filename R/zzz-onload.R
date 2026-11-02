.onLoad <- function(libname, pkgname) { # nocov start

  register_front_end(
    "basic",
    js_module("js/basic.js"),
    class = "board",
    options = list(new_show_conditions_option)
  )

  invisible(NULL)
} # nocov end
