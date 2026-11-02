.onLoad <- function(libname, pkgname) { # nocov start

  register_front_end(
    "basic",
    js_module("js/basic.js"),
    class = "board",
    options = default_board_options
  )

  register_blockr_options(
    gate_visibility = TRUE,
    evaluation_queue = 2L,
    background_evaluation_delay = Inf
  )

  invisible(NULL)
} # nocov end
