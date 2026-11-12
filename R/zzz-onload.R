.onLoad <- function(libname, pkgname) { # nocov start

  register_module("basic", js_module("js/basic.js"))

  # The UI of core's Edit board plugin, which the plugin's class names as
  # blockr.core-edit_board.
  register_module("edit_board", js_module("js/edit-board.js"))

  # Core's inputs for modules written without React,
  # which other packages' modules reach as blockr.core-controls,
  # in the file that the page's import map names `@blockr/core/controls`,
  # so that both reach one copy.
  register_module("controls", js_module("js/blockr-core-controls.js"))

  register_core_blocks()

  trace_block_classes(pkgname)

  invisible(NULL)
} # nocov end
