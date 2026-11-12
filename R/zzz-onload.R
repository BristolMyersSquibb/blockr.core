.onLoad <- function(libname, pkgname) { # nocov start

  list2env(core_listing(pkgname), envir = ._BLOCKR_MODULES_)

  trace_block_classes(pkgname)

  invisible(NULL)
} # nocov end

# What core lists in its `._BLOCKR_MODULES_`, which its `.onLoad()` fills: its
# block classes, as its `inst/registry/blocks.yml` lists them, its plugin
# classes and its board options, each with its module, and its inputs for
# modules written without React, which it offers other packages' modules as
# blockr.core-controls, in the file that the page's import map names
# `@blockr/core/controls`, so that both reach one copy.
core_listing <- function(pkgname = "blockr.core") {
  list(
    blocks = read_blocks_yml(pkgname),
    plugins = list(
      new_basic_frontend_plugin = list(
        id = "basic_frontend_plugin",
        name = "Basic frontend",
        module = "js/basic.js"
      ),
      new_file_storage_plugin = list(
        id = "file_storage_plugin",
        name = "File storage",
        module = "js/file-storage.js"
      ),
      new_edit_board_plugin = list(
        id = "edit_board_plugin",
        name = "Edit board",
        module = "js/edit-board.js"
      )
    ),
    options = list(
      n_rows = list(module = "js/n-rows-option.js"),
      show_conditions = list(module = "js/show-conditions-option.js"),
      dark_mode = list(module = "js/dark-mode-option.js")
    ),
    modules = list(controls = "js/blockr-core-controls.js")
  )
}
