# Restores the block registry of the process once `frame` ends, as it was when
# this was called, so that the classes that a test registers go and those
# that it removes or replaces come back.
local_block_registry <- function(frame = parent.frame()) {

  saved <- registry_entries(block_registry)

  withr::defer(
    {
      empty_block_registry()
      for (entry in saved) {
        put_registry_entry(entry, block_registry)
      }
    },
    envir = frame
  )

  invisible()
}

# Removes every class from the block registry of the process.
empty_block_registry <- function() {

  for (part in c("packages", "script")) {
    env <- block_registry[[part]]
    rm(list = ls(env, all.names = TRUE), envir = env)
  }

  invisible()
}

# Whether the block registry of the process holds the class whose package is
# `package`, `NA` for a class that an app script defines, and whose id is
# `id`, as a row of `list_blocks()` gives it.
lists_block <- function(id, package = NA_character_) {

  rows <- list_blocks()
  same_package <- if (is.na(package)) {
    is.na(rows$package)
  } else {
    !is.na(rows$package) & rows$package == package
  }

  any(rows$id == id & same_package)
}

# The app's plugin whose class is `id` of `package`, among `plugins`, as
# `app_plugins()` gives them, or `NULL` where none is.
app_plugin <- function(plugins, package, id) {
  find_pkg_ref(plugins, pkg_ref(package, id))
}

# The classes of `plugins`, as `app_plugins()` gives them, as rows with each
# class's package in `package` and its id in `id`, in their order.
plugin_class_rows <- function(plugins) {
  data.frame(
    package = chr_xtr(plugins, "package"),
    id = chr_xtr(plugins, "id")
  )
}

# The entry of the block class `id` of `package` in `registry`, an array of
# entries as R's reply to `hello` gives it in `block_registry`, or `NULL`.
reply_entry <- function(registry, id, package = NULL) {
  find_pkg_ref(registry, pkg_ref(package, id))
}

# The registry id of core's head block.
core_head_ref <- function() {
  pkg_ref("blockr.core", "head_block")
}

# How a message names each of the renderers `x`, by the package of its module
# and its name.
renderer_labels <- function(x) {
  chr_ply(unname(x), function(r) pkg_ref_label(renderer_ref(r)))
}

# How a message names each of `x`, a list of entries that each hold a
# `package` and an `id`, such as the entries of a block registry.
ref_labels <- function(x) {
  chr_ply(unname(x), pkg_ref_label)
}
