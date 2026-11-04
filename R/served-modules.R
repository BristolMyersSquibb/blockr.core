# R serves each module that it names to the client, and the stylesheet beside
# it, at a URL relative to the page that holds a hash of the file's content, so
# that no browser runs a changed file from its cache. A package's file is
# served under the package's name, at its path in the package's inst/, with the
# hash in the query, as `blockr.core/js/head-block.js?v=9a41f2c7`. A file from
# no package, such as the module of a block class that a script defines, is
# served under `blockr-modules`, in a directory named by the hash, as
# `blockr-modules/5e0c8b1d/my-block.js`. So it has a URL of its own, read from
# the path that the script gave, rather than a `blob:` URL, which a content
# security policy may forbid.
#
# R copies each file that it names into a directory of its own, so that it
# serves those files and nothing beside them, such as the other files of an
# app's directory. Shiny serves that directory as a resource path, which httpuv
# serves from its own thread, so that a module loads while R evaluates a block.

# Where R serves a module and the stylesheet beside it, as the client gets
# them: the URL of the module in `module`, and that of the stylesheet in `css`,
# for a module that ships one.
served_module <- function(module) {

  css <- module_file(module, ".css")

  c(
    list(module = served_url(module)),
    if (nzchar(css)) list(css = served_url(module, ".css"))
  )
}

# The URL at which R serves a module, or the file that it ships beside it with
# `suffix`, which R copies into the directory that it serves. A module whose
# file R cannot find gets a URL at which nothing is served, so that the client
# reports the module as failing to load, where it would mount it, while the
# rest of the page carries on.
served_url <- function(module, suffix = NULL) {

  path <- module_path(module, suffix)
  file <- module_file(module, suffix)
  package <- module[["package"]]
  prefix <- coal(package, "blockr-modules", fail_all = FALSE)

  if (!nzchar(file)) {

    log_warn(
      "R finds no file {path}",
      if (not_null(package)) " in package {package}" else "",
      ", so the URL that it sends for it serves nothing."
    )

    return(url_path(prefix, if (is.null(package)) basename(path) else path))
  }

  hash <- file_hash(file)

  if (is.null(package)) {
    path <- file.path(hash, basename(path))
  }

  copy_served_file(file, prefix, path, hash)

  paste0(url_path(prefix, path), if (not_null(package)) paste0("?v=", hash))
}

file_hash <- function(file) {
  substr(unname(tools::md5sum(file)), 1L, 8L)
}

url_path <- function(prefix, path) {
  parts <- c(prefix, strsplit(path, "/", fixed = TRUE)[[1L]])
  paste(chr_ply(parts, utils::URLencode, reserved = TRUE), collapse = "/")
}

# Copies a file into the directory under `prefix` that R serves, at `path`,
# where it is missing or holds other content, and has Shiny serve that
# directory under `prefix`. A running app serves it from then on, and one
# started later from its start.
copy_served_file <- function(file, prefix, path, hash) {

  dir <- file.path(served_dir(), prefix)
  dest <- file.path(dir, path)

  if (!file.exists(dest) || !identical(file_hash(dest), hash)) {
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    file.copy(file, dest, overwrite = TRUE)
  }

  if (!identical(served_path(prefix), dir_path(dir))) {
    shiny::addResourcePath(prefix, dir)
  }

  invisible(dest)
}

# The directory that Shiny serves under `prefix`, or `NA` where it serves none,
# in the form that `dir_path()` gives, since Shiny keeps a path on Windows with
# forward slashes, where `normalizePath()` gives backslashes.
served_path <- function(prefix) {

  path <- unname(shiny::resourcePaths()[prefix])

  if (is.na(path)) {
    return(NA_character_)
  }

  dir_path(path)
}

dir_path <- function(dir) {
  normalizePath(dir, winslash = "/", mustWork = FALSE)
}

served_dir <- function() {
  file.path(tempdir(), "blockr-served")
}
