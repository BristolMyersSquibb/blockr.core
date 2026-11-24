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
# R copies each file that it names into a directory of its own.
# Shiny serves that directory as a resource path, which httpuv serves from its
# own thread, so that a module loads while R evaluates a block.
# With a package's file, R also copies the other files in the file's directory,
# each at its path.
# So a module or its stylesheet can import a file beside it, by a static
# import, a dynamic import or an `@import`.
# The browser reads the URL of such a file off the import, so the URL holds no
# hash.
# A package names the file with a hash of its content instead, as core's build
# names the bundle that holds React `vendor-<hash>.js`.
# A changed file then has a new name, and the module that imports it changes
# with it.
# So R copies the other files only when it copies the module or the stylesheet
# itself: the first time that it serves it in a session, and again once it
# changed.
# A file from no package is copied alone, so that R serves nothing else of an
# app's directory, such as its secrets.

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
# reports the module as failing to load. Where the module is that of a plugin
# class or a board option, the client then opens no board. Where it is a
# block's UI module, the client reports the failure where it would mount the
# module, while the rest of the page carries on.
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

  if (copy_served_file(file, prefix, path, hash) && not_null(package)) {
    copy_files_beside(file, prefix, path)
  }

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
# It returns whether it copied the file.
copy_served_file <- function(file, prefix, path, hash) {

  dir <- file.path(served_dir(), prefix)
  dest <- file.path(dir, path)
  copy <- !file.exists(dest) || !identical(file_hash(dest), hash)

  if (copy) {
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    file.copy(file, dest, overwrite = TRUE)
  }

  if (!identical(served_path(prefix), dir_path(dir))) {
    shiny::addResourcePath(prefix, dir)
  }

  invisible(copy)
}

# Copies the other files in the directory of a package's file, which R serves
# at `path` under `prefix`, each beside the file's copy, where its copy is
# missing or holds other content.
# It copies neither the directories within nor hidden files.
copy_files_beside <- function(file, prefix, path) {

  beside <- list.files(dirname(file), full.names = TRUE)
  beside <- beside[!dir.exists(beside) & basename(beside) != basename(file)]

  for (other in beside) {
    copy_served_file(
      other,
      prefix,
      file.path(dirname(path), basename(other)),
      file_hash(other)
    )
  }

  invisible()
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

# The import map of the page that R serves, as the script tag for the page's
# head. It gives each import that core's build leaves in a module, such as
# `react` and `@blockr/core/react`, the URL at which R serves the file that
# blockr.core ships for it, as inst/js/import-map.json names them.
# R serves the files that those import in turn, such as the vendor file, as it
# serves every file beside a package's module.
# A browser takes an import map only ahead of the page's first module script,
# so the page puts this tag before any such script.
# The map gives each URL relative to the page, starting with `./`, since it
# takes no other relative URL.
import_map <- function() {

  map <- yyjsonr::read_json_file(
    system.file("js", "import-map.json", package = "blockr.core")
  )

  imports <- lapply(
    map[["imports"]],
    function(file) {
      module <- js_module(file.path("js", file), package = "blockr.core")
      paste0("./", served_url(module))
    }
  )

  shiny::tags$script(
    type = "importmap",
    shiny::HTML(
      yyjsonr::write_json_str(list(imports = imports), auto_unbox = TRUE)
    )
  )
}

# The links in the page's head that have the browser fetch the files that the
# page imports as it starts, all at once as the page loads, rather than each
# once the file that imports it has loaded.
# They name core's runtime, at the URL given, and the `modules` given, each
# with its stylesheet, and what these import up front, and the files that those
# import in turn (see `imported_files()`).
# A module and a file that it imports come as `<link rel="modulepreload">`, and
# a stylesheet as `<link rel="preload">`.
# The runtime and what it imports keep their priority, since the page needs
# them before Shiny connects.
# The rest comes at a low priority, since the page needs it only once R has
# answered, so that it takes no bandwidth from the runtime.
# R serves each module as it names it here, so that a fresh app serves the
# module of a package before the first reply to `hello` names it.
# A module whose file R cannot find gets no link.
preload_links <- function(runtime, modules = list()) {

  found <- Filter(function(x) nzchar(module_file(x)), modules)
  served <- lapply(found, served_module)

  first <- imported_files(list(core_runtime()))
  sheets <- lapply(
    Filter(function(x) nzchar(module_file(x, ".css")), found),
    function(x) list(package = x[["package"]], path = module_path(x, ".css"))
  )
  then <- imported_files(c(found, sheets))

  css <- function(x) grepl("[.]css$", x[["path"]])

  early <- unique(c(runtime, first[["url"]][!css(first)]))
  late <- setdiff(
    unique(c(chr_xtr(served, "module"), then[["url"]][!css(then)])),
    early
  )
  styles <- unique(
    c(unlst(lst_xtr(served, "css")), first[["url"]][css(first)],
      then[["url"]][css(then)])
  )

  link <- function(url, ...) {
    shiny::tags$link(rel = "modulepreload", href = paste0("./", url), ...)
  }

  shiny::tagList(
    lapply(early, link),
    lapply(late, link, fetchpriority = "low"),
    lapply(
      paste0("./", styles),
      function(url) {
        shiny::tags$link(
          rel = "preload", href = url, as = "style", fetchpriority = "low"
        )
      }
    )
  )
}

# What the files given import up front, as the build of each file's package
# lists it in the preload.json beside the file, and the files that those
# import in turn, each by its path in its package and by the URL at which the
# page imports it, in `path` and `url`.
# An import by name is a file of core's that the import map names, at the URL
# that the map gives it.
# An import of a file beside the importing file comes at its path, without a
# hash, since the browser reads that URL off the import.
# A file from no package lists nothing.
imported_files <- function(files) {

  map <- yyjsonr::read_json_file(
    system.file("js", "import-map.json", package = "blockr.core")
  )[["imports"]]

  lists <- new.env(parent = emptyenv())

  listed <- function(file) {

    pkg <- file[["package"]]

    if (is.null(pkg)) {
      return(character())
    }

    dir <- dirname(file[["path"]])
    key <- paste(pkg, dir, sep = "/")
    imports <- rlang::env_cache(
      lists,
      key,
      {
        json <- package_file(file.path(dir, "preload.json"), pkg)
        if (nzchar(json)) yyjsonr::read_json_file(json) else list()
      }
    )

    as.character(unlst(imports[[basename(file[["path"]])]]))
  }

  paths <- character()
  urls <- character()
  queue <- files

  while (length(queue)) {

    file <- queue[[1L]]
    queue <- queue[-1L]

    for (name in listed(file)) {

      if (grepl("^[.]/[^/]+$", name)) {
        next_file <- list(
          package = file[["package"]],
          path = file.path(dirname(file[["path"]]), substring(name, 3L))
        )
        url <- url_path(next_file[["package"]], next_file[["path"]])
      } else if (name %in% names(map)) {
        next_file <- list(
          package = "blockr.core",
          path = file.path("js", map[[name]])
        )
        url <- served_url(js_module(next_file[["path"]], "blockr.core"))
      } else {
        next
      }

      if (url %in% urls) {
        next
      }

      paths <- c(paths, next_file[["path"]])
      urls <- c(urls, url)
      queue <- c(queue, list(next_file))
    }
  }

  list(path = paths, url = urls)
}
