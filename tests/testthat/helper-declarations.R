# Restores what `env` holds of what packages declare once the calling test
# ends, so that a test can declare for packages of its own
local_declared <- function(env, frame = parent.frame()) {

  old <- as.list(env, all.names = TRUE)

  withr::defer(
    {
      rm(list = ls(env, all.names = TRUE), envir = env)
      list2env(old, envir = env)
    },
    envir = frame
  )
}

# The rows of `available_blocks()` for core's block classes, as an app that
# offers core's classes alone gives them to `serve()` in `blocks`.
core_block_rows <- function() {
  rows <- available_blocks()
  rows[rows[["package"]] == "blockr.core", ]
}

test_module <- function(path, package = "blockr.test") {
  js_module(path, package = package)
}

# Has the available packages hold `pkg`, which no library holds, as a loaded
# package that lists `listing` in its `._BLOCKR_MODULES_`, a list of its parts
# by name, until `frame` ends.
# A package whose listing gives plugin classes is no fit for it, since R looks
# up their constructors in the package's namespace, which `pkg` lacks; a test
# lists those with `local_plugin_classes()`.
# The function `loaded_listing_packages()` leaves `pkg` out, since an app holds
# the namespace of each package that it gives (see `serve_app()`).
# Where R reads the modules that packages offer, or the board options that they
# list, it reads those of `pkg` too, whichever packages it reads them for.
local_listing <- function(pkg, listing = list(), frame = parent.frame()) {

  force(pkg)
  force(listing)

  available <- available_packages
  offered <- offered_modules
  options <- loaded_options
  read <- package_listing

  given <- lapply(
    set_names(nm = c("blocks", "plugins", "options", "modules")),
    function(part) coal(listing[[part]], list(), fail_all = FALSE)
  )

  testthat::local_mocked_bindings(
    available_packages = function() sort(unique(c(available(), pkg))),
    offered_modules = function(pkgs = loaded_listing_packages()) {
      offered(sort(unique(c(pkgs, pkg))))
    },
    loaded_options = function(pkgs = loaded_listing_packages()) {
      options(sort(unique(c(pkgs, pkg))))
    },
    package_listing = function(pkg_name) {
      if (identical(pkg_name, pkg)) given else read(pkg_name)
    },
    .package = "blockr.core",
    .env = frame
  )

  invisible()
}

# Has core list nothing in its `._BLOCKR_MODULES_` until `frame` ends, so that
# what R sends a client holds nothing of core's, whose modules have URLs that
# change with core's JS.
local_empty_core_listing <- function(frame = parent.frame()) {

  read <- package_listing

  testthat::local_mocked_bindings(
    package_listing = function(pkg) {
      if (identical(pkg, "blockr.core")) {
        lapply(
          set_names(nm = c("blocks", "plugins", "options", "modules")),
          function(part) list()
        )
      } else {
        read(pkg)
      }
    },
    .package = "blockr.core",
    .env = frame
  )

  invisible()
}
