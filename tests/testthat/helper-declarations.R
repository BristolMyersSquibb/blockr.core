# Restores what `env` holds of what packages declare once the calling test
# ends, so that a test can declare for packages of its own
local_declared <- function(env, frame = parent.frame()) {

  # The block registry holds its classes in environments of its own.
  # Linting skips the call of local_block_registry(), a helper of another
  # file, which lintr does not see.
  if (identical(env, block_registry)) {
    return(local_block_registry(frame)) # nolint: object_usage_linter.
  }

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

# Has the available packages hold `pkg`, which no library holds, as a package
# that lists `listing` in its `._BLOCKR_MODULES_`, a list of its parts by name,
# until `frame` ends.
# A package whose listing gives plugin classes is no fit for it, since R looks
# up their constructors in the package's namespace, which `pkg` lacks; a test
# lists those with `local_plugin_classes()`.
# The same holds for output families, which a test lists with
# `local_family_package()`.
local_listing <- function(pkg, listing = list(), frame = parent.frame()) {

  force(pkg)
  force(listing)

  available <- available_packages
  read <- package_listing

  given <- lapply(
    set_names(nm = listing_parts),
    function(part) coal(listing[[part]], list(), fail_all = FALSE)
  )

  testthat::local_mocked_bindings(
    available_packages = function() sort(unique(c(available(), pkg))),
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
          set_names(nm = listing_parts),
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
