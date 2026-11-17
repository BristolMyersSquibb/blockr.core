# Installs the package optprobe into a temporary library, which `.libPaths()`
# lists first until `frame` ends, and unloads the package then.
# The package imports none, and lists the board option probe_gap, with its
# module, in the `._BLOCKR_MODULES_` that it exports.
# It is installed rather than loaded by `pkgload::load_all()`, so that a test
# can unload it and R can load it again with `loadNamespace()`.
# Core's tests leave the listings of installed packages out of what
# `available_packages()` gives (see setup-listings.R).
# So the package's board option reaches the replies to `hello` only while it
# is loaded.
local_installed_package <- function(frame = parent.frame()) {

  testthat::skip_on_cran()

  dir <- withr::local_tempdir(.local_envir = frame)
  src <- file.path(dir, "optprobe")
  lib <- file.path(dir, "library")

  files <- list(
    DESCRIPTION = c(
      "Package: optprobe",
      "Title: Probes Which Packages an App Loads",
      "Version: 0.0.1",
      "Description: Lists one board option for the tests of blockr.core.",
      "License: MIT",
      "Encoding: UTF-8"
    ),
    NAMESPACE = "export(._BLOCKR_MODULES_)",
    `R/listing.R` = c(
      "._BLOCKR_MODULES_ <- list(",
      "  options = list(probe_gap = list(module = \"js/probe-gap.js\"))",
      ")"
    ),
    `inst/js/probe-gap.js` = "export default {};"
  )

  write_package_files(src, files) # nolint: object_usage_linter.

  dir.create(lib)

  utils::install.packages(
    src,
    lib = lib,
    repos = NULL,
    type = "source",
    quiet = TRUE
  )

  if (!dir.exists(file.path(lib, "optprobe"))) {
    stop("R did not install the test package optprobe.")
  }

  withr::local_libpaths(lib, action = "prefix", .local_envir = frame)

  withr::defer(
    if (isNamespaceLoaded("optprobe")) unloadNamespace("optprobe"),
    envir = frame
  )

  invisible(lib)
}
