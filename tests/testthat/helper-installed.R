# Installs the package optprobe into a temporary library, which `.libPaths()`
# lists first until `frame` ends, and unloads the package then.
# The package imports none, and lists the board option probe_gap, with its
# module, in the `._BLOCKR_MODULES_` that it exports.
# It gives the board option the request ping, which answers "pong", through a
# method of `board_option_requests()` that R registers as the package loads.
# It is installed rather than loaded by `pkgload::load_all()`, so that a test
# can unload it and R can load it again by its name.
# Core's tests leave the listings of installed packages out of what
# `available_packages()` gives (see setup-listings.R).
# So the available packages hold this one alone among them, until `frame`
# ends.
local_installed_package <- function(frame = parent.frame()) {

  testthat::skip_on_cran()

  dir <- withr::local_tempdir(.local_envir = frame)
  src <- file.path(dir, "optprobe")
  lib <- file.path(dir, "library")

  files <- list(
    DESCRIPTION = c(
      "Package: optprobe",
      "Title: Probes What an App Holds of a Package",
      "Version: 0.0.1",
      "Description: Lists one board option, with a request, for the tests of",
      "  blockr.core.",
      "License: MIT",
      "Encoding: UTF-8"
    ),
    NAMESPACE = c(
      "export(._BLOCKR_MODULES_)",
      "S3method(blockr.core::board_option_requests, probe_gap_option)"
    ),
    `R/listing.R` = c(
      "._BLOCKR_MODULES_ <- list(",
      "  options = list(probe_gap = list(module = \"js/probe-gap.js\"))",
      ")"
    ),
    `R/requests.R` = c(
      "board_option_requests.probe_gap_option <- function(x, ...) {",
      "  list(ping = function() \"pong\")",
      "}"
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

  available <- available_packages

  testthat::local_mocked_bindings(
    available_packages = function() sort(unique(c(available(), "optprobe"))),
    .package = "blockr.core",
    .env = frame
  )

  withr::defer(
    if (isNamespaceLoaded("optprobe")) unloadNamespace("optprobe"),
    envir = frame
  )

  invisible(lib)
}
