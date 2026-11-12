# Loads the package regprobe with `pkgload::load_all()`, from a temporary
# directory, until `frame` ends.
# The package defines the block class probe_block, and lists it in the
# `._BLOCKR_MODULES_` that it exports, which its `.onLoad()` fills from its
# `inst/registry/blocks.yml`, or with `registers`, ships no such file, exports
# no such object and registers the class in the block registry of the process
# as it loads.
# Beside it, it ships the class's module, a JSON Schema and the types of its
# JS, each among the files of its `inst/`, which R installs at the package's
# top, while `load_all()` leaves them under `inst/`.
# So base R's `system.file()` finds none of them, since it looks in the
# package's source directory.
# Gives the package's source directory.
local_load_all_package <- function(frame = parent.frame(), registers = FALSE) {

  testthat::skip_if_not_installed("pkgload")

  pkg <- file.path(withr::local_tempdir(.local_envir = frame), "regprobe")

  files <- list(
    DESCRIPTION = c(
      "Package: regprobe",
      "Title: Probes How Core Reads the Files of a Package",
      "Version: 0.0.1",
      "Description: Defines one block class for the tests of blockr.core.",
      "License: MIT",
      "Encoding: UTF-8",
      "Imports: blockr.core"
    ),
    NAMESPACE = c("export(._BLOCKR_MODULES_)", "export(new_probe_block)"),
    `R/listing.R` = c(
      "._BLOCKR_MODULES_ <- new.env(parent = emptyenv())",
      "",
      ".onLoad <- function(libname, pkgname) {",
      "  ._BLOCKR_MODULES_$blocks <- blockr.core::read_blocks_yml(pkgname)",
      "}"
    ),
    `R/probe.R` = c(
      "probe_block <- blockr.core::new_block_class(",
      "  \"probe_block\",",
      "  expr = function() quote(seq_len(1L)),",
      "  ui = blockr.core::js_module(\"js/probe-block.js\")",
      ")",
      "",
      "new_probe_block <- blockr.core::block_constructor(probe_block)"
    ),
    `inst/registry/blocks.yml` = c(
      "new_probe_block:",
      "  id: probe_block",
      "  name: Probe",
      "  description: Probes the registry",
      "  category: input"
    ),
    `inst/js/probe-block.js` = "export const api = 1;",
    `inst/schemas/probe.json` = "{\"type\": \"object\"}",
    `inst/types/package.json` = "{\"name\": \"@blockr/regprobe\"}"
  )

  if (registers) {
    files[["inst/registry/blocks.yml"]] <- NULL
    files[["R/listing.R"]] <- NULL
    files[["NAMESPACE"]] <- "export(new_probe_block)"
    files[["R/zzz.R"]] <- c(
      ".onLoad <- function(libname, pkgname) {",
      "  blockr.core::register_block(",
      "    new_probe_block, \"Probe\", \"Probes the registry\", \"input\"",
      "  )",
      "}"
    )
    withr::defer(
      unregister_blocks(intersect(list_blocks(), "regprobe-probe_block")),
      envir = frame
    )
  }

  for (path in names(files)) {
    file <- file.path(pkg, path)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    writeLines(files[[path]], file)
  }

  pkgload::load_all(pkg, attach = FALSE, helpers = FALSE, quiet = TRUE)
  withr::defer(pkgload::unload("regprobe", quiet = TRUE), envir = frame)

  invisible(normalizePath(pkg))
}
