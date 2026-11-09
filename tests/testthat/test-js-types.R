write_description <- function(dir, ...) {
  writeLines(c("Package: blockr.test", "Version: 0.0.1", ...),
             file.path(dir, "DESCRIPTION"))
}

# A directory of types that a package ships, named in its package.json, with
# no peers, so that npm installs it without the registry.
write_types <- function(dir, name) {
  types <- file.path(dir, name, "types")
  dir.create(types, recursive = TRUE)
  writeLines(
    sprintf('{ "name": "@blockr/%s", "private": true }', name),
    file.path(types, "package.json")
  )
  writeLines("export {};", file.path(types, "index.d.ts"))
  types
}

test_that("dependencies follow Depends and Imports of installed packages", {

  dir <- withr::local_tempdir()
  write_description(
    dir,
    "Depends: R (>= 4.1.0)",
    "Imports: blockr.core (>= 0.2.0),\n    utils",
    "Suggests: notapackage"
  )

  deps <- hard_dependencies(file.path(dir, "DESCRIPTION"))

  expect_contains(deps, c("blockr.core", "utils"))
  # Through blockr.core's Imports
  expect_contains(deps, c("shiny", "yyjsonr"))
  expect_false("R" %in% deps)
  expect_false("notapackage" %in% deps)
  expect_identical(deps, unique(deps))
})

test_that("only packages that ship types with a package.json count", {

  dirs <- js_types_dirs(c("utils", "blockr.core", "notapackage"))

  expect_named(dirs, "blockr.core")
  expect_true(file.exists(file.path(dirs[["blockr.core"]], "package.json")))
})

test_that("install_js_types() installs every package's types with one call", {

  dir <- withr::local_tempdir()
  write_description(dir, "Imports: blockr.core")

  calls <- list()
  local_mocked_bindings(
    npm_install_links = function(path, dirs) {
      calls[[length(calls) + 1L]] <<- list(path = path, dirs = dirs)
      0L
    }
  )

  res <- withVisible(install_js_types(dir))

  expect_false(res$visible)
  expect_length(calls, 1L)
  expect_identical(calls[[1L]]$path, dir)
  expect_identical(calls[[1L]]$dirs, res$value)
  expect_contains(names(res$value), "blockr.core")
})

test_that("install_js_types() refuses what it cannot install", {

  dir <- withr::local_tempdir()

  expect_error(install_js_types(dir), class = "js_types_no_description")

  write_description(dir, "Imports: utils")

  expect_error(
    install_js_types(dir),
    "names, directly or through others, ships types.",
    class = "js_types_none"
  )

  write_description(dir, "Imports: blockr.core")
  local_mocked_bindings(npm_install_links = function(path, dirs) 1L)

  expect_error(install_js_types(dir), class = "js_types_npm_failed")
})

test_that("npm installs the types of all packages at once, as copies", {

  skip_on_cran()
  skip_on_os("windows")
  skip_if_not(nzchar(Sys.which("npm")), "npm is not installed")

  src <- withr::local_tempdir()
  dirs <- c(
    first = write_types(src, "first"),
    second = write_types(src, "second")
  )

  dir <- withr::local_tempdir()
  write_description(dir, "Imports: blockr.core")
  writeLines('{ "name": "blockr.test", "private": true }',
             file.path(dir, "package.json"))

  local_mocked_bindings(js_types_dirs = function(pkgs) dirs)

  expect_identical(install_js_types(dir), dirs)

  for (name in names(dirs)) {
    installed <- file.path(dir, "node_modules", "@blockr", name)
    expect_true(file.exists(file.path(installed, "package.json")))
    expect_identical(Sys.readlink(installed), "")
  }
})
