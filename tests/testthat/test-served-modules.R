served_copy <- function(prefix, ...) {
  file.path(dir_path(file.path(served_dir(), prefix)), ...)
}

test_that("R serves a package's file under the package's name", {

  dir <- withr::local_tempdir()
  write_declared_files(dir)
  local_empty_declarations()
  local_declared_package(dir)

  file <- file.path(dir, "blockr.test", "js", "grid.js")

  expect_identical(
    served_url(js_module("js/grid.js", package = "blockr.test")),
    paste0("blockr.test/js/grid.js?v=", file_hash(file))
  )

  # Shiny serves a directory of R's own under the package's name.
  expect_identical(
    served_path("blockr.test"),
    served_copy("blockr.test")
  )
  expect_identical(
    readLines(served_copy("blockr.test", "js", "grid.js")),
    readLines(file)
  )
})

test_that("R serves the files beside a package's module, each at its path", {

  dir <- withr::local_tempdir()
  write_declared_files(dir)
  local_empty_declarations()
  local_declared_package(dir)

  served_url(js_module("js/grid.js", package = "blockr.test"))

  # The bundle that the module imports by a relative URL, and every other file
  # of the module's directory, at the path that the browser resolves such a URL
  # to.
  pkg <- file.path(dir, "blockr.test", "js")

  for (file in c("grid-lib-493f0854.js", "grid.css", "picker.js")) {
    expect_identical(
      readLines(served_copy("blockr.test", "js", file)),
      readLines(file.path(pkg, file)),
      info = file
    )
  }

  # Nothing of a directory within it, or of the rest of the package.
  expect_false(file.exists(served_copy("blockr.test", "js", "vendor")))
  expect_false(file.exists(served_copy("blockr.test", "notes.txt")))
})

test_that("R copies the files beside a module again once the module changed", {

  dir <- withr::local_tempdir()
  write_declared_files(dir)
  local_empty_declarations()
  local_declared_package(dir)

  module <- js_module("js/picker.js", package = "blockr.test")
  served_url(module)

  # A new build of the package writes a bundle, and the module imports it by
  # the bundle's name.
  pkg <- file.path(dir, "blockr.test", "js")

  writeBin(
    charToRaw("export const lib = 2;\n"),
    file.path(pkg, "picker-lib-a49cd4c9.js")
  )
  writeBin(
    charToRaw("import { lib } from \"./picker-lib-a49cd4c9.js\";\n"),
    file.path(pkg, "picker.js")
  )

  served_url(module)

  expect_identical(
    readLines(served_copy("blockr.test", "js", "picker-lib-a49cd4c9.js")),
    "export const lib = 2;"
  )
})

test_that("a changed file gets a new URL, at which R serves the change", {

  dir <- withr::local_tempdir()
  write_declared_files(dir)
  local_empty_declarations()
  local_declared_package(dir)

  module <- js_module("js/picker.js", package = "blockr.test")
  before <- served_url(module)

  writeBin(
    charToRaw("export const api = 1; // changed\n"),
    file.path(dir, "blockr.test", "js", "picker.js")
  )

  after <- served_url(module)

  expect_false(identical(after, before))
  expect_identical(
    sub("\\?v=.*$", "", after),
    sub("\\?v=.*$", "", before)
  )
  expect_identical(
    readLines(served_copy("blockr.test", "js", "picker.js")),
    "export const api = 1; // changed"
  )
})

test_that("R serves a file from no package at a URL of its own", {

  dir <- withr::local_tempdir()
  write_declared_files(dir)
  local_empty_declarations()

  file <- file.path(dir, "app", "letters-block.js")
  hash <- file_hash(file)

  expect_identical(
    served_url(js_module(file, package = NULL)),
    paste0("blockr-modules/", hash, "/letters-block.js")
  )
  expect_identical(
    served_path("blockr-modules"),
    served_copy("blockr-modules")
  )

  # Nothing else of the script's directory is served, such as its secrets.
  expect_identical(
    list.files(served_copy("blockr-modules", hash)),
    "letters-block.js"
  )
  expect_length(
    list.files(served_copy("blockr-modules"), "secrets", recursive = TRUE),
    0L
  )

  # A file of the same name in another directory, with other content, has a
  # URL of its own.
  other <- file.path(dir, "other", "letters-block.js")
  dir.create(dirname(other))
  writeBin(charToRaw("export const api = 1; // other\n"), other)

  expect_identical(
    served_url(js_module(other, package = NULL)),
    paste0("blockr-modules/", file_hash(other), "/letters-block.js")
  )
})

test_that("a URL encodes what a file's path holds beyond letters and digits", {

  dir <- withr::local_tempdir()
  local_empty_declarations()

  file <- file.path(dir, "my block.js")
  writeBin(charToRaw("export const api = 1;\n"), file)

  expect_identical(
    served_url(js_module(file, package = NULL)),
    paste0("blockr-modules/", file_hash(file), "/my%20block.js")
  )
})

test_that("a module comes with the stylesheet beside it, where it has one", {

  dir <- withr::local_tempdir()
  write_declared_files(dir)
  local_empty_declarations()
  local_declared_package(dir)

  css <- file.path(dir, "blockr.test", "js", "grid.css")

  expect_identical(
    served_module(js_module("js/grid.js", package = "blockr.test")),
    list(
      module = served_url(js_module("js/grid.js", package = "blockr.test")),
      css = paste0("blockr.test/js/grid.css?v=", file_hash(css))
    )
  )
  expect_named(
    served_module(js_module("js/startup.js", package = "blockr.test")),
    "module"
  )
})

test_that("a module that R cannot find gets a URL that serves nothing", {

  dir <- withr::local_tempdir()
  write_declared_files(dir)
  local_empty_declarations()
  local_declared_package(dir)

  expect_output(
    res <- served_url(js_module("js/gone.js", package = "blockr.test")),
    "R finds no file js/gone.js in package blockr.test",
    width = 500L
  )
  expect_identical(res, "blockr.test/js/gone.js")
  expect_false(file.exists(served_copy("blockr.test", "js", "gone.js")))

  expect_output(
    res <- served_url(js_module(file.path(dir, "gone.js"), package = NULL)),
    "R finds no file .*gone.js, so",
    width = 500L
  )
  expect_identical(res, "blockr-modules/gone.js")
})

# Whether the blockr.core installed, which an app in another R process runs,
# serves modules, since a test run from the source would otherwise test an
# older install.
installed_serves_modules <- function() {
  callr::r(
    function() "served_url" %in% ls(asNamespace("blockr.core"), TRUE)
  )
}

test_that("a running app serves a module that R names to the client", {

  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if_not_installed("httpuv")
  skip_if_not(
    installed_serves_modules(),
    "The blockr.core installed serves no modules to test."
  )

  dir <- withr::local_tempdir()
  write_declared_files(dir)

  port <- httpuv::randomPort()
  log <- withr::local_tempfile(fileext = ".log")

  # The page names the URL of a module that a script defines, which R serves
  # from the moment it names it, as when it names it in its reply to `hello`
  # in a running session.
  proc <- callr::r_bg(
    function(dir, port) {
      file <- file.path(dir, "app", "letters-block.js")
      ui <- function(req) {
        shiny::tags$p(
          id = "url",
          blockr.core:::served_url(blockr.core::js_module(file, NULL))
        )
      }
      shiny::runApp(
        shiny::shinyApp(ui, function(input, output, session) NULL),
        port = port,
        launch.browser = FALSE,
        host = "127.0.0.1"
      )
    },
    args = list(dir = dir, port = port),
    stdout = log,
    stderr = "2>&1"
  )
  withr::defer(proc$kill())

  base <- paste0("http://127.0.0.1:", port, "/")

  get <- function(path) {
    con <- url(paste0(base, path))
    on.exit(close(con))
    readLines(con, warn = FALSE)
  }

  deadline <- Sys.time() + 30
  page <- NULL

  while (is.null(page)) {

    if (!proc$is_alive() || Sys.time() > deadline) {
      stop(
        "The app did not start:\n",
        paste(readLines(log, warn = FALSE), collapse = "\n")
      )
    }

    page <- tryCatch(suppressWarnings(get("")), error = function(e) NULL)

    if (is.null(page)) {
      Sys.sleep(0.1)
    }
  }

  page <- paste(page, collapse = "")
  path <- sub(".*<p id=\"url\">([^<]*)</p>.*", "\\1", page)

  expect_identical(
    path,
    paste0(
      "blockr-modules/",
      file_hash(file.path(dir, "app", "letters-block.js")),
      "/letters-block.js"
    )
  )
  expect_identical(get(path), "export const api = 1; // letters")

  # Nothing beside the module is served, under its URL or another.
  beside <- sub("letters-block.js", "secrets.R", path, fixed = TRUE)

  expect_error(suppressWarnings(get(beside)))
  expect_error(suppressWarnings(get("blockr-modules/secrets.R")))
})

test_that("the import map names the URL at which R serves each import", {

  local_served_dir()

  shipped <- function(pkg) {
    yyjsonr::read_json_file(
      system.file("js", "import-map.json", package = pkg)
    )[["imports"]]
  }

  tag <- import_map()

  expect_identical(tag$attribs$type, "importmap")

  map <- yyjsonr::read_json_str(as.character(tag$children[[1L]]))

  expect_named(map, "imports")
  # The entries of blockr.ui, React's entry points, its components and React
  # Aria's three packages, and then core's
  expect_named(
    map$imports,
    c(
      "react", "react/jsx-runtime", "react-dom", "react-dom/client",
      "@blockr/ui", "react-aria-components", "react-aria", "react-stately",
      "@blockr/core/react", "@blockr/core/binding", "@blockr/core/controls",
      "@blockr/core/components", "@blockr/core/otel", "@opentelemetry/api"
    )
  )

  for (pkg in c("blockr.ui", "blockr.core")) {
    for (name in names(shipped(pkg))) {
      file <- shipped(pkg)[[name]]
      expect_identical(
        map$imports[[name]],
        paste0(
          "./",
          served_url(js_module(file.path("js", file), package = pkg))
        )
      )
      expect_true(file.exists(served_copy(pkg, "js", file)))
    }
  }

  # Core's vendor file, which core's runtime imports, and the bundle of
  # OpenTelemetry's SDK, which core's OpenTelemetry module imports, by their
  # names, as files beside them.
  bundles <- list.files(
    system.file("js", package = "blockr.core"),
    "^(vendor|opentelemetry)-[0-9a-f]{8}\\.js$"
  )

  expect_length(bundles, 2L)
  expect_true(all(file.exists(served_copy("blockr.core", "js", bundles))))

  # React's file, which React's entry points import, beside them in blockr.ui.
  react <- list.files(
    system.file("js", package = "blockr.ui"),
    "^react-[0-9a-f]{8}\\.js$"
  )

  expect_length(react, 1L)
  expect_true(file.exists(served_copy("blockr.ui", "js", react)))
})
