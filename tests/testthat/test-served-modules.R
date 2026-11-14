hash_of <- function(file) {
  substr(unname(tools::md5sum(file)), 1L, 8L)
}

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
    paste0("blockr.test/js/grid.js?v=", hash_of(file))
  )

  # Shiny serves a directory of R's own under the package's name, which holds
  # the files that R names and nothing beside them.
  expect_identical(
    served_path("blockr.test"),
    served_copy("blockr.test")
  )
  expect_identical(
    readLines(served_copy("blockr.test", "js", "grid.js")),
    readLines(file)
  )
  expect_false(file.exists(served_copy("blockr.test", "js", "notes.txt")))
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
  hash <- hash_of(file)

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
    paste0("blockr-modules/", hash_of(other), "/letters-block.js")
  )
})

test_that("a URL encodes what a file's path holds beyond letters and digits", {

  dir <- withr::local_tempdir()
  local_empty_declarations()

  file <- file.path(dir, "my block.js")
  writeBin(charToRaw("export const api = 1;\n"), file)

  expect_identical(
    served_url(js_module(file, package = NULL)),
    paste0("blockr-modules/", hash_of(file), "/my%20block.js")
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
      css = paste0("blockr.test/js/grid.css?v=", hash_of(css))
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
      hash_of(file.path(dir, "app", "letters-block.js")),
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

  shipped <- yyjsonr::read_json_file(
    system.file("js", "import-map.json", package = "blockr.core")
  )

  tag <- import_map()

  expect_identical(tag$attribs$type, "importmap")

  map <- yyjsonr::read_json_str(as.character(tag$children[[1L]]))

  expect_named(map, "imports")
  expect_named(
    map$imports,
    c(
      "react", "react/jsx-runtime", "react-dom", "react-dom/client",
      "@blockr/core/react", "@blockr/core/binding", "@blockr/core/controls",
      "@blockr/core/otel"
    )
  )

  for (name in names(shipped$imports)) {
    file <- shipped$imports[[name]]
    expect_identical(
      map$imports[[name]],
      paste0(
        "./",
        served_url(js_module(file.path("js", file), package = "blockr.core"))
      )
    )
    expect_true(file.exists(served_copy("blockr.core", "js", file)))
  }

  # The bundle of React that the entry points of React import, by its name.
  expect_length(shipped$files, 1L)
  expect_true(file.exists(served_copy("blockr.core", "js", shipped$files)))
})
