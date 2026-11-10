# What the browser tests use, which drive Chrome through chromote, with the
# app in another R process: those of the Shiny transport
# (test-shiny-transport-browser.R) and those of the apps that serve() returns
# (test-serve-browser.R).

# Whether the app's port takes connections, as once the app has started.
port_open <- function(port) {
  tryCatch(
    {
      con <- socketConnection("127.0.0.1", port, open = "r+", timeout = 1)
      close(con)
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
}

# Evaluates JS on the page, awaiting a promise, and gives the value.
page_eval <- function(page, js, timeout = 30) {

  res <- page$Runtime$evaluate(
    js,
    awaitPromise = TRUE,
    returnByValue = TRUE,
    timeout_ = timeout
  )

  if (not_null(res$exceptionDetails)) {
    stop(res$exceptionDetails$exception$description)
  }

  res$result$value
}

# Whether the blockr.core installed, which the app runs, has the transport and
# the import map of its page, since a test run from the source would otherwise
# test an older install.
installed_with_transport <- function() {

  path <- find.package("blockr.core", lib.loc = .libPaths(), quiet = TRUE)

  if (!length(path)) {
    return(FALSE)
  }

  js <- file.path(path[[1L]], "js", "blockr-core.js")

  file.exists(js) &&
    file.exists(file.path(path[[1L]], "js", "import-map.json")) &&
    any(grepl("shinyTransport", readLines(js, warn = FALSE)))
}

# Starts the test app in another R process, on a port of its own, with the
# blockr option `message_body_encoding` set.
start_transport_app <- function(encoding = "base64", timeout = 30) {

  port <- httpuv::randomPort()
  log <- withr::local_tempfile(.local_envir = parent.frame(), fileext = ".log")

  dir <- normalizePath(testthat::test_path("apps", "shiny-transport"))

  proc <- callr::r_bg(
    function(dir, port, encoding) {
      options(blockr.message_body_encoding = encoding)
      shiny::runApp(dir, port, launch.browser = FALSE, host = "127.0.0.1")
    },
    args = list(dir = dir, port = port, encoding = encoding),
    stdout = log,
    stderr = "2>&1"
  )

  deadline <- Sys.time() + timeout

  while (!port_open(port)) {

    if (!proc$is_alive() || Sys.time() > deadline) {
      proc$kill()
      stop(
        "The transport's test app did not start:\n",
        paste(readLines(log, warn = FALSE), collapse = "\n")
      )
    }

    Sys.sleep(0.1)
  }

  list(
    url = paste0("http://127.0.0.1:", port, "/"),
    kill = function() proc$kill()
  )
}

# Opens a URL on the page and waits until the page's script has defined `app`.
# The load event alone does not show that:
# the wait for it ends at the page's next load event,
# whichever document fires it.
# So `navigate()` also asks the page for `app` until it has one.
navigate <- function(page, url, timeout = 30) {

  loaded <- page$Page$loadEventFired(wait_ = FALSE)
  page$Page$navigate(url, wait_ = FALSE)
  page$wait_for(loaded)

  deadline <- Sys.time() + timeout

  while (!page_eval(page, "window.app !== undefined")) {

    if (Sys.time() > deadline) {
      stop(
        "The page at ", page_eval(page, "location.href"),
        " defined no `app` within ", timeout, " seconds."
      )
    }

    Sys.sleep(0.1)
  }
}

# Whether the blockr.core installed, which the app runs, has the methods of
# serve() and the card of a block that serve() serves alone, since a test run
# from the source would otherwise test an older install.
installed_with_serve <- function() {

  path <- find.package("blockr.core", lib.loc = .libPaths(), quiet = TRUE)

  if (!length(path)) {
    return(FALSE)
  }

  ns <- file.path(path[[1L]], "NAMESPACE")
  js <- file.path(path[[1L]], "js", "blockr-core.js")

  file.exists(ns) && file.exists(js) &&
    any(grepl("S3method(serve,block)", readLines(ns), fixed = TRUE)) &&
    any(grepl("serves alone", readLines(js, warn = FALSE), fixed = TRUE))
}

# Starts the app that `call` returns, a call to serve(), in another R process
# with blockr.core attached, on a port of its own.
start_served_app <- function(call, timeout = 30) {

  port <- httpuv::randomPort()
  log <- withr::local_tempfile(.local_envir = parent.frame(), fileext = ".log")

  proc <- callr::r_bg(
    function(call, port) {
      library(blockr.core)
      shiny::runApp(
        eval(call),
        port = port,
        launch.browser = FALSE,
        host = "127.0.0.1"
      )
    },
    args = list(call = call, port = port),
    stdout = log,
    stderr = "2>&1"
  )

  withr::defer(proc$kill(), envir = parent.frame())

  deadline <- Sys.time() + timeout

  while (!port_open(port)) {

    if (!proc$is_alive() || Sys.time() > deadline) {
      proc$kill()
      stop(
        "The served app did not start:\n",
        paste(readLines(log, warn = FALSE), collapse = "\n")
      )
    }

    Sys.sleep(0.1)
  }

  paste0("http://127.0.0.1:", port, "/")
}

# Opens the URL on the page, and waits until the page's text holds each of the
# texts given, as once core's client has drawn the board or the block and R
# has evaluated it. It gives the page's text.
page_showing <- function(page, url, texts, timeout = 30) {

  loaded <- page$Page$loadEventFired(wait_ = FALSE)
  page$Page$navigate(url, wait_ = FALSE)
  page$wait_for(loaded)

  deadline <- Sys.time() + timeout

  repeat {

    shown <- page_eval(page, "document.body.innerText")

    if (all(vapply(texts, grepl, logical(1L), shown, fixed = TRUE))) {
      return(shown)
    }

    if (Sys.time() > deadline) {
      stop(
        "The page at ", url, " did not show ",
        paste(texts, collapse = ", "), " within ", timeout, " seconds, ",
        "but:\n", shown
      )
    }

    Sys.sleep(0.2)
  }
}
