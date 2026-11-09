# The Shiny transport end to end: core's client on a page in Chrome over the
# transport, and R's half of it in a Shiny app, apps/shiny-transport/app.R,
# which runs in another R process with the blockr.core installed, page and R
# alike.

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

test_that("the transport carries messages and bodies between a page and R", {

  skip_on_cran()
  skip_if_not_installed("chromote")
  skip_if_not_installed("callr")
  skip_if_not(
    installed_with_transport(),
    "The blockr.core installed has no Shiny transport to test."
  )

  app <- start_transport_app()
  withr::defer(app$kill())

  page <- try(chromote::ChromoteSession$new(), silent = TRUE)
  skip_if(inherits(page, "try-error"), "Chrome does not start.")
  withr::defer(page$close())

  navigate(page, app$url)

  expect_identical(page_eval(page, "app.sessionsStarted(1)"), 1L)

  res <- page_eval(page, "app.ask('echo', { x: [1, 2], y: 'a' })")

  expect_identical(res$reply$status, "ok")
  expect_identical(res$reply$value, list(x = list(1L, 2L), y = list("a")))
  expect_null(res$body)

  res <- page_eval(page, "app.ask('progress')")

  expect_identical(res$notes, list(list(value = 0.5, message = "halfway")))
  expect_identical(res$reply$value, list(TRUE))

  # A body of up to 1 MiB travels in its reply's message, a larger one from a
  # URL, and each arrives as R sent it.
  for (size in c(1000, 2^20 + 1000)) {

    res <- page_eval(
      page,
      sprintf("app.ask('download', { size: %d, name: 'data.bin' })", size)
    )

    expect_identical(
      res$body,
      list(
        type = "application/octet-stream",
        name = "data.bin",
        size = as.integer(size),
        sum = sum(as.integer(as.raw(seq_len(size) %% 251L)))
      )
    )
    expect_identical(
      res$reply$body,
      list(type = "application/octet-stream", name = "data.bin")
    )
  }

  expect_identical(page_eval(page, "app.frames()"), 0L)

  res <- page_eval(page, "app.upload(1000, 'sales.csv')")

  expect_identical(
    res$reply$value,
    list(
      size = list(1000L),
      sum = list(sum(as.integer(as.raw(seq_len(1000L) %% 251L)))),
      match = list(TRUE),
      type = list("text/csv"),
      name = list("sales.csv")
    )
  )

  res <- page_eval(page, "app.upload(1200000)")

  expect_identical(res$reply$value$match, list(TRUE))
  expect_identical(res$reply$value$type, list("application/octet-stream"))
  expect_identical(res$reply$value$name, list())

  # A body over shiny.maxRequestSize, here 1.5 MiB, the client refuses itself.
  res <- page_eval(page, "app.upload(2000000)")

  expect_identical(res$error, "Error")
  expect_match(res$message, "larger than the 1572864 bytes that R takes")

  # The server ends the session, which loses what waited on it, and the page
  # starts a new one, which R does not end again with the message that Shiny
  # replays to it, and in which what the page asked meanwhile is answered.
  res <- page_eval(page, "app.ask('close')")

  expect_identical(res$error, "SessionEndedError")

  res <- page_eval(page, "app.ask('echo', { after: true })")

  expect_identical(res$reply$value, list(after = list(TRUE)))
  expect_identical(page_eval(page, "app.sessions.length"), 2L)
})

test_that("the transport carries a body in a binary frame where it is set to", {

  skip_on_cran()
  skip_if_not_installed("chromote")
  skip_if_not_installed("callr")
  skip_if_not(
    installed_with_transport(),
    "The blockr.core installed has no Shiny transport to test."
  )

  app <- start_transport_app("binary")
  withr::defer(app$kill())

  page <- try(chromote::ChromoteSession$new(), silent = TRUE)
  skip_if(inherits(page, "try-error"), "Chrome does not start.")
  withr::defer(page$close())

  navigate(page, app$url)

  expect_identical(page_eval(page, "app.sessionsStarted(1)"), 1L)

  # A body of up to 4 MiB travels in a binary frame, a larger one from a URL.
  for (size in c(1000, 4 * 2^20 + 1000)) {

    res <- page_eval(
      page,
      sprintf("app.ask('download', { size: %d, name: 'data.bin' })", size)
    )

    expect_identical(res$body$size, as.integer(size))
    expect_identical(
      res$body$sum,
      sum(as.integer(as.raw(seq_len(size) %% 251L)))
    )
    expect_identical(res$body$name, "data.bin")
  }

  expect_identical(page_eval(page, "app.frames()"), 1L)

  res <- page_eval(page, "app.ask('progress')")

  expect_identical(res$notes, list(list(value = 0.5, message = "halfway")))
})

test_that("the page's import map gives core's client and its SDK one React", {

  skip_on_cran()
  skip_if_not_installed("chromote")
  skip_if_not_installed("callr")
  skip_if_not(
    installed_with_transport(),
    "The blockr.core installed has no Shiny transport to test."
  )

  app <- start_transport_app()
  withr::defer(app$kill())

  page <- try(chromote::ChromoteSession$new(), silent = TRUE)
  skip_if(inherits(page, "try-error"), "Chrome does not start.")
  withr::defer(page$close())

  # The page has loaded core's client, which imports React and the SDK through
  # the import map, once it defines `app`.
  navigate(page, app$url)

  expect_identical(page_eval(page, "app.sdkInput()"), "hello")
})
