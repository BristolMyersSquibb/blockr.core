# What the browser tests use, which drive Chrome through chromote, with the
# app in another R process: those of the Shiny transport
# (test-shiny-transport-browser.R) and those of the apps that serve() returns
# (test-serve-browser.R).
# The end-to-end tests (test-end-to-end.R) drive Chrome through shinytest2.
# Each starts an app that serve() returns and acts on its page as a user does.
# It reads what the app gives end-to-end tests in Shiny's test mode:
# each block's latest result, which R exports,
# and the board and each block's status, which the page gives through
# `blockrTest`.

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

# The lines of a file of the blockr.core installed, which a browser test's app
# runs, by the file's path in the install, or none where the install lacks it.
installed_lines <- function(...) {

  path <- find.package("blockr.core", lib.loc = .libPaths(), quiet = TRUE)

  if (!length(path)) {
    return(character())
  }

  file <- file.path(path[[1L]], ...)

  if (!file.exists(file)) {
    return(character())
  }

  readLines(file, warn = FALSE)
}

# Whether the file of the blockr.core installed, by its path in the install,
# holds the text given.
# A test checks the install for what it tests, since a test run from the
# source would otherwise test an older install.
installed_has <- function(text, ...) {
  any(grepl(text, installed_lines(...), fixed = TRUE))
}

# Whether the blockr.core installed, which the app runs, has the transport and
# the import map of its page.
installed_with_transport <- function() {
  length(installed_lines("js", "import-map.json")) > 0L &&
    installed_has("shinyTransport", "js", "blockr-core.js")
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
# serve() and the card of a block that serve() serves alone.
installed_with_serve <- function() {
  installed_has("S3method(serve,block)", "NAMESPACE") &&
    installed_has("serves alone", "js", "blockr-core.js")
}

# Whether the blockr.core installed, which the app runs, gives end-to-end tests
# what they read (test-end-to-end.R).
installed_with_blockr_test <- function() {
  installed_has("blockrTest", "js", "blockr-test.js")
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

# Starts the app that `call` returns, a call to serve(), with shinytest2,
# from an app script that attaches blockr.core, and stops it as the test ends.
# It waits until the client has opened the board.
# It then puts `e2e` on the page, the helpers through which a test acts on the
# page (`e2e_js` below).
local_app <- function(call, frame = parent.frame()) {

  testthat::skip_on_cran()
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not(
    installed_with_blockr_test(),
    "The blockr.core installed gives end-to-end tests nothing to read."
  )

  chrome <- try(chromote::default_chromote_object(), silent = TRUE)
  testthat::skip_if(inherits(chrome, "try-error"), "Chrome does not start.")

  dir <- withr::local_tempdir(.local_envir = frame)
  writeLines(c("library(blockr.core)", deparse(call)), file.path(dir, "app.R"))

  app <- shinytest2::AppDriver$new(
    dir,
    load_timeout = 30 * 1000,
    timeout = 30 * 1000
  )

  withr::defer(app$stop(), envir = frame)

  app$wait_for_js(
    "window.blockrTest !== undefined && blockrTest.board() !== null"
  )
  app$run_js(e2e_js)

  stopifnot(isTRUE(app$get_js("window.e2e !== undefined")))

  app
}

# The board that the client holds, as the page gives it.
app_board <- function(app) {
  app$get_js("blockrTest.board()")
}

# The IDs of the board's blocks, in the board's order.
app_blocks <- function(app) {
  names(app_board(app)$blocks)
}

# The status of each of the board's blocks, by block ID, as the page gives it.
app_statuses <- function(app) {
  app$get_js("blockrTest.statuses()")
}

# The revision of each block given, by block ID, as the page gives it.
app_revisions <- function(app, blocks) {
  int_xtr(app_statuses(app)[blocks], "revision")
}

# Each block's latest result, as R exports it.
app_results <- function(app) {
  app$get_values(export = "result")$export$result
}

# Waits until `check()` gives `TRUE`, and fails with what `what()` says once
# `timeout` seconds have passed.
wait_until <- function(check, what, timeout = 30) {

  deadline <- Sys.time() + timeout

  repeat {

    if (isTRUE(check())) {
      return(invisible())
    }

    if (Sys.time() > deadline) {
      stop("Waited ", timeout, " seconds for ", what(), call. = FALSE)
    }

    Sys.sleep(0.1)
  }
}

# Waits until each block named in `statuses` reads the status given, and R runs
# no evaluation of it.
# A block named in `after` waits for a revision above the one given too, as
# once an edit has reached it.
wait_for_statuses <- function(app, statuses, after = integer()) {

  got <- NULL

  settled <- function(id) {
    status <- got[[id]]
    not_null(status) && identical(status$status, statuses[[id]]) &&
      isFALSE(status$running) &&
      (!id %in% names(after) || status$revision > after[[id]])
  }

  wait_until(
    function() {
      got <<- app_statuses(app)
      all(lgl_ply(names(statuses), settled))
    },
    function() {
      seen <- chr_ply(
        names(statuses),
        function(id) {
          paste0(
            id, " ",
            coal(got[[id]]$status, "without status"),
            if (isTRUE(got[[id]]$running)) ", running",
            " at revision ", coal(got[[id]]$revision, NA)
          )
        }
      )
      paste0(
        "the statuses ", paste(statuses, collapse = ", "),
        if (length(after)) " after an edit",
        ", where the page gives ", paste(seen, collapse = ", "), "."
      )
    }
  )
}

# Waits until `check()`, a function of the board, gives `TRUE`.
wait_for_board <- function(app, check, what) {
  wait_until(
    function() check(app_board(app)),
    function() paste0(what, ".")
  )
}

# Has the page run the helper of `e2e` that `fun` names, with the arguments
# given, in the element given: the card of a block, the open dialog, the
# element that a CSS selector finds, such as a section of core's Edit board
# plugin, or the whole page.
# It fails with what the helper threw.
act <- function(app, fun, ..., block = NULL, dialog = FALSE, selector = NULL) {

  within <- if (not_null(block)) {
    paste0("await e2e.card(", js_value(block), ")")
  } else if (dialog) {
    "await e2e.dialog()"
  } else if (not_null(selector)) {
    paste0("await e2e.element(", js_value(selector), ")")
  } else {
    "document"
  }

  args <- c(chr_ply(list(...), js_value), within)

  res <- app$get_js(
    paste0(
      "(async () => {\n",
      "  try {\n",
      "    await e2e.", fun, "(", paste(args, collapse = ", "), ");\n",
      "    return true;\n",
      "  } catch (e) {\n",
      "    return String(e);\n",
      "  }\n",
      "})()"
    )
  )

  if (!isTRUE(res)) {
    stop(coal(res, paste0("e2e.", fun, "() failed on the page.")))
  }

  invisible(app)
}

js_value <- function(x) {
  unclass(json_str(x, auto_unbox = TRUE))
}

# The helpers through which a test acts on the page as a user does.
# Each finds what it acts on by what the user reads, such as a button's text
# or a field's label.
# Each waits for that to show, since a block's UI draws its choices only once
# R has answered the block's requests.
# A value goes into a field through the setter of the field's class, which
# React does not track as a value it set itself, and then the event that the
# browser fires, so that React hears it as it hears a user.
e2e_js <- "
window.e2e = {
  async until(find, what) {
    const deadline = Date.now() + 20000;
    for (;;) {
      const found = find();
      if (found) return found;
      if (Date.now() > deadline) throw new Error('Found no ' + what + '.');
      await new Promise((resolve) => setTimeout(resolve, 50));
    }
  },
  // The card of the block of the ID given, which shows the block's name.
  // Of the cards of one name, it is the one at the block's place among the
  // blocks of that name, as the basic frontend draws a board without stacks.
  card(id) {
    return e2e.until(() => {
      const blocks = blockrTest.board().blocks;
      const name = blocks[id]?.name;
      const ids = Object.keys(blocks).filter((b) => blocks[b].name === name);
      const cards = [...document.querySelectorAll('article')]
        .filter((card) => card.getAttribute('aria-label') === name);
      return cards[ids.indexOf(id)];
    }, 'card of ' + id);
  },
  dialog() {
    return e2e.until(() => document.querySelector('dialog[open]'), 'dialog');
  },
  element(selector) {
    return e2e.until(() => document.querySelector(selector), selector);
  },
  label(within, text) {
    return [...within.querySelectorAll('label')].find((label) =>
      label.textContent.trim() === text ||
        label.querySelector('span')?.textContent === text
    );
  },
  set(field, value, event) {
    Reflect.set(Object.getPrototypeOf(field), 'value', value, field);
    field.dispatchEvent(new Event(event, { bubbles: true }));
  },
  // Clicks the button whose label, text or first line is the one given, as
  // an entry of the block registry shows its name above its description.
  async click(text, within) {
    const button = await e2e.until(() =>
      [...within.querySelectorAll('button')].find((b) =>
        b.getAttribute('aria-label') === text ||
          b.textContent.trim() === text ||
          b.querySelector('span')?.textContent === text
      ), 'button ' + text);
    button.click();
  },
  async pick(label, value, within) {
    const select = await e2e.until(() => {
      const found = e2e.label(within, label)?.querySelector('select');
      const known = [...(found?.options ?? [])].some((o) => o.value === value);
      return known && found;
    }, 'choice ' + value + ' of ' + label);
    e2e.set(select, value, 'change');
  },
  // The field that the label of the text given names, through its for
  // attribute or by holding it.
  labelled(within, text) {
    const label = [...within.querySelectorAll('label')]
      .find((l) => l.textContent.trim() === text);
    return label &&
      (document.getElementById(label.htmlFor) || label.querySelector('input'));
  },
  // Types the text into the field of the label given.
  async fill(label, value, within) {
    const field = await e2e.until(
      () => e2e.labelled(within, label),
      'field ' + label
    );
    e2e.set(field, value, 'input');
  },
  // Picks the option of the label given in the searchable dropdown of the
  // label given, as a user types the search into its field and clicks the
  // option in the list that the search narrows.
  async choose(label, search, option, within) {
    const field = await e2e.until(
      () => e2e.labelled(within, label),
      'field ' + label
    );
    field.focus();
    field.click();
    e2e.set(field, search, 'input');
    const found = await e2e.until(() => {
      const list = document.getElementById(field.getAttribute('aria-controls'));
      return [...(list?.querySelectorAll('[role=option]') ?? [])].find((o) =>
        o.querySelector('span > span')?.textContent.replace(/^\u2713 /, '') ===
          option
      );
    }, 'option ' + option + ' of ' + label);
    found.click();
  },
  async check(label, within) {
    const box = await e2e.until(
      () => e2e.label(within, label)?.querySelector('input[type=checkbox]'),
      'checkbox ' + label
    );
    box.click();
  },
  // Types the name into the field of the label given and submits it with
  // Enter, once React has drawn what was typed.
  async rename(label, name, within) {
    const field = await e2e.until(
      () => [...within.querySelectorAll('input')]
        .find((input) => input.getAttribute('aria-label') === label),
      'field ' + label
    );
    e2e.set(field, name, 'input');
    await e2e.until(() => field.value === name, 'name ' + name);
    await new Promise((resolve) => setTimeout(resolve, 0));
    field.dispatchEvent(
      new KeyboardEvent('keydown', { key: 'Enter', bubbles: true })
    );
  },
  // Collapses or expands the group of the stack of the name given.
  async toggle(name, within) {
    const summary = await e2e.until(
      () => [...within.querySelectorAll('section')]
        .find((s) => s.getAttribute('aria-label') === name)
        ?.querySelector('summary'),
      'stack ' + name
    );
    summary.click();
  },
};
"
