# The apps that serve() returns, end to end: core's client on the page in
# Chrome, and R's evaluator in the app, which runs in another R process with
# the blockr.core installed, page and R alike.
# Each app that serves a board or a path names core's plugins in `plugins`.
# The mock of setup-listings.R does not reach the app's process.

test_that("serve() serves a board, a JSON board's path and a block alone", {

  skip_on_cran()
  skip_if_not_installed("chromote")
  skip_if_not_installed("callr")
  skip_if_not(
    installed_with_serve(),
    "The blockr.core installed has no serve() to test."
  )

  page <- try(chromote::ChromoteSession$new(), silent = TRUE)
  skip_if(inherits(page, "try-error"), "Chrome does not start.")
  withr::defer(page$close())

  # Core's basic frontend draws the board of an app whose plugins hold no
  # other frontend, and its toolbar saves and opens through core's storage
  # plugin.
  url <- start_served_app(
    quote(
      serve(
        new_board(
          c(a = new_dataset_block("BOD"), b = new_head_block(n = 3L)),
          list(from = "a", to = "b"),
          name = "Served board"
        ),
        plugins = subset(available_plugins(), package == "blockr.core")
      )
    )
  )

  page_showing(
    page,
    url,
    c("Served board", "<data.frame> [6 × 2]", "<data.frame> [3 × 2]")
  )

  toolbar <- paste(
    "[...document.querySelectorAll('[role=\"toolbar\"] button')]",
    ".map((b) => b.textContent).join(',')"
  )

  expect_match(page_eval(page, toolbar), "Save,Open")

  # An app that runs no storage plugin draws neither control.
  url <- start_served_app(
    quote(
      serve(
        new_board(name = "Unsaved board"),
        plugins = subset(
          available_plugins(),
          package == "blockr.core" & id != "file_storage_plugin"
        )
      )
    )
  )

  page_showing(page, url, "Unsaved board")

  expect_no_match(page_eval(page, toolbar), "Save|Open")

  # The page holds the import map ahead of its first module script.
  expect_true(
    page_eval(
      page,
      paste(
        "(() => {",
        "  const scripts = [...document.querySelectorAll('script')];",
        "  const map = scripts.findIndex((s) => s.type === 'importmap');",
        "  const module = scripts.findIndex((s) => s.type === 'module');",
        "  return map >= 0 && map < module;",
        "})()"
      )
    )
  )

  # The client opens the JSON board that R reads from the file, as it is.
  path <- withr::local_tempfile(fileext = ".json")
  board <- new_board(
    c(a = new_dataset_block("mtcars"), b = new_head_block(n = 4L)),
    list(from = "a", to = "b"),
    name = "Saved board"
  )
  writeLines(unclass(encode_message(blockr_ser(board))), path)

  url <- start_served_app(
    bquote(
      serve(
        .(path),
        plugins = subset(available_plugins(), package == "blockr.core")
      )
    )
  )

  page_showing(page, url, c("Saved board", "<data.frame> [4 × 11]"))

  # The card of a block that serve() serves alone shows its name, its UI, its
  # output and its summary, from the data that R binds to its input.
  url <- start_served_app(
    quote(serve(new_head_block(n = 2L), data = list(data = datasets::BOD)))
  )

  # The footer shows the summary as R replies to the evaluation, and the output
  # follows once R has answered the output's own request, so the page shows the
  # table's column demand only then.
  page_showing(page, url, c("Head", "<data.frame> [2 × 2]", "demand"))

  expect_identical(
    page_eval(page, "document.querySelector('h1').textContent"),
    "Head"
  )
  expect_identical(
    page_eval(page, "document.querySelector('footer').textContent"),
    "<data.frame> [2 × 2]"
  )
  expect_identical(
    page_eval(
      page,
      "document.querySelector('[aria-label=\"Output\"] table') !== null"
    ),
    TRUE
  )

  # A variadic block takes its data under the names given.
  url <- start_served_app(
    quote(serve(new_rbind_block(), a = datasets::BOD, b = datasets::BOD))
  )

  page_showing(page, url, "<data.frame> [12 × 2]")
})
