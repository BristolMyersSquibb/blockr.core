test_that("opt utils", {

  opt <- new_board_option(
    "test",
    "test",
    ui = function(id) textInput(NS(id, "test"), "Test", "test"),
    ctor = "new_board_option",
    pkg = "blockr.core"
  )

  expect_true(is_board_option(opt))
  expect_identical(validate_board_option(opt), opt)
  expect_identical(as_board_option(opt), opt)

  expect_error(
    validate_board_option(list()),
    class = "board_option_inheritance_invalid"
  )

  expect_error(
    validate_board_option(structure(list(), id = 1L, class = "board_option")),
    class = "board_option_component_id_invalid"
  )

  expect_error(
    validate_board_option(
      structure(list(), id = "test", trigger = 1L, class = "board_option")
    ),
    class = "board_option_component_trigger_invalid"
  )

  expect_error(
    validate_board_option(
      structure(list(ui = "test"), id = "test", class = "board_option")
    ),
    class = "board_option_component_ui_invalid"
  )

  expect_error(
    validate_board_option(
      structure(
        list(ui = function(id) id, server = "test"),
        id = "test",
        class = "board_option"
      )
    ),
    class = "board_option_component_server_invalid"
  )

  expect_error(
    validate_board_option(
      structure(
        list(ui = function(id) id, server = function(id) id),
        id = "test",
        class = "board_option"
      )
    ),
    class = "board_option_component_server_invalid"
  )

  expect_error(
    validate_board_option(
      structure(
        list(
          ui = function(id) id,
          server = function(board, ..., session) { },
          transform = "test"
        ),
        id = "test",
        class = "board_option"
      )
    ),
    class = "board_option_component_transform_invalid"
  )

  expect_snapshot(opt)
})

test_that("a board option's label defaults to its ID in sentence case", {

  new_test_opt <- function(value = "test", ...) {
    new_board_option(
      "test_opt",
      value,
      ui = function(id) textInput(NS(id, "test_opt"), "Test", value),
      ...
    )
  }

  expect_identical(board_option_label(new_test_opt()), "Test opt")

  expect_identical(
    board_option_label(new_test_opt(label = "My label")),
    "My label"
  )

  expect_error(
    new_test_opt(label = 1L),
    class = "board_option_component_label_invalid"
  )
})

test_that("core board options label their input with their label", {

  chat_fun <- function(system_prompt = NULL, params = NULL) NULL

  withr::local_options(
    blockr.chat_function = list(chat1 = chat_fun, chat2 = chat_fun)
  )

  ctors <- c(
    "new_board_name_option", "new_n_rows_option", "new_page_size_option",
    "new_filter_rows_option", "new_thematic_option", "new_dark_mode_option",
    "new_show_conditions_option", "new_llm_model_option"
  )

  for (ctor in ctors) {

    opt <- do.call(ctor, list(label = "My label"))

    expect_identical(board_option_label(opt), "My label", info = ctor)

    ui <- with_mocked_bindings(
      board_option_ui(opt, "opt"),
      pkg_avail = function(...) TRUE
    )

    expect_match(as.character(ui), "My label", fixed = TRUE, info = ctor)
  }
})

test_that("board name opt", {

  expect_true(is_board_option(new_board_name_option()))

  expect_error(
    new_board_name_option(1L),
    class = "board_name_option_invalid"
  )
})

test_that("board n_rows opt", {

  expect_true(is_board_option(new_n_rows_option()))

  expect_error(
    new_n_rows_option(-1L),
    class = "board_options_n_rows_invalid"
  )
})

test_that("board page_size opt", {

  expect_true(is_board_option(new_page_size_option()))

  expect_error(
    new_page_size_option(-1L),
    class = "board_options_page_size_invalid"
  )
})

test_that("board filter_rows opt", {

  expect_true(is_board_option(new_filter_rows_option()))

  expect_error(
    new_filter_rows_option(-1L),
    class = "board_options_filter_rows_invalid"
  )
})

test_that("board thematic opt", {

  expect_true(is_board_option(new_thematic_option()))

  expect_error(
    new_thematic_option(-1L),
    class = "board_options_thematic_invalid"
  )

  expect_error(
    with_mocked_bindings(
      new_thematic_option(TRUE),
      pkg_avail = function(...) FALSE
    ),
    class = "thematic_not_installed"
  )
})

test_that("board dark_mode opt", {

  expect_true(is_board_option(new_dark_mode_option()))
  expect_true(is_board_option(new_dark_mode_option(TRUE)))
  expect_true(is_board_option(new_dark_mode_option(FALSE)))

  expect_error(
    new_dark_mode_option(-1L),
    class = "board_options_dark_mode_invalid"
  )

  opt <- new_dark_mode_option()

  expect_identical(board_option_label(opt), "Dark mode")

  # The box reports TRUE or FALSE, while stored values stay strings or NULL
  expect_identical(board_option_value(opt, TRUE), "dark")
  expect_identical(board_option_value(opt, FALSE), "light")
  expect_identical(board_option_value(opt, "light"), "light")
  expect_null(board_option_value(opt))
})

test_that("the switches and the dark mode toggle are Shiny's checkbox", {

  opts <- list(
    new_filter_rows_option(TRUE),
    new_thematic_option(),
    new_dark_mode_option("dark")
  )

  for (opt in opts) {

    id <- board_option_id(opt)

    ui <- with_mocked_bindings(
      board_option_ui(opt, "opt"),
      pkg_avail = function(...) TRUE
    )

    box <- htmltools::tagQuery(ui)$find("input")$selectedTags()

    expect_length(box, 1L)

    box <- box[[1L]]

    expect_identical(htmltools::tagGetAttribute(box, "type"), "checkbox")
    expect_identical(htmltools::tagGetAttribute(box, "id"), NS("opt", id))

    expect_identical(
      not_null(htmltools::tagGetAttribute(box, "checked")),
      !identical(id, "thematic"),
      info = id
    )

    deps <- chr_xtr(htmltools::findDependencies(ui), "name")

    expect_false(any(grepl("bslib", deps)), info = id)
  }
})

test_that("dark mode sets the scheme in the page's head", {

  dark_mode_ui <- function(value) {

    ui <- board_option_ui(new_dark_mode_option(value), "opt")

    list(
      head = htmltools::findDependencies(ui)[[1L]]$head,
      box = htmltools::tagQuery(ui)$find("input")$selectedTags()[[1L]]
    )
  }

  dark <- dark_mode_ui("dark")

  expect_match(dark$head, "\"data-bs-theme\", \"dark\")", fixed = TRUE)
  expect_null(htmltools::tagGetAttribute(dark$box, "data-follow-system"))

  unset <- dark_mode_ui(NULL)

  expect_match(unset$head, "prefers-color-scheme: dark", fixed = TRUE)
  expect_true(htmltools::tagHasAttribute(unset$box, "data-follow-system"))
})

test_that("board show_conditions opt", {

  expect_true(is_board_option(new_show_conditions_option()))

  expect_error(
    new_show_conditions_option(-1L),
    class = "board_options_show_conditions_invalid"
  )
})

test_that("llm model opt", {

  opt0 <- withr::with_options(
    list(blockr.chat_function = default_chat),
    new_llm_model_option()
  )

  expect_null(board_option_ui(opt0, "chat"))

  chat_fun <- function(system_prompt = NULL, params = NULL) {
    list(system_prompt = system_prompt, params = params)
  }

  opt1 <- withr::with_options(
    list(blockr.chat_function = chat_fun),
    new_llm_model_option()
  )

  expect_null(board_option_ui(opt1, "chat"))
  expect_null(board_option_server(opt1, board = "foo"))
  expect_null(board_option_trigger(opt1))

  expect_identical(board_option_value(opt1), chat_fun)

  withr::with_options(
    list(blockr.chat_function = chat_fun),
    expect_snapshot(opt1)
  )

  opt2 <- withr::with_options(
    list(blockr.chat_function = list(chat1 = chat_fun, chat2 = chat_fun)),
    new_llm_model_option()
  )

  expect_s3_class(board_option_ui(opt2, "chat"), "shiny.tag")

  withr::with_options(
    list(blockr.chat_function = list(chat1 = chat_fun, chat2 = chat_fun)),
    expect_snapshot(opt2)
  )

  sess <- MockShinySession$new()
  sess$setInputs(llm_model = "chat1")

  board_option_to_userdata(opt2, session = sess)

  expect_s3_class(board_option_server(opt2, session = sess), "Observer")

  res <- board_option_value(opt2)
  fun <- attr(res, "chat_name")

  expect_identical(fun, "chat1")

  opt3 <- withr::with_options(
    list(blockr.chat_function = list(chat1 = chat_fun, chat2 = chat_fun)),
    new_llm_model_option("chat2")
  )

  expect_true(is_board_option(opt3))

  opt4 <- withr::with_options(
    list(blockr.chat_function = list(chat1 = chat_fun)),
    new_llm_model_option("chat1")
  )

  expect_true(is_board_option(opt4))

  expect_error(
    withr::with_options(
      list(blockr.chat_function = identity),
      new_llm_model_option()
    ),
    class = "board_options_llm_model_invalid"
  )

  expect_error(
    withr::with_options(
      list(blockr.chat_function = "test"),
      new_llm_model_option()
    ),
    class = "board_options_llm_model_invalid"
  )
})

test_that("a board option has a compact str_value()", {

  opt <- new_board_name_option("My board")

  expect_identical(str_value(opt), "board_name: My board")
  expect_identical(str_value(opt), format(opt))

  expect_identical(capture.output(str(opt)), " board_name: My board")

  chat_fun <- function(system_prompt = NULL, params = NULL) NULL

  llm <- withr::with_options(
    list(blockr.chat_function = chat_fun),
    new_llm_model_option()
  )

  expect_identical(str_value(llm), "llm_model: Default chat function")
  expect_identical(str_value(llm), format(llm))
})

dark_mode_app <- function(name, ...) {

  app_path <- system.file("examples", "board", "dark", "app.R",
                          package = "blockr.core")

  try(
    shinytest2::AppDriver$new(
      app_path,
      name = name,
      seed = 42,
      load_timeout = 30 * 1000,
      ...
    )
  )
}

test_that("a dark board is dark from its first paint, with no bslib", {

  skip_on_cran()

  app <- dark_mode_app("dark", options = list(blockr.dark_mode = "dark"))

  testthat::skip_if(
    inherits(app, "try-error"),
    "Cannot start shinytest2 dark mode app."
  )

  on.exit(app$stop())

  scheme <- "document.documentElement.dataset.bsTheme"
  box <- "document.getElementById('my_board-dark_mode')"

  # No script of bslib's on the page to take a message or set the scheme
  expect_false(
    app$get_js("Array.from(document.scripts).some(s => /bslib/.test(s.src))")
  )

  # Dark as the body starts, so before any of it can be painted
  expect_identical(app$get_js("window.schemeAtBody"), "dark")
  expect_true(app$get_js(paste0(box, ".checked")))
  expect_identical(
    app$get_value(export = "my_board-plot_bg"),
    "rgb(16, 24, 32)"
  )

  # A click switches the page before any message from the server can arrive,
  # and the option follows
  expect_identical(app$get_js(paste0(box, ".click(); ", scheme)), "light")

  expect_identical(
    app$wait_for_value(export = "my_board-dark_mode", ignore = list("dark")),
    "light"
  )

  # Shiny reports the plot's new colours, which thematic reads
  expect_identical(
    app$wait_for_value(
      export = "my_board-plot_bg",
      ignore = list(NULL, "rgb(16, 24, 32)")
    ),
    "rgb(255, 255, 255)"
  )

  # A value set on the server reaches the box and the page
  app$set_inputs(`my_board-set_mode` = "dark", allow_no_input_binding_ = TRUE)
  app$wait_for_js(paste(scheme, "=== 'dark'"))

  expect_true(app$get_js(paste0(box, ".checked")))
  expect_identical(app$get_value(export = "my_board-dark_mode"), "dark")
})

test_that("an unset dark mode follows the system until a value is chosen", {

  skip_on_cran()

  app <- dark_mode_app("dark-unset")

  testthat::skip_if(
    inherits(app, "try-error"),
    "Cannot start shinytest2 dark mode app."
  )

  on.exit(app$stop())

  scheme <- "document.documentElement.dataset.bsTheme"
  box <- "document.getElementById('my_board-dark_mode').checked"

  chrome <- app$get_chromote_session()

  emulate <- function(mode) {
    chrome$Emulation$setEmulatedMedia(
      features = list(list(name = "prefers-color-scheme", value = mode))
    )
  }

  # Headless Chrome starts with a light system
  expect_identical(app$get_js("window.schemeAtBody"), "light")
  expect_false(app$get_js(box))
  expect_null(app$get_value(export = "my_board-dark_mode"))

  # The page and the box follow the system, and the option takes its value, as
  # it did with bslib's toggle
  emulate("dark")
  app$wait_for_js(paste(scheme, "=== 'dark'"))

  expect_true(app$get_js(box))
  expect_identical(app$wait_for_value(export = "my_board-dark_mode"), "dark")

  emulate("light")
  app$wait_for_js(paste(scheme, "=== 'light'"))

  expect_false(app$get_js(box))
  expect_identical(
    app$wait_for_value(export = "my_board-dark_mode", ignore = list("dark")),
    "light"
  )

  # A value chosen on the page holds while the system changes
  app$click(selector = "#my_board-dark_mode")

  expect_identical(
    app$wait_for_value(export = "my_board-dark_mode", ignore = list("light")),
    "dark"
  )

  app$run_js(
    paste(
      "window.changes = 0;",
      "window.matchMedia('(prefers-color-scheme: dark)')",
      ".addEventListener('change', () => window.changes++);"
    )
  )

  emulate("dark")
  app$wait_for_js("window.changes >= 1")
  emulate("light")
  app$wait_for_js("window.changes >= 2")

  expect_identical(app$get_js(scheme), "dark")
  expect_true(app$get_js(box))
  expect_identical(app$get_value(export = "my_board-dark_mode"), "dark")

  # Loaded with a dark system, the page is dark as the body starts, and Shiny
  # reads the box ticked
  emulate("dark")
  app$run_js("window.stale = true;")
  chrome$Page$reload()

  app$wait_for_js(
    "!window.stale && !!window.Shiny?.shinyapp?.isConnected()"
  )

  expect_identical(app$get_js("window.schemeAtBody"), "dark")
  expect_true(app$get_js(box))
  expect_true(app$get_js("Shiny.shinyapp.$initialInput['my_board-dark_mode']"))
})
