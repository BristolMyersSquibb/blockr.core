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
