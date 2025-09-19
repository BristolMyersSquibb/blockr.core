test_that("llm model opt", {

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

  opt2 <- withr::with_options(
    list(blockr.chat_function = list(chat1 = chat_fun, chat2 = chat_fun)),
    new_llm_model_option()
  )

  expect_s3_class(board_option_ui(opt2, "chat"), "shiny.tag")

  sess <- MockShinySession$new()
  sess$setInputs(llm_model = "chat1")

  board_option_to_userdata(opt2, session = sess)

  expect_s3_class(board_option_server(opt2, session = sess), "Observer")

  res <- board_option_value(opt2)
  fun <- attr(res, "chat_name")

  expect_identical(fun, "chat1")
})
