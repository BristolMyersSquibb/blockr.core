test_that("a text output holds a string", {

  out <- new_text_output("Hello")

  expect_s3_class(out, c("text_output", "block_output"), exact = TRUE)
  expect_identical(out$text, "Hello")

  expect_identical(
    as.character(encode_message(shape_output(out)$output)),
    "{\"text\":\"Hello\"}"
  )

  expect_s3_class(new_text_output("Hi", class = "md_output"),
                  c("md_output", "text_output", "block_output"), exact = TRUE)

  expect_error(new_text_output(1), class = "text_output_invalid")
  expect_error(new_text_output(c("a", "b")), class = "text_output_invalid")
  expect_error(new_text_output(NA_character_), class = "text_output_invalid")
})

test_that("any result prints into a text output, at R's width", {

  withr::local_options(width = 40L)

  out <- as_text_output(1:20)

  expect_identical(
    out$text,
    paste(utils::capture.output(print(1:20)), collapse = "\n")
  )
  expect_length(strsplit(out$text, "\n")[[1L]], 2L)

  expect_identical(as_text_output(NULL)$text, "NULL")

  fit <- stats::lm(dist ~ speed, datasets::cars)

  expect_identical(
    as_text_output(fit, list(n_rows = 5L), list(width = 640), list())$text,
    paste(utils::capture.output(print(fit)), collapse = "\n")
  )
})
