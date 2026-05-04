test_that("otel_lbl", {

  expect_identical(otel_lbl("setup_board"), "<blockr.core>setup_board")

  expect_identical(otel_lbl(paste0("link_", "abc")),
                   "<blockr.core>link_abc")

  expect_error(otel_lbl(""), class = "otel_lbl_name_invalid")
  expect_error(otel_lbl(NULL), class = "otel_lbl_name_invalid")
  expect_error(otel_lbl(NA_character_), class = "otel_lbl_name_invalid")
  expect_error(otel_lbl(c("a", "b")), class = "otel_lbl_name_invalid")
  expect_error(otel_lbl(1L), class = "otel_lbl_name_invalid")
})
