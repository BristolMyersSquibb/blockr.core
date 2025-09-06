test_that("logging", {

  withr::local_envvar(
    BLOCKR_LOG_LEVEL = "",
    BLOCKR_LOG_MEM = "",
    BLOCKR_LOG_TIME = "",
    BLOCKR_LOGGER = ""
  )

  withr::with_options(
    list(blockr.log_level = "info"),
    {
      expect_identical(get_log_level(), info_log_level)
      expect_silent(log_debug("abc"))
      expect_output(log_warn("abc"), " abc$")
      expect_output(log_warn("abc"), "^\\[WARN\\]")
    }
  )

  withr::with_options(
    list(
      blockr.log_level = "info",
      blockr.log_mem = TRUE,
      blockr.log_time = TRUE
    ),
    {
      expect_true(blockr_option("log_time", FALSE))
      expect_true(blockr_option("log_mem", FALSE))
      expect_output(log_info("abc"), "\\]\\[.+B\\]\\[blockr\\.core\\] ")
      expect_output(log_info("abc"), "\\]\\[.+\\]\\[")
    }
  )

  withr::with_options(
    list(
      blockr.log_level = "info",
      blockr.logger = cnd_logger
    ),
    {
      expect_message(log_info("abc"))
      expect_warning(log_warn("abc"))
      expect_warning(log_error("abc"))
      expect_error(log_fatal("abc"))
    }
  )
})
