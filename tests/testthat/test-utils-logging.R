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

      with_mocked_bindings(
        {
          expect_output(log_info("abc"), "\\]\\[42B\\]\\[blockr\\.core\\] ")
          expect_output(log_info("abc"), "\\]\\[.+\\]\\[")
        },
        get_mem_use = function(prefix = "", suffix = "") {
          paste0(prefix, "42B", suffix)
        }
      )
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

test_that("a log message is a string, written from an environment", {

  withr::local_envvar(BLOCKR_LOG_LEVEL = "")
  withr::local_options(blockr.log_level = "info")

  expect_error(write_log("abc", envir = 1), class = "write_log_envir_invalid")
  expect_error(write_log("abc", pkg = 1), class = "write_log_pkg_invalid")
  expect_error(write_log(c("a", "b"), use_glue = FALSE),
               class = "write_log_message_invalid")
})

test_that("memory marker is omitted when memuse is unavailable", {

  expect_identical(
    with_mocked_bindings(
      get_mem_use("[", "]"),
      has_memuse = function() FALSE
    ),
    ""
  )
})
