test_that("logging", {

  withr::local_envvar(
    BLOCKR_LOG_LEVEL = "",
    BLOCKR_LOG_MEM = "",
    BLOCKR_LOG_TIME = "",
    BLOCKR_LOGGER = ""
  )
  local_logging()

  withr::with_options(
    list(blockr.log_level = "info"),
    {
      reset_logging()
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
      reset_logging()
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
      reset_logging()
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
  local_logging()

  expect_error(write_log("abc", envir = 1), class = "write_log_envir_invalid")
  expect_error(write_log("abc", pkg = 1), class = "write_log_pkg_invalid")
  expect_error(write_log(c("a", "b"), use_glue = FALSE),
               class = "write_log_message_invalid")
})

test_that("log_time and log_mem read a string as as.logical() does", {

  withr::local_envvar(BLOCKR_LOG_LEVEL = "", BLOCKR_LOGGER = "")
  withr::local_options(
    blockr.log_level = "info",
    blockr.log_time = NULL,
    blockr.log_mem = NULL
  )

  withr::local_envvar(BLOCKR_LOG_TIME = "FALSE", BLOCKR_LOG_MEM = "T")
  local_logging()

  with_mocked_bindings(
    expect_output(log_info("abc"), "^\\[INFO\\]\\[42B\\]"),
    get_mem_use = function(prefix = "", suffix = "") {
      paste0(prefix, "42B", suffix)
    }
  )

  withr::local_envvar(BLOCKR_LOG_TIME = "yes")
  reset_logging()
  expect_error(log_info("abc"), class = "log_time_invalid")

  withr::local_envvar(BLOCKR_LOG_TIME = "", BLOCKR_LOG_MEM = "1")
  reset_logging()
  expect_error(log_info("abc"), class = "log_mem_invalid")
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

test_that("a logger gets the message as written, with its level and package", {

  withr::local_envvar(BLOCKR_LOG_LEVEL = "", BLOCKR_LOGGER = "")

  got <- NULL

  withr::local_options(
    blockr.log_level = "info",
    blockr.logger = function(msg, level, pkg, asis, attributes) {
      got <<- list(
        msg = msg,
        level = level,
        pkg = pkg,
        asis = asis,
        attributes = attributes
      )
    }
  )
  local_logging()

  n <- 2L
  log_warn("{n} lines", attributes = list(blockr.block.id = "a"))

  expect_identical(
    got,
    list(
      msg = "2 lines",
      level = warn_log_level,
      pkg = "blockr.core",
      asis = FALSE,
      attributes = list(blockr.block.id = "a")
    )
  )

  write_log("a\nb", pkg = NULL, asis = TRUE)

  expect_identical(got$msg, "a\nb")
  expect_null(got$pkg)
  expect_true(got$asis)
  expect_identical(got$attributes, list())

  expect_error(
    write_log("abc", attributes = list(1)),
    class = "write_log_attributes_invalid"
  )
})

test_that("the console's logger puts the prefix on each line it wraps", {

  withr::local_envvar(BLOCKR_LOG_TIME = "", BLOCKR_LOG_MEM = "")
  withr::local_options(
    blockr.log_time = FALSE,
    blockr.log_mem = FALSE,
    width = 40L
  )
  local_logging()

  text <- "one two three four five six seven eight"

  expect_identical(
    capture.output(cat_logger(text, info_log_level, "pkg", FALSE, list())),
    c("[INFO][pkg] one two three four five six", "[INFO][pkg]   seven eight")
  )

  expect_identical(
    capture.output(cat_logger("a\nb", info_log_level, NULL, TRUE, list())),
    c("[INFO] a", "[INFO] b")
  )

  expect_message(
    cnd_logger(text, info_log_level, "pkg", FALSE, list()),
    "five six\n\\[INFO\\]\\[pkg\\]   seven eight"
  )
})

test_that("core keeps the logger and the level until reset_logging()", {

  withr::local_envvar(BLOCKR_LOG_LEVEL = "", BLOCKR_LOGGER = "")

  got <- character()

  withr::local_options(
    blockr.log_level = "info",
    blockr.logger = function(msg, level, pkg, asis, attributes) {
      got <<- c(got, msg)
    }
  )
  local_logging()

  log_info("first")

  withr::local_options(blockr.log_level = "error", blockr.logger = cnd_logger)

  expect_identical(get_log_level(), info_log_level)
  log_info("second")
  expect_identical(got, c("first", "second"))

  reset_logging()

  expect_identical(get_log_level(), error_log_level)
  expect_silent(log_info("third"))
  expect_warning(log_error("fourth"), "fourth")
  expect_identical(got, c("first", "second"))
})

test_that("core logs through OpenTelemetry where it has set up an exporter", {

  withr::local_envvar(
    BLOCKR_LOG_LEVEL = "",
    BLOCKR_LOG_MEM = "",
    BLOCKR_LOG_TIME = "",
    BLOCKR_LOGGER = "",
    OTEL_LOG_LEVEL = "warn"
  )

  withr::local_options(blockr.log_level = "info", blockr.log_time = FALSE)
  local_logging()

  # With no exporter set up, core writes to the console.
  expect_output(log_info("abc"), "^\\[INFO\\]\\[blockr\\.core\\] abc$")

  skip_if_not_installed("otelsdk")

  # Only the blockr option log_level drops a line, not OTEL_LOG_LEVEL.
  rec <- otelsdk::with_otel_record(
    {
      reset_logging()
      expect_silent(
        log_info("abc {{x}}", attributes = list(blockr.block.id = "a"))
      )
      expect_silent(log_debug("abc"))
      expect_error(log_fatal("abc"), "^abc$")
    },
    what = "logs"
  )

  reset_logging()

  logs <- rec[["logs"]]

  expect_length(logs, 2L)
  expect_identical(logs[[1L]][["body"]], "abc {x}")
  expect_identical(logs[[1L]][["severity_text"]], "INFO")
  expect_mapequal(
    unclass(logs[[1L]][["attributes"]]),
    list(blockr.package.name = "blockr.core", blockr.block.id = "a")
  )
  expect_identical(logs[[2L]][["severity_text"]], "FATAL")
})

test_that("a logger named otel_logger() fails where no exporter is set up", {

  withr::local_envvar(BLOCKR_LOG_LEVEL = "", BLOCKR_LOGGER = "")
  withr::local_options(blockr.log_level = "info", blockr.logger = "otel_logger")
  local_logging()

  expect_error(log_info("abc"), class = "otel_logger_unavailable")

  # Core keeps no logger that it failed to work out, so the next line fails
  # too.
  expect_error(log_info("abc"), class = "otel_logger_unavailable")
})
