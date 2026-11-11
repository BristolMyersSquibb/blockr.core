#' Logging
#'
#' Internally used infrastructure for emitting log messages is exported, hoping
#' that other packages which depend on this, use it and thereby logging is
#' carried out consistently both in terms of presentation and output device.
#' All log messages are associated with an (ordered) level ("fatal", "error",
#' "warn", "info", "debug" or "trace") which is compared against the currently
#' set value (available as `get_log_level()`) and output is only generated if
#' the message level is greater or equal to the currently set value.
#'
#' The blockr option `logger` picks the logger, the function that writes each
#' line out.
#' Where it names none, core picks `otel_logger()` where OpenTelemetry has set
#' up an exporter of logs, as the environment variable `OTEL_LOGS_EXPORTER`
#' does, and `cat_logger()` otherwise.
#' Where it names `otel_logger()` and OpenTelemetry has set up no exporter,
#' writing a line fails.
#' The function `write_log()` calls the logger with the message as written,
#' its level, its package, `asis` and `attributes`.
#' Both `cat_logger()` and `cnd_logger()` put the level, the time and the
#' package in front of the message, as the blockr options `log_time` and
#' `log_mem` set.
#' They wrap the message to the console's width unless `asis` is `TRUE`.
#' Then `cat_logger()` writes an error to standard error and any other line to
#' standard output.
#' The other, `cnd_logger()`, signals an error or a warning as a warning and
#' any other line as a message.
#' The logger `otel_logger()` writes each line as a record through
#' [otel::log()], with its level as the record's severity.
#' The record's attributes are the package, as `blockr.package.name`, and
#' `attributes`.
#' Only the level that `get_log_level()` returns drops a line: OpenTelemetry's
#' own level, as the environment variable `OTEL_LOG_LEVEL` sets it, drops none
#' of these records.
#' Every logger stops with an error at the level "fatal".
#'
#' Core works out the logger, the level and the blockr options `log_time` and
#' `log_mem` as it writes its first line, and keeps them for the process.
#' So does [serve()], afresh, as it starts an app.
#' After a change to one of these blockr options, as in a test, call
#' `reset_logging()`, so that core works them out again for the next line.
#'
#' @param ... Concatenated as `paste0(..., "\n")`
#' @param level Logging level (possible values are "fatal", "error", "warn",
#' "info", "debug" and "trace"
#' @param envir Environment where the logging call originated from
#' @param asis Flag to disable re-wrapping of text to terminal width
#' @param use_glue Flag to disable use of glue
#' @param pkg Package name
#' @param attributes Attributes of the line's record, as a named list, such as
#' `blockr.block.id`, which only `otel_logger()` writes
#'
#' @return Logging function `write_log()`, wrappers `log_*()` and loggers
#' provided as `cnd_logger()`, `cat_logger()` and `otel_logger()` all return
#' `NULL` invisibly and are called for their side effect of emitting a
#' message, as does `reset_logging()` for its side effect. Helpers
#' `as_log_level()` and `get_log_level()` return a scalar-valued ordered
#' factor.
#'
#' @export
write_log <- function(..., level = "info", envir = parent.frame(),
                      asis = FALSE, use_glue = TRUE, pkg = pkg_name(envir),
                      attributes = list()) {

  lvl <- as_log_level(level)

  if (lvl > get_log_level()) {
    return(invisible(NULL))
  }

  if (!is.environment(envir)) {
    blockr_abort(
      "Expecting `envir` to be an environment.",
      class = "write_log_envir_invalid"
    )
  }

  if (!(is.null(pkg) || is_string(pkg))) {
    blockr_abort(
      "Expecting `pkg` to be a string or `NULL`.",
      class = "write_log_pkg_invalid"
    )
  }

  if (!is.list(attributes) || !has_unique_names(attributes)) {
    blockr_abort(
      "Expecting `attributes` to be a named list.",
      class = "write_log_attributes_invalid"
    )
  }

  logger <- get_logger()

  if (isTRUE(use_glue)) {
    msg <- glue_plur(..., envir = envir, .trim = !asis)
  } else {
    msg <- paste0(...)
  }

  if (!is_string(msg)) {
    blockr_abort(
      "Expecting a log message to be a string.",
      class = "write_log_message_invalid"
    )
  }

  logger(msg, level = lvl, pkg = pkg, asis = asis, attributes = attributes)
}

# The line that `cat_logger()` and `cnd_logger()` write: the message behind a
# prefix of its level, the time and its package, as the blockr options
# `log_time` and `log_mem` set, and wrapped to the console's width unless
# `asis` is `TRUE`.
log_line <- function(msg, level, pkg, asis) {

  setup <- log_setup()

  pfx <- paste0(
    "[", toupper(level), "]",
    if (setup[["log_time"]]) get_timmestamp("[", "]"),
    if (setup[["log_mem"]]) get_mem_use("[", "]"),
    if (not_null(pkg)) paste0("[", pkg, "]"),
    " "
  )

  lines <- strsplit(msg, "\n", fixed = TRUE)[[1L]]

  if (!isTRUE(asis)) {
    lines <- strwrap(
      lines,
      width = getOption("width") - nchar(pfx),
      exdent = 2L
    )
  }

  paste0(pfx, lines, collapse = "\n")
}

get_mem_use <- function(prefix = "", suffix = "") {

  if (!has_memuse()) {
    return("")
  }

  mem <- memuse::Sys.procmem()

  if (is.null(mem$peak)) {
    peak <- ""
  } else {
    peak <- paste0("/", mem$peak)
  }

  paste0(prefix, mem$size, peak, suffix)
}

has_memuse <- function() {
  requireNamespace("memuse", quietly = TRUE)
}

get_timmestamp <- function(prefix = "", suffix = "") {
  paste0(prefix, format(Sys.time()), suffix)
}

#' @rdname write_log
#' @export
log_fatal <- function(..., envir = parent.frame()) {
  write_log(..., level = "fatal", envir = envir)
}

#' @rdname write_log
#' @export
log_error <- function(..., envir = parent.frame()) {
  write_log(..., level = "error", envir = envir)
}

#' @rdname write_log
#' @export
log_warn <- function(..., envir = parent.frame()) {
  write_log(..., level = "warn", envir = envir)
}

#' @rdname write_log
#' @export
log_info <- function(..., envir = parent.frame()) {
  write_log(..., level = "info", envir = envir)
}

#' @rdname write_log
#' @export
log_debug <- function(..., envir = parent.frame()) {
  write_log(..., level = "debug", envir = envir)
}

#' @rdname write_log
#' @export
log_trace <- function(..., envir = parent.frame()) {
  write_log(..., level = "trace", envir = envir)
}

#' @rdname write_log
#' @export
as_log_level <- function(level) {
  log_levels <- c("fatal", "error", "warn", "info", "debug", "trace")
  ordered(match.arg(level, log_levels), log_levels)
}

#' @rdname write_log
#' @export
fatal_log_level <- as_log_level("fatal")

#' @rdname write_log
#' @export
error_log_level <- as_log_level("error")

#' @rdname write_log
#' @export
warn_log_level <- as_log_level("warn")

#' @rdname write_log
#' @export
info_log_level <- as_log_level("info")

#' @rdname write_log
#' @export
debug_log_level <- as_log_level("debug")

#' @rdname write_log
#' @export
trace_log_level <- as_log_level("trace")

#' @rdname write_log
#' @export
get_log_level <- function() {
  log_setup()[["level"]]
}

get_logger <- function() {
  log_setup()[["logger"]]
}

#' @rdname write_log
#' @export
reset_logging <- function() {
  rm(list = ls(logging, all.names = TRUE), envir = logging)
  invisible()
}

# What core works out about logging as it writes its first line, and keeps
# until `reset_logging()` drops it: the logger, the level that
# `get_log_level()` returns, the blockr options `log_time` and `log_mem`, and
# core's logger in OpenTelemetry, where OpenTelemetry has set up an exporter.
logging <- new.env(parent = emptyenv())

log_setup <- function() {

  if (is.null(logging[["level"]])) {
    set_up_logging()
  }

  logging
}

# Core keeps nothing of a setup that fails, so that the next line fails as
# well, rather than writing with part of it.
set_up_logging <- function() {

  level <- as_log_level(blockr_option("log_level", "info"))

  log_time <- as.logical(blockr_option("log_time", TRUE))

  if (!is_bool(log_time)) {
    blockr_abort(
      "The blockr option `log_time` is TRUE or FALSE.",
      class = "log_time_invalid"
    )
  }

  log_mem <- as.logical(blockr_option("log_mem", FALSE))

  if (!is_bool(log_mem)) {
    blockr_abort(
      "The blockr option `log_mem` is TRUE or FALSE.",
      class = "log_mem_invalid"
    )
  }

  otel <- otel_core_logger()

  logging[["logger"]] <- pick_logger(otel)
  logging[["otel"]] <- otel
  logging[["log_time"]] <- log_time
  logging[["log_mem"]] <- log_mem
  logging[["level"]] <- level

  invisible()
}

# Core's logger in OpenTelemetry, where OpenTelemetry has set up an exporter of
# logs, as `OTEL_LOGS_EXPORTER` has it, and `NULL` otherwise.
# Its minimum severity is the lowest, so that only `get_log_level()` drops a
# line, whatever `OTEL_LOG_LEVEL` sets.
# Core asks the logger itself whether it is enabled, since
# `otel::is_logging_enabled()` fails for a logger that it is given in otel
# 0.2.0: it checks the logger for the class `otel_tracer`.
otel_core_logger <- function() {

  lgr <- otel::get_logger(minimum_severity = "trace")

  if (!lgr$is_enabled("trace")) {
    return(NULL)
  }

  lgr
}

# The logger that the blockr option `logger` names, or else OpenTelemetry's,
# where it has set up an exporter, and the console's otherwise.
pick_logger <- function(otel) {

  fun <- blockr_option("logger", NULL)

  if (is.null(fun)) {
    return(if (is.null(otel)) cat_logger else otel_logger)
  }

  if (!is.function(fun)) {
    fun <- get(fun, mode = "function")
  }

  if (identical(fun, otel_logger) && is.null(otel)) {
    blockr_abort(
      "The blockr option `logger` names `otel_logger()`, but OpenTelemetry ",
      "has set up no exporter of logs. The environment variable ",
      "`OTEL_LOGS_EXPORTER` sets one up, such as `otlp`, where otelsdk is ",
      "installed.",
      class = "otel_logger_unavailable"
    )
  }

  fun
}

#' @param msg Message (string)
#' @rdname write_log
#' @export
cnd_logger <- function(msg, level, pkg, asis, attributes) {

  line <- log_line(msg, level, pkg, asis)

  if (level == fatal_log_level) {
    stop(line, call. = FALSE)
  }

  if (level <= warn_log_level) {
    warning(line, call. = FALSE)
  } else {
    message(line)
  }

  invisible()
}

#' @rdname write_log
#' @export
cat_logger <- function(msg, level, pkg, asis, attributes) {

  line <- log_line(msg, level, pkg, asis)

  if (level == fatal_log_level) {
    stop(line, call. = FALSE)
  }

  if (level == error_log_level) {
    out <- stderr()
  } else {
    out <- stdout()
  }

  cat(paste0(line, "\n"), file = out)

  invisible()
}

#' @rdname write_log
#' @export
otel_logger <- function(msg, level, pkg, asis, attributes) {

  otel::log(
    msg,
    severity = as.character(level),
    logger = log_setup()[["otel"]],
    attributes = c(
      if (not_null(pkg)) list(blockr.package.name = pkg),
      attributes
    )
  )

  if (level == fatal_log_level) {
    stop(msg, call. = FALSE)
  }

  invisible()
}
