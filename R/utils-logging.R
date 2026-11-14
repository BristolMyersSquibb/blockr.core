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
#' The blockr option `logger` names the logger, what writes each line out,
#' by a string that core's client reads as well:
#'
#' * With `"console"`, R writes an error to standard error and any other line
#'   to standard output.
#'   The client writes each line to the browser's console.
#' * With `"otel"`, R writes each line as a record through [otel::log()],
#'   with its level as the record's severity.
#'   The record's attributes are the package, as `blockr.package.name`,
#'   and `attributes`.
#'   The client sends its lines as records to the OpenTelemetry collector
#'   whose URL the blockr option `otel_client_endpoint` gives,
#'   at its path `v1/logs`,
#'   as an SDK sends to `OTEL_EXPORTER_OTLP_ENDPOINT`.
#'   Every user's browser must reach that URL.
#' * With `"condition"`, R signals an error or a warning as a warning and any
#'   other line as a message, which a test catches as such.
#'   The client writes to the browser's console,
#'   since the browser has no conditions.
#'
#' For `"console"` and `"condition"`, R puts the level, the time and the
#' package in front of the message, as the blockr options `log_time` and
#' `log_mem` set.
#' It wraps the message to the console's width unless `asis` is `TRUE`.
#' Where `logger` is not set, R writes through OpenTelemetry where it has set
#' up an exporter of logs, as the environment variable `OTEL_LOGS_EXPORTER`
#' does, and to the console otherwise.
#' The client then sends its records to the collector where
#' `otel_client_endpoint` names one, and writes to the console otherwise.
#' Where `logger` is `"otel"` and OpenTelemetry has set up no exporter,
#' writing a line fails.
#' Where it is `"otel"` and `otel_client_endpoint` names no collector,
#' [serve()] stops before the app's first session.
#' Any other value of `logger` is an error, a function among them.
#' Only the level that `get_log_level()` returns drops a line: OpenTelemetry's
#' own level, as the environment variable `OTEL_LOG_LEVEL` sets it, drops none
#' of these records.
#' The client reads the blockr option `log_level` as well, and drops a line
#' below it in the browser.
#' Every logger stops with an error at the level "fatal".
#'
#' Core works out the logger, the level and the blockr options `log_time` and
#' `log_mem` each when it first needs them, and keeps them for the process.
#' As it starts an app, [serve()] drops what core keeps and works out the
#' logger afresh.
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
#' `blockr.block.id`, which only OpenTelemetry's logger writes
#'
#' @return Logging function `write_log()` and wrappers `log_*()` all return
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
format_log_line <- function(msg, level, pkg, asis) {

  pfx <- paste0(
    "[", toupper(level), "]",
    if (get_log_flag("log_time", TRUE)) get_timmestamp("[", "]"),
    if (get_log_flag("log_mem", FALSE)) get_mem_use("[", "]"),
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
  cached_log_setting(
    "level",
    function() as_log_level(blockr_option("log_level", "info"))
  )
}

get_logger <- function() {
  cached_log_setting("logger", resolve_logger)
}

#' @rdname write_log
#' @export
reset_logging <- function() {
  rm(list = ls(log_cache, all.names = TRUE), envir = log_cache)
  invisible()
}

# The log cache holds the settings that core works out about logging, until
# `reset_logging()` drops them: the logger, the level that `get_log_level()`
# returns, the blockr options `log_time` and `log_mem`, and core's logger in
# OpenTelemetry.
log_cache <- new.env(parent = emptyenv())

# The setting that `name` names in the log cache, which `compute` works out
# the first time core reads it.
# Core keeps no setting that fails to compute, so that it computes the
# setting again, and fails again, the next time.
cached_log_setting <- function(name, compute) {

  if (!exists(name, envir = log_cache, inherits = FALSE)) {
    log_cache[[name]] <- compute()
  }

  log_cache[[name]]
}

# The blockr option `log_time` or `log_mem`, which reads a string as
# `as.logical()` does.
get_log_flag <- function(name, default) {
  cached_log_setting(
    name,
    function() {

      val <- as.logical(blockr_option(name, default))

      if (!is_bool(val)) {
        blockr_abort(
          "The blockr option `{name}` is TRUE or FALSE.",
          class = paste0(name, "_invalid")
        )
      }

      val
    }
  )
}

# Core's logger in OpenTelemetry, where OpenTelemetry has set up an exporter of
# logs, as `OTEL_LOGS_EXPORTER` has it, and `NULL` otherwise.
get_otel_logger <- function() {
  cached_log_setting("otel_logger", make_otel_logger)
}

# Asks OpenTelemetry for core's logger, with the lowest minimum severity, so
# that only `get_log_level()` drops a line, whatever `OTEL_LOG_LEVEL` sets.
# Core asks the logger itself whether it is enabled, since
# `otel::is_logging_enabled()` fails for a logger that it is given in otel
# 0.2.0: it checks the logger for the class `otel_tracer`.
make_otel_logger <- function() {

  lgr <- otel::get_logger(minimum_severity = "trace")

  if (!lgr$is_enabled("trace")) {
    return(NULL)
  }

  lgr
}

# The logger that the blockr option `logger` names, or else OpenTelemetry's,
# where it has set up an exporter, and the console's otherwise.
resolve_logger <- function() {

  name <- blockr_option("logger", NULL)

  if (is.null(name)) {
    return(if (is.null(get_otel_logger())) cat_logger else otel_logger)
  }

  if (!is_string(name) || !name %in% c("console", "otel", "condition")) {
    blockr_abort(
      "The blockr option `logger` names a logger: \"console\", \"otel\" or ",
      "\"condition\".",
      class = "logger_invalid"
    )
  }

  if (identical(name, "otel") && is.null(get_otel_logger())) {
    blockr_abort(
      "The blockr option `logger` is \"otel\", but OpenTelemetry has set up ",
      "no exporter of logs. The environment variable `OTEL_LOGS_EXPORTER` ",
      "sets one up, such as `otlp`, where otelsdk is installed.",
      class = "otel_logger_unavailable"
    )
  }

  switch(
    name,
    console = cat_logger,
    otel = otel_logger,
    condition = cnd_logger
  )
}

# Stops where the blockr option `logger` is "otel" and the blockr option
# `otel_client_endpoint` names no collector, to which the client would send
# its log records, so that `serve()` stops before a page starts.
check_client_logger <- function() {

  name <- blockr_option("logger", NULL)
  endpoint <- blockr_option("otel_client_endpoint", NULL)

  if (identical(name, "otel") && is.null(endpoint)) {
    blockr_abort(
      "The blockr option `logger` is \"otel\", but the blockr option ",
      "`otel_client_endpoint` names no collector that the client sends its ",
      "log records to.",
      class = "otel_client_endpoint_missing"
    )
  }

  invisible()
}

# The logger of `"condition"`, which signals an error or a warning as a
# warning, and any other line as a message.
cnd_logger <- function(msg, level, pkg, asis, attributes) {

  line <- format_log_line(msg, level, pkg, asis)

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

# The logger of `"console"`, which writes an error to standard error and any
# other line to standard output.
cat_logger <- function(msg, level, pkg, asis, attributes) {

  line <- format_log_line(msg, level, pkg, asis)

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

# The logger of `"otel"`, which writes each line as a record through
# `otel::log()`.
otel_logger <- function(msg, level, pkg, asis, attributes) {

  otel::log(
    msg,
    severity = as.character(level),
    logger = get_otel_logger(),
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
