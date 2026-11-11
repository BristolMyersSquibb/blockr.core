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
#' line out, and is `cat_logger()` unless it names another.
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
#' The logger `otel_logger()` writes each line through [otel::log()] instead,
#' with its level as the record's severity.
#' The record's attributes are the package, as `blockr.package.name`, and
#' `attributes`.
#' Where no OpenTelemetry SDK exports logs of that level, `otel_logger()`
#' writes as `cat_logger()` does, so that no line is lost.
#' Every logger stops with an error at the level "fatal".
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
#' message. Helpers `as_log_level()` and `get_log_level()` return a
#' scalar-valued ordered factor.
#'
#' @export
write_log <- function(..., level = "info", envir = parent.frame(),
                      asis = FALSE, use_glue = TRUE, pkg = pkg_name(envir),
                      attributes = list()) {

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

  lvl <- as_log_level(level)

  if (lvl > get_log_level()) {
    return(invisible(NULL))
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

  pfx <- paste0(
    "[", toupper(level), "]",
    if (log_time) get_timmestamp("[", "]"),
    if (log_mem) get_mem_use("[", "]"),
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
  as_log_level(
    blockr_option("log_level", "info")
  )
}

get_logger <- function() {

  fun <- blockr_option("logger", "cat_logger")

  if (is.function(fun)) {
    return(fun)
  }

  get(fun, mode = "function")
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

  severity <- as.character(level)

  if (!otel::is_logging_enabled(severity)) {
    return(cat_logger(msg, level, pkg, asis, attributes))
  }

  otel::log(
    msg,
    severity = severity,
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
