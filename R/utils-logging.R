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
#' @param ... Concatenated as `paste0(..., "\n")`
#' @param level Logging level (possible values are "fatal", "error", "warn",
#' "info", "debug" and "trace"
#' @param envir Environment where the logging call originated from
#' @param asis Flag to disable re-wrapping of text to terminal width
#' @param use_glue Flag to disable use of glue
#' @param pkg Package name
#'
#' @return Logging function `write_log()`, wrappers `log_*()` and loggers
#' provided as `cnd_logger()`/cat_logger() all return `NULL` invisibly and are
#' called for their side effect of emitting a message. Helpers `as_log_level()`
#' and `get_log_level()` return a scalar-valued ordered factor.
#'
#' @export
write_log <- function(..., level = "info", envir = parent.frame(),
                      asis = FALSE, use_glue = TRUE, pkg = pkg_name(envir)) {

  stopifnot(is.environment(envir), is_string(pkg))

  lvl <- as_log_level(level)

  if (lvl > get_log_level()) {
    return(invisible(NULL))
  }

  logger <- get_logger()

  pfx <- paste0(
    "[", toupper(level), "]",
    if (isTRUE(blockr_option("log_time", TRUE))) get_timmestamp("[", "]"),
    if (isTRUE(blockr_option("log_mem", FALSE))) get_mem_use("[", "]"),
    "[", pkg, "]",
    " "
  )

  if (isTRUE(use_glue)) {
    msg <- glue_plur(..., envir = envir, .trim = !asis)
  } else {
    msg <- paste0(...)
  }

  stopifnot(is_string(msg))

  msg <- strsplit(msg, "\n", fixed = TRUE)

  if (!isTRUE(asis)) {

    msg <- strwrap(
      msg[[1L]],
      width = getOption("width") - nchar(pfx),
      exdent = 2L
    )

  } else {
    msg <- msg[[1L]]
  }

  logger(paste0(pfx, msg, collapse = "\n"), level = lvl)
}

get_mem_use <- function(prefix = "", suffix = "") {

  mem <- memuse::Sys.procmem()

  if (is.null(mem$peak)) {
    peak <- ""
  } else {
    peak <- paste0("/", mem$peak)
  }

  paste0(prefix, mem$size, peak, suffix)
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
cnd_logger <- function(msg, level) {

  if (level == fatal_log_level) {
    stop(msg, call. = FALSE)
  }

  if (level <= warn_log_level) {
    warning(msg, call. = FALSE)
  } else {
    message(msg)
  }

  invisible()
}

#' @rdname write_log
#' @export
cat_logger <- function(msg, level) {

  if (level == fatal_log_level) {
    stop(msg, call. = FALSE)
  }

  if (level == error_log_level) {
    out <- stderr()
  } else {
    out <- stdout()
  }

  cat(paste0(msg, "\n"), file = out)

  invisible()
}
