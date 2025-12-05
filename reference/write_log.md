# Logging

Internally used infrastructure for emitting log messages is exported,
hoping that other packages which depend on this, use it and thereby
logging is carried out consistently both in terms of presentation and
output device. All log messages are associated with an (ordered) level
("fatal", "error", "warn", "info", "debug" or "trace") which is compared
against the currently set value (available as `get_log_level()`) and
output is only generated if the message level is greater or equal to the
currently set value.

## Usage

``` r
write_log(
  ...,
  level = "info",
  envir = parent.frame(),
  asis = FALSE,
  use_glue = TRUE,
  pkg = pkg_name(envir)
)

log_fatal(..., envir = parent.frame())

log_error(..., envir = parent.frame())

log_warn(..., envir = parent.frame())

log_info(..., envir = parent.frame())

log_debug(..., envir = parent.frame())

log_trace(..., envir = parent.frame())

as_log_level(level)

fatal_log_level

error_log_level

warn_log_level

info_log_level

debug_log_level

trace_log_level

get_log_level()

cnd_logger(msg, level)

cat_logger(msg, level)
```

## Format

An object of class `ordered` (inherits from `factor`) of length 1.

An object of class `ordered` (inherits from `factor`) of length 1.

An object of class `ordered` (inherits from `factor`) of length 1.

An object of class `ordered` (inherits from `factor`) of length 1.

An object of class `ordered` (inherits from `factor`) of length 1.

An object of class `ordered` (inherits from `factor`) of length 1.

## Arguments

- ...:

  Concatenated as `paste0(..., "\n")`

- level:

  Logging level (possible values are "fatal", "error", "warn", "info",
  "debug" and "trace"

- envir:

  Environment where the logging call originated from

- asis:

  Flag to disable re-wrapping of text to terminal width

- use_glue:

  Flag to disable use of glue

- pkg:

  Package name

- msg:

  Message (string)

## Value

Logging function `write_log()`, wrappers `log_*()` and loggers provided
as `cnd_logger()`/cat_logger() all return `NULL` invisibly and are
called for their side effect of emitting a message. Helpers
`as_log_level()` and `get_log_level()` return a scalar-valued ordered
factor.
