#' Random IDs
#'
#' Randomly generated unique IDs are used throughout the package, created by
#' `rand_names()`. If random strings are required that may not clash with a set
#' of existing values, this can be guaranteed by passing them as `old_names`.
#' The set of allowed characters can be controlled via `chars` and non-random
#' pre- and suffixes may be specified as `prefix`/`suffix` arguments, while
#' uniqueness is guaranteed including pre- and suffixes.
#'
#' @param old_names Disallowed IDs
#' @param n Number of IDs to generate
#' @param length ID length
#' @param chars Allowed characters
#' @param prefix,suffix ID pre-/suffix
#'
#' @examples
#' rand_names(chars = c(letters, LETTERS, 0:9))
#' rand_names(length = 5L)
#' rand_names(n = 5L, prefix = "pre-", suffix = "-suf")
#'
#' @return A character vector of length `n` where each entry contains `length`
#' characters (all among `chars` and start/end with `prefix`/`suffix`), is
#' guaranteed to be unique and not present among values passed as `old_names`.
#'
#' @export
rand_names <- function(old_names = character(0L), n = 1L, length = 15L,
                       chars = letters, prefix = "", suffix = "") {
  stopifnot(
    is.null(old_names) || is.character(old_names),
    is_count(n), is_count(length),
    is.character(chars), length(chars) >= 1L,
    is_string(prefix), is_string(suffix),
    nchar(prefix) + nchar(suffix) < length
  )

  length <- length - (nchar(prefix) + nchar(suffix))

  repeat {
    res <- replicate(
      n,
      paste0(
        prefix,
        paste(sample(chars, length, replace = TRUE), collapse = ""),
        suffix
      )
    )

    if (length(res) == length(unique(res)) && !any(res %in% old_names)) {
      break
    }
  }

  res
}

reval <- function(x) x()

reval_if <- function(x) if (is.function(x)) x() else x

lst_xtr_reval <- function(x, ...) {
  lapply(lst_xtr(x, ...), reval)
}

inherits <- function(x, ..., agg = NULL) {

  res <- lgl_ply(c(...), function(y) base::inherits(x, y))

  if (is.null(agg)) {
    return(res)
  }

  agg(res)
}

#' Miscellaneous utilities
#'
#' Several internal utility functions are exported for convencience in case
#' dependent packages can make use of this functionality.
#'
#' @param object,nm See [stats::setNames()]
#'
#' @return Function `set_names()` returns a names object, `paste_enum()` a
#' string, `coal()` the first non-null object and `unlst()` performs the same
#' action as [base::unlist()] but with differing defaults.
#'
#' @keywords internal
#'
#' @export
set_names <- function(object = nm, nm) {
  names(object) <- nm
  object
}

#' @param x Character vector to [base::paste()]
#' @param sep,conj Separation strings for all but last and last positions
#' @param quotes Quotes to wrap each entry in `x` with
#'
#' @rdname set_names
#' @export
paste_enum <- function(x, sep = ", ", conj = " and ", quotes = "`") {

  if (length(x) == 0L) {
    return(x)
  }

  if (length(x) == 1L) {
    return(paste0(quotes, x, quotes))
  }

  paste0(
    paste0(quotes, x[seq_len(length(x) - 1L)], quotes, collapse = sep),
    conj,
    quotes, x[length(x)], quotes
  )
}

#' @param ... Set of objects to iterate over
#' @param fail_null Error if no non-null objects are present
#'
#' @rdname set_names
#' @export
coal <- function(..., fail_null = TRUE) {

  for (i in seq_len(...length())) {
    x <- ...elt(i)
    if (is.null(x)) next else return(x)
  }

  if (isTRUE(fail_null)) {
    stop("No non-NULL value encountered")
  }

  NULL
}

int_to_chr <- function(x) {

  stopifnot(is_intish(x))

  chr_ply(
    lapply(strsplit(as.character(x), ""), as.integer),
    function(i) paste0(letters[i + 1L], collapse = "")
  )
}

#' @param recursive,use_names See [base::unlist()]
#' @rdname set_names
#' @export
unlst <- function(x, recursive = FALSE, use_names = FALSE) {
  unlist(x, recursive = recursive, use.names = use_names)
}

na_to_empty <- function(x) replace(x, is.na(x), "")

#' Blockr Options
#'
#' Retrieves options via [base::getOption()] or [base::Sys.getenv()], in that
#' order, and prefixes the option name passed as `name` with `blockr.` or
#' `blockr_` respectively. Additionally, the name is converted to lower case
#' for `getOption()` and upper case for environment variables. In case no value
#' is available for a given `name`, `default` is returned.
#'
#' @param name Option name
#' @param default Default value
#'
#' @examples
#' blockr_option("test-example", "default")
#'
#' options(`blockr.test-example` = "non-default")
#' blockr_option("test-example", "default")
#'
#' Sys.setenv(`BLOCKR_TEST-EXAMPLE` = "another value")
#' tryCatch(
#'   blockr_option("test-example", "default"),
#'   error = function(e) conditionMessage(e)
#' )
#' options(`blockr.test-example` = NULL)
#' blockr_option("test-example", "default")
#'
#' Sys.unsetenv("BLOCKR_TEST-EXAMPLE")
#' blockr_option("test-example", "default")
#'
#' @return The value set as option `name` or `default` if not set. In case of
#' the option being available only as environment variable, the value will be
#' a string and if available as [base::options()] entry it may be of any R type.
#'
#' @export
blockr_option <- function(name, default) {

  opt <- tolower(paste0("blockr.", name))
  env <- toupper(paste0("blockr_", name))

  res_opt <- getOption(opt, default = NULL)
  res_env <- Sys.getenv(env, unset = "")

  if (is.null(res_opt) && identical(res_env, "")) {
    return(default)
  }

  if (identical(res_opt, res_env)) {
    return(res_opt)
  }

  if (is.null(res_opt) && !identical(res_env, "")) {
    return(res_env)
  }

  if (identical(res_env, "") && !is.null(res_opt)) {
    return(res_opt)
  }

  stop("Conflicting options set for ", name, ": check environment variable `",
       env, "` and option `", opt, "`.")
}

dot_args_names <- function(x) {

  res <- names(x)

  unnamed <- grepl("^[1-9][0-9]*$", res)

  if (all(unnamed)) {
    return(NULL)
  }

  if (any(unnamed)) {
    return(replace(res, unnamed, ""))
  }

  res
}

dot_args_to_list <- function(x) {
  set_names(reactiveValuesToList(x), dot_args_names(x))
}

# https://github.com/rstudio/shiny/issues/3768
safely_export <- function(r) {
  r_quo <- rlang::enquo(r)
  rlang::inject({
    reactive({
      tryCatch(!!r_quo, error = function(e) e)
    })
  })
}

exprs_to_lang <- function(exprs) {

  if (rlang::is_syntactic_literal(exprs)) {
    return(exprs)
  }

  if (is.expression(exprs)) {
    exprs <- as.list(exprs)
  }

  if (is.list(exprs)) {
    exprs <- as.call(c(quote(`{`), exprs))
  }

  stopifnot(typeof(exprs) == "language")

  exprs
}

starts_with <- function(x, prefix) {
  x[startsWith(x, prefix)]
}

ansi_html <- function(x, ...) {

  if (requireNamespace("cli", quietly = TRUE)) {
    return(cli::ansi_html(x, ...))
  }

  x
}
