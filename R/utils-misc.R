#' Random IDs
#'
#' Randomly generated unique IDs are used throughout the package, created by
#' `rand_names()`. If random strings are required that may not clash with a set
#' of existing values, this can be guaranteed by passing them as `old_names`.
#' A [blockr_option()] `rand_id` can be set to swap out the function responsible
#' for ID generation.
#'
#' @param old_names Disallowed IDs
#' @param n Number of IDs to generate
#' @param max_tries Max number of attempts to create IDs that do not intersect
#' with `old_names`
#' @param id_fun A function with a single argument `n` that generates random
#' IDs. A value of `NULL` defaults to [ids::adjective_animal()] if available and
#' `sample_letters` otherwise.
#'
#' @examples
#' rand_names()
#' rand_names(n = 5L)
#' rand_names(id_fun = sample_letters)
#'
#' @return A character vector of length `n` where each entry contains `length`
#' characters (all among `chars` and start/end with `prefix`/`suffix`), is
#' guaranteed to be unique and not present among values passed as `old_names`.
#'
#' @export
rand_names <- function(old_names = character(0L), n = 1L, max_tries = 100L,
                       id_fun = blockr_option("rand_id", NULL)) {

  stopifnot(is.character(old_names), is_count(n), is_count(max_tries))

  if (is.null(id_fun)) {
    if (pkg_avail("ids")) {
      id_fun <- adjective_animal
    } else {
      id_fun <- sample_letters
    }
  } else {
    stopifnot(is.function(id_fun), identical(names(formals(id_fun)), "n"))
  }

  new_names <- character(0L)
  counter <- 0L

  while (length(new_names) < n && counter < max_tries) {

    counter <- counter + 1L

    candidates <- unique(id_fun(n - length(new_names)))

    collisions <- candidates %in% c(old_names, new_names)

    new_names <- c(new_names, candidates[!collisions])
  }

  if (length(new_names) < n) {
    blockr_abort(
      "Failed to create {n} unique ID{?s} within {max_tries} attempt{?s}.",
      class = "id_creation_unsuccessful"
    )
  }

  new_names
}

#' @rdname rand_names
#' @export
adjective_animal <- function(n) {
  ids::adjective_animal(n, max_len = 8L)
}

#' @rdname rand_names
#' @export
sample_letters <- function(n) {
  paste(sample(letters, 8, replace = TRUE), collapse = "")
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
#' Several internal utility functions are exported for convenience in case
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
#' @param fail_all Error if no non-null objects are present
#' @param test_fun Function to test each element with
#'
#' @rdname set_names
#' @export
coal <- function(..., fail_all = TRUE, test_fun = is.null) {

  for (i in seq_len(...length())) {
    x <- ...elt(i)
    if (test_fun(x)) next else return(x)
  }

  if (isTRUE(fail_all)) {
    blockr_abort(
      "No value encountered that does not fail `test_fun`.",
      class = "coal_null_return_disallowed"
    )
  }

  x
}

#' @rdname set_names
#' @export
reval <- function(x) x()

#' @rdname set_names
#' @export
reval_if <- function(x) if (is.function(x)) x() else x

lst_xtr_reval <- function(x, ...) {
  lapply(lst_xtr(x, ...), reval)
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

  blockr_abort(
    "Conflicting options set for {name}: check environment variable {env} ",
    "and option {opt}.",
    class = "conflicing_blockr_option"
  )
}

#' @param ... Option key value pairs as named arguments
#' @rdname blockr_option
#' @export
set_blockr_options <- function(...) {

  if (...length() == 0L) {
    return(invisible())
  }

  vals <- list(...)
  name <- names(vals)

  stopifnot(
    length(vals) == length(name),
    all(nzchar(name)),
    anyDuplicated(name) == 0L
  )

  env <- toupper(paste0("blockr_", name))
  evl <- nzchar(Sys.getenv(env))

  if (any(evl)) {
    Sys.unsetenv(env[evl])
  }

  names(vals) <- tolower(paste0("blockr.", name))

  do.call(options, vals)
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

#' @param ctor Function (either a string, a function or number used to index
#' the call stack
#' @param ctor_pkg The package where `ctor` is defined (either a string or
#' `NULL` which will use the function environment)
#'
#' @rdname rand_names
#' @export
resolve_ctor <- function(ctor, ctor_pkg = NULL) {

  try <- NULL

  if (is.numeric(ctor)) {

    if (ctor < 0L) {
      func <- rlang::caller_fn(-ctor)
      call <- as.character(rlang::caller_call(-ctor)[[1L]])
    } else {
      func <- sys.function(ctor)
      call <- deparse(sys.call(ctor)[[1L]])
    }

    if (any(grepl("::", call, fixed = TRUE))) {

      if (length(call) == 1L) {
        call <- strsplit(call, "::", fixed = TRUE)[[1L]]
      } else {
        stopifnot(length(call) == 3L)
        call <- call[-1L]
      }

      stopifnot(length(call) == 2L)

      ctor <- call[2L]

      if (is.null(ctor_pkg)) {
        ctor_pkg <- call[1L]
      } else {
        stopifnot(identical(ctor_pkg, call[1L]))
      }

    } else {

      if (is.null(ctor_pkg)) {
        ctor_pkg <- pkg_name(environment(func))
      }

      if (not_null(ctor_pkg)) {
        ctor <- call
      }
    }

    if (is_string(ctor_pkg) && is_string(ctor)) {
      try <- get0(ctor, asNamespace(ctor_pkg), mode = "function",
                  inherits = FALSE)
    }

    if (is.null(try)) {
      ctor <- func
      ctor_pkg <- NULL
    }
  }

  if (is.null(ctor_pkg)) {
    return(new_blockr_ctor(ctor))
  }

  new_blockr_ctor(try, ctor, ctor_pkg)
}

#' @rdname rand_names
#' @export
forward_ctor <- function(x) {

  x <- coal(x, 0L)

  if (is.numeric(x)) {

    stopifnot(is_scalar(x), is_intish(x))

    if (x > 0L) {
      x <- -x
    }

    x <- x - 1L
  }

  x
}

new_blockr_ctor <- function(fun, nme = NULL, pkg = NULL) {

  if (is.null(nme) && is.null(pkg)) {

    if (is.function(fun)) {
      return(structure(fun, class = "blockr_ctor"))
    }

    stopifnot(is_string(fun))

    fun <- strsplit(fun, "::", fixed = TRUE)[[1L]]

    stopifnot(length(fun) == 2L)

    nme <- fun[2L]
    pkg <- fun[1L]
    fun <- NULL
  }

  stopifnot(is_string(nme), is_string(pkg))

  structure(
    coal(fun, get_ctor_from_pkg(nme, pkg)),
    fun = nme,
    pkg = pkg,
    class = "blockr_ctor"
  )
}

get_ctor_from_pkg <- function(name, package) {
  get0(name, asNamespace(package), mode = "function", inherits = FALSE)
}

#' @rdname rand_names
#' @export
is_blockr_ctor <- function(x) {
  inherits(x, "blockr_ctor")
}

#' @rdname rand_names
#' @export
ctor_name <- function(x) {
  stopifnot(is_blockr_ctor(x))
  attr(x, "fun")
}

#' @rdname rand_names
#' @export
ctor_pkg <- function(x) {
  stopifnot(is_blockr_ctor(x))
  attr(x, "pkg")
}

#' @rdname rand_names
#' @export
ctor_fun <- function(x) {
  stopifnot(is_blockr_ctor(x))
  attributes(x) <- NULL
  x
}

#' @param x Character vector to transform
#' @param replace,with Mapped to [base::gsub()]
#' @rdname rand_names
#' @export
to_sentence_case <- function(x, replace = character(), with = character()) {

  to_case_collapse <- function(x) {

    if (length(x) == 0L) {
      return("")
    }

    first <- x[1L]
    first <- paste0(toupper(substr(first, 1, 1)), tolower(substring(first, 2)))

    if (length(x) == 1L) {
      return(first)
    }

    paste(first, paste0(tolower(x[-1L]), collapse = " "))
  }

  if (is.null(x)) {
    x <- ""
  }

  len_rep <- length(replace)
  len_wit <- length(with)

  stopifnot(
    is.character(x), is.character(replace), is.character(with),
    length(x) > 0, len_wit == length(replace) || len_wit == 1L
  )

  x <- replace(x, is.na(x), "")

  if (len_wit == 1L) {
    with <- rep(with, len_rep)
  }

  for (i in seq_len(len_rep)) {
    x <- gsub(replace[i], with[i], x)
  }

  chr_ply(strsplit(trimws(x), " ", fixed = TRUE), to_case_collapse)
}

#' @rdname rand_names
#' @export
id_to_sentence_case <- function(x) {
  to_sentence_case(x, c("([A-Z])", "_", "-", "\\."), c(" \\1", " ", " ", " "))
}

format_head <- function(x, n = 3L) {

  if (length(x) > 1) {

    res <- paste(chr_ply(utils::head(x, n = n), format), collapse = ", ")

    if (length(x) > n) {
      res <- paste0(res, ", ...")
    }

    return(res)
  }

  format(x)
}

get_attr <- function(i, x) attr(x, i, exact = TRUE)
