#' Include a pre-rendered mermaid diagram
#'
#' Renders mermaid `.mmd` source to `.svg` when the source is newer than the
#' existing SVG and `mmdc` is available, then includes the SVG via
#' [knitr::include_graphics()].
#'
#' @param name Diagram name (without extension), resolved relative to a
#'   `mermaid/` directory alongside the calling document.
#' @param chromium_args Character vector of extra Chromium flags passed to
#'   `mmdc` via a temporary puppeteer config file.
#'
#' @keywords internal
#'
#' @export
include_mermaid <- function(name,
                            chromium_args = c("--no-sandbox")) {

  mmd <- file.path("mermaid", paste0(name, ".mmd"))
  svg <- file.path("mermaid", paste0(name, ".svg"))

  needs_render <- !file.exists(svg) ||
    (file.exists(mmd) && file.mtime(mmd) > file.mtime(svg))

  if (needs_render && nzchar(Sys.which("mmdc"))) {
    cfg <- tempfile(fileext = ".json")
    on.exit(unlink(cfg))
    writeLines(
      sprintf('{"args":[%s]}', paste0('"', chromium_args, '"', collapse = ",")),
      cfg
    )
    system2(
      "mmdc",
      c("-i", mmd, "-o", svg, "--puppeteerConfigFile", cfg, "--quiet")
    )
  }

  knitr::include_graphics(svg)
}

#' Random IDs
#'
#' Random unique strings are created by `rand_names()`, which the package no
#' longer uses for the IDs of boards, blocks, links and stacks, since the client
#' mints those. If random strings are required that may not clash with a set
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

  if (!is.character(old_names)) {
    blockr_abort(
      "Expecting `old_names` to be a character vector.",
      class = "rand_names_old_names_invalid"
    )
  }

  if (!is_count(n)) {
    blockr_abort(
      "Expecting `n` to be a single whole number of 0 or more.",
      class = "rand_names_n_invalid"
    )
  }

  if (!is_count(max_tries)) {
    blockr_abort(
      "Expecting `max_tries` to be a single whole number of 0 or more.",
      class = "rand_names_max_tries_invalid"
    )
  }

  if (is.null(id_fun)) {
    if (pkg_avail("ids")) {
      id_fun <- adjective_animal
    } else {
      id_fun <- sample_letters
    }
  } else if (!is.function(id_fun) ||
               !identical(names(formals(id_fun)), "n")) {
    blockr_abort(
      "Expecting `id_fun` to be a function with the single argument `n`.",
      class = "rand_names_id_fun_invalid"
    )
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

  if (length(vals) != length(name) || !all(nzchar(name))) {
    blockr_abort(
      "Expecting blockr options to be set by name.",
      class = "blockr_options_names_invalid"
    )
  }

  dups <- unique(name[duplicated(name)])

  if (length(dups)) {
    blockr_abort(
      "Expecting each blockr option to be set once, but {dups} {?is/are} set ",
      "more than once.",
      class = "blockr_options_names_invalid"
    )
  }

  env <- toupper(paste0("blockr_", name))
  evl <- nzchar(Sys.getenv(env))

  if (any(evl)) {
    Sys.unsetenv(env[evl])
  }

  names(vals) <- tolower(paste0("blockr.", name))

  do.call(options, vals)
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

  if (typeof(exprs) != "language") {
    blockr_abort(
      "Expecting a call, a list of calls or an expression vector.",
      class = "exprs_to_lang_invalid"
    )
  }

  exprs
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

  if (!is.character(x) || !length(x)) {
    blockr_abort(
      "Expecting `x` to be a non-empty character vector.",
      class = "to_sentence_case_x_invalid"
    )
  }

  if (!is.character(replace)) {
    blockr_abort(
      "Expecting `replace` to be a character vector.",
      class = "to_sentence_case_replace_invalid"
    )
  }

  if (!is.character(with) || !len_wit %in% c(1L, len_rep)) {
    blockr_abort(
      "Expecting `with` to be a character vector of length one or as long as ",
      "`replace`.",
      class = "to_sentence_case_with_invalid"
    )
  }

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
