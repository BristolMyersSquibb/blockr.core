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
#' Random unique IDs are drawn by `rand_names()`, in one of the shapes that the
#' blockr option `rand_id` names. The client of blockr.core mints IDs in the
#' same shapes, so that an ID looks the same whichever side drew it:
#'
#' * An adjective and an animal, as `calm_heron`, for `adjective_animal`, the
#'   default;
#' * Two different adjectives and an animal, as `calm_brave_heron`, for
#'   `adjective_adjective_animal`;
#' * That many lowercase letters, as `qkzvtmwa`, for `letters_6`, `letters_8`
#'   and `letters_12`;
#' * That many random bytes in hex, as `9f04a1c7`, for `bytes_4` and `bytes_8`;
#' * A version 4 UUID, as `f47ac10b-58cc-4372-a567-0e02b2c3d479`, for `uuid`.
#'
#' The words are familiar ones of at most six letters, picked from those of the
#' ids package, which blockr.core ships in `inst/ids`. An ID that may not clash
#' with a set of existing values is drawn by passing those as `old_names`. A
#' draw made of digits alone, which no ID may be, is drawn again. Where
#' `max_tries` draws in a row are all taken, the ID is a further draw with the
#' lowest number from 2 that makes it new, as `calm_heron_2`, as the client
#' counts up. The draws of `adjective_animal()` may repeat.
#'
#' @param old_names Disallowed IDs
#' @param n Number of IDs to draw
#' @param max_tries Number of draws for one ID before it is counted up
#' @param rand_id The shape of the IDs, as the blockr option `rand_id` names it
#'
#' @examples
#' rand_names()
#' rand_names(n = 5L)
#' rand_names(n = 2L, rand_id = "uuid")
#' adjective_animal(3L)
#'
#' @return A character vector of length `n`, whose entries are unique and none
#' of `old_names` for `rand_names()`, and draws that may repeat for
#' `adjective_animal()`.
#'
#' @export
rand_names <- function(old_names = character(0L), n = 1L, max_tries = 100L,
                       rand_id = blockr_option("rand_id", "adjective_animal")) {

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

  if (!is_string(rand_id) || !rand_id %in% rand_id_shapes()) {
    blockr_abort(
      "Expecting `rand_id` to be one of ",
      "{paste_enum(rand_id_shapes(), conj = ' or ')}.",
      class = "rand_names_rand_id_invalid"
    )
  }

  res <- character(0L)

  for (i in seq_len(n)) {
    res <- c(res, new_id(c(old_names, res), max_tries, rand_id))
  }

  res
}

#' @rdname rand_names
#' @export
adjective_animal <- function(n) {

  if (!is_count(n)) {
    blockr_abort(
      "Expecting `n` to be a single whole number of 0 or more.",
      class = "adjective_animal_n_invalid"
    )
  }

  words <- id_words()

  paste(
    sample(words[["adjectives"]], n, replace = TRUE),
    sample(words[["animals"]], n, replace = TRUE),
    sep = "_"
  )
}

# The shapes that the blockr option `rand_id` names, which the client's
# `RAND_IDS` in srcts/store/ids.ts gives as well.
rand_id_shapes <- function() {
  c(
    "adjective_animal",
    "adjective_adjective_animal",
    "letters_6",
    "letters_8",
    "letters_12",
    "bytes_4",
    "bytes_8",
    "uuid"
  )
}

# A new ID that `taken` does not hold, drawn as the client's mintId() in
# srcts/store/ids.ts draws one.
new_id <- function(taken, max_tries, rand_id) {

  for (attempt in seq_len(max_tries)) {

    id <- draw_id(rand_id)

    if (!grepl("^[0-9]+$", id) && !id %in% taken) {
      return(id)
    }
  }

  id <- draw_id(rand_id)
  count <- 2L

  while (paste0(id, "_", count) %in% taken) {
    count <- count + 1L
  }

  paste0(id, "_", count)
}

# One draw in the shape that `rand_id` names, which may be taken.
draw_id <- function(rand_id) {
  switch(
    rand_id,
    adjective_animal = adjective_animal(1L),
    adjective_adjective_animal = paste(
      c(
        sample(id_words()[["adjectives"]], 2L),
        sample(id_words()[["animals"]], 1L)
      ),
      collapse = "_"
    ),
    letters_6 = random_letters(6L),
    letters_8 = random_letters(8L),
    letters_12 = random_letters(12L),
    bytes_4 = random_hex(4L),
    bytes_8 = random_hex(8L),
    uuid = random_uuid()
  )
}

random_letters <- function(n) {
  paste(sample(letters, n, replace = TRUE), collapse = "")
}

random_bytes <- function(n) {
  sample.int(256L, n, replace = TRUE) - 1L
}

random_hex <- function(n) {
  paste(sprintf("%02x", random_bytes(n)), collapse = "")
}

# A version 4 UUID: 16 random bytes, of which the seventh holds the version, 4,
# and the ninth the variant, in hex, grouped by hyphens as 8-4-4-4-12.
random_uuid <- function() {

  bytes <- random_bytes(16L)
  bytes[7L] <- bitwOr(bitwAnd(bytes[7L], 15L), 64L)
  bytes[9L] <- bitwOr(bitwAnd(bytes[9L], 63L), 128L)

  hex <- sprintf("%02x", bytes)
  groups <- list(1:4, 5:6, 7:8, 9:10, 11:16)

  paste(
    chr_ply(groups, function(i) paste(hex[i], collapse = "")),
    collapse = "-"
  )
}

# The words that IDs are made of, which the build writes into inst/ids from
# srcts/store/words.ts, read once per process.
id_word_cache <- new.env(parent = emptyenv())

id_words <- function() {

  if (is.null(id_word_cache[["words"]])) {
    id_word_cache[["words"]] <- list(
      adjectives = read_id_words("adjectives"),
      animals = read_id_words("animals")
    )
  }

  id_word_cache[["words"]]
}

read_id_words <- function(kind) {
  readLines(
    system.file("ids", paste0(kind, ".txt"), package = "blockr.core"),
    warn = FALSE
  )
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

# The name that an object of class vector `class` takes where it is given
# none: the name of its own class without `suffix`, its underscores as spaces
# and its first letter upper case, as "Dataset" for "dataset_block".
default_name <- function(class, suffix) {
  res <- gsub("_", " ", sub(paste0(suffix, "$"), "", class[1L]))
  paste0(toupper(substr(res, 1L, 1L)), substring(res, 2L))
}

# The values of a classed list by name, without its class and its other
# attributes, as the state of a block or of a plugin.
bare_list <- function(x) {

  res <- unclass(x)
  attributes(res) <- NULL
  names(res) <- names(x)

  res
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
