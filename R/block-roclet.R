#' Block registration roclet
#'
#' A custom roxygen2 roclet that turns block registration metadata, declared as
#' tags on block constructors, into a YAML registry (`inst/registry/blocks.yml`)
#' that the package's `.onLoad()` reads into the `blocks` of its
#' `._BLOCKR_MODULES_` with [read_blocks_yml()], from which
#' [available_blocks()] lists the package's classes, as [serve()] starts an app
#' (see [register_block()]). Each entry gives the id of the class that its
#' constructor makes, which the roclet reads off the loaded package, since
#' nothing ties a class's id to its constructor's name (see
#' [new_block_class()]). It runs alongside the
#' standard roclets during [roxygen2::roxygenise()] (or `devtools::document()`)
#' for any package that lists `blockr.core::block_registration_roclet` in the
#' `Roxygen` field of its `DESCRIPTION`.
#'
#' @section Tags:
#' Placed in the roxygen block above a block constructor:
#' \describe{
#'   \item{`@block <name>`}{Human-readable block name. Required: its presence is
#'     what marks a constructor for registration.}
#'   \item{`@blockDescr <text>`}{Short block description. Required.}
#'   \item{`@blockCategory <category>`}{One of the [suggested_categories()].
#'     Required.}
#'   \item{`@blockIcon <icon>`}{Bootstrap icon name. Optional, defaulting to the
#'     category icon (see [default_icon()]).}
#'   \item{`@blockGuidance <text>`}{Model-facing construction guidance -- do and
#'     don't rules, enumerations, pitfalls. Optional.}
#'   \item{`@blockKeywords <terms>`}{Comma-separated search terms for block
#'     discovery; a term may contain spaces. Optional.}
#'   \item{`@blockLink <url>`}{URL of the block's help or documentation page.
#'     Optional; when omitted it is derived from the package's pkgdown `url`
#'     (`_pkgdown.yml` or `DESCRIPTION`) and the documented topic, as
#'     `<url>/reference/<topic>.html`.}
#'   \item{`@blockCtor <name>`}{Constructor name override. Optional: the
#'     documented object otherwise supplies the name.}
#' }
#'
#' No tag describes a state arg.
#' The schema of the block's state, which the class's UI module ships, gives
#' each state arg's type and description (see `vignette("create-block")`).
#'
#' @section The file:
#' The roclet writes `inst/registry/blocks.yml` as a mapping of each block
#' constructor, by name, to the registry metadata of the class that it makes:
#' the class's `id`, `name`, `description` and `category`, each a string, and,
#' where the tags give them, its `icon`, `link` and `guidance`, each a string,
#' and its `keywords`, a list of strings.
#' A package that writes the file by hand, or lists its block classes in its
#' `._BLOCKR_MODULES_` without one, gives the same fields.
#' R reads the listing as [available_blocks()] lists the package's classes and
#' as [serve()] registers them, and stops on an entry that lacks one of the
#' first four or gives it as anything but a string, with each such fault, while
#' it checks the others as [register_block()] does.
#'
#' @return `block_registration_roclet()` returns a roclet object.
#'
#' @name block_roclet
#' @export
block_registration_roclet <- function() {
  roxygen2::roclet("block_registration")
}

#' @exportS3Method roxygen2::roxy_tag_parse
roxy_tag_parse.roxy_tag_block <- function(x) {
  roxygen2::tag_value(x)
}

#' @exportS3Method roxygen2::roxy_tag_parse
roxy_tag_parse.roxy_tag_blockDescr <- function(x) {
  roxygen2::tag_value(x)
}

#' @exportS3Method roxygen2::roxy_tag_parse
roxy_tag_parse.roxy_tag_blockCategory <- function(x) {
  roxygen2::tag_value(x)
}

#' @exportS3Method roxygen2::roxy_tag_parse
roxy_tag_parse.roxy_tag_blockIcon <- function(x) {
  roxygen2::tag_value(x)
}

#' @exportS3Method roxygen2::roxy_tag_parse
roxy_tag_parse.roxy_tag_blockGuidance <- function(x) {
  roxygen2::tag_value(x, multiline = TRUE)
}

#' @exportS3Method roxygen2::roxy_tag_parse
roxy_tag_parse.roxy_tag_blockKeywords <- function(x) {
  roxygen2::tag_value(x)
}

#' @exportS3Method roxygen2::roxy_tag_parse
roxy_tag_parse.roxy_tag_blockLink <- function(x) {
  roxygen2::tag_value(x)
}

#' @exportS3Method roxygen2::roxy_tag_parse
roxy_tag_parse.roxy_tag_blockCtor <- function(x) {
  roxygen2::tag_value(x)
}

#' @exportS3Method roxygen2::roclet_process
roclet_process.roclet_block_registration <- function(x, blocks, env,
                                                     base_path) {
  base_url <- pkgdown_site_url(base_path)
  entries <- list()

  for (block in blocks) {

    if (is.null(roxygen2::block_get_tag(block, "block"))) {
      next
    }

    ctor <- block_roclet_ctor(block)
    entry <- block_roclet_entry(block, base_url)

    entries[[ctor]] <- c(list(id = block_roclet_id(block, ctor, env)), entry)
  }

  entries
}

#' @exportS3Method roxygen2::roclet_output
roclet_output.roclet_block_registration <- function(x, results, base_path,
                                                    ...) {
  if (!length(results)) {
    return(invisible(character()))
  }

  dir <- file.path(base_path, "inst", "registry")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  path <- file.path(dir, "blocks.yml")

  writeLines(
    c(block_roclet_header(), "", yaml::as.yaml(results)),
    path
  )

  invisible(path)
}

#' @exportS3Method roxygen2::roclet_clean
roclet_clean.roclet_block_registration <- function(x, base_path) {

  path <- file.path(base_path, "inst", "registry", "blocks.yml")

  generated <- file.exists(path) &&
    identical(readLines(path, n = 1L, warn = FALSE), block_roclet_header())

  if (generated) {
    unlink(path)
  }

  invisible()
}

block_roclet_header <- function() {
  "# Generated by blockr.core block_registration roclet: do not edit by hand"
}

block_roclet_ctor <- function(block) {

  ctor <- roxygen2::block_get_tag(block, "blockCtor")

  if (not_null(ctor)) {
    return(ctor$val)
  }

  name <- coal(block$object$alias, block$object$topic, fail_all = FALSE)

  if (is.null(name) && not_null(block$call)) {
    name <- as.character(block$call[[2L]])
  }

  if (!is_string(name)) {
    block_roclet_abort(
      roxygen2::block_get_tag(block, "block"),
      "Unable to determine the block constructor name."
    )
  }

  name
}

# The id of the block class that a documented constructor makes, read off the
# constructor in the loaded package, since nothing ties a class's id to its
# constructor's name.
block_roclet_id <- function(block, ctor, env) {

  fun <- get0(ctor, envir = env, inherits = FALSE)

  if (!inherits(fun, "block_constructor")) {
    block_roclet_abort(
      roxygen2::block_get_tag(block, "block"),
      paste0(
        "The object `", ctor, "` that `@block` documents is no block ",
        "constructor made by `block_constructor()`."
      )
    )
  }

  constructor_class(fun)[["id"]]
}

block_roclet_entry <- function(block, base_url) {

  require_block_tag(block, "blockDescr")
  require_block_tag(block, "blockCategory")

  entry <- list(
    name = roxygen2::block_get_tag_value(block, "block"),
    description = roxygen2::block_get_tag_value(block, "blockDescr"),
    category = roxygen2::block_get_tag_value(block, "blockCategory")
  )

  link <- block_link(block, base_url)

  if (not_null(link)) {
    entry[["link"]] <- link
  }

  icon <- roxygen2::block_get_tag_value(block, "blockIcon")

  if (not_null(icon)) {
    entry[["icon"]] <- icon
  }

  guidance <- block_guidance(block)

  if (not_null(guidance)) {
    entry[["guidance"]] <- guidance
  }

  keywords <- block_keywords(block)

  if (length(keywords)) {
    entry[["keywords"]] <- keywords
  }

  entry
}

require_block_tag <- function(block, tag) {

  if (is.null(roxygen2::block_get_tag(block, tag))) {
    block_roclet_abort(
      roxygen2::block_get_tag(block, "block"),
      paste0("A block declared with @block must also provide @", tag, ".")
    )
  }

  invisible()
}

block_keywords <- function(block) {

  value <- roxygen2::block_get_tag_value(block, "blockKeywords")

  if (is.null(value)) {
    return(character())
  }

  parts <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])

  parts[nzchar(parts)]
}

block_guidance <- function(block) {
  reflow_text(roxygen2::block_get_tag_value(block, "blockGuidance"))
}

block_link <- function(block, base_url) {

  explicit <- roxygen2::block_get_tag_value(block, "blockLink")

  if (not_null(explicit)) {
    return(explicit)
  }

  url <- derived_link(block, base_url)

  if (not_null(url) && url_exists(url)) {
    return(url)
  }

  NULL
}

derived_link <- function(block, base_url) {

  if (is.null(base_url)) {
    return(NULL)
  }

  topic <- coal(
    roxygen2::block_get_tag_value(block, "rdname"),
    roxygen2::block_get_tag_value(block, "name"),
    block_roclet_ctor(block),
    fail_all = FALSE
  )

  paste0(sub("/?$", "/", base_url), "reference/", topic, ".html")
}

url_exists <- function(url) {

  status <- tryCatch(
    attr(curlGetHeaders(url, timeout = 10L), "status"),
    error = function(e) NA_integer_
  )

  isTRUE(status >= 200L && status < 300L)
}

pkgdown_site_url <- function(base_path) {

  configs <- file.path(
    base_path,
    c("_pkgdown.yml", "pkgdown/_pkgdown.yml", "inst/_pkgdown.yml")
  )

  for (path in configs[file.exists(configs)]) {

    url <- yaml::read_yaml(path)[["url"]]

    if (is_string(url)) {
      return(url)
    }
  }

  desc <- file.path(base_path, "DESCRIPTION")

  if (!file.exists(desc)) {
    return(NULL)
  }

  url <- read.dcf(desc, "URL")[1L]

  if (is.na(url)) {
    return(NULL)
  }

  trimws(strsplit(url, "[,[:space:]]+")[[1L]][1L])
}

reflow_text <- function(text) {

  if (is.null(text)) {
    return(NULL)
  }

  gsub("\\s+", " ", trimws(text))
}

block_roclet_abort <- function(tag, message) {
  cli::cli_abort(
    paste0("{tag$file}:{tag$line}: ", message),
    class = "block_roclet_invalid"
  )
}
