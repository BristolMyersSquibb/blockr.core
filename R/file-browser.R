#' @include block-class.R output-text.R
NULL

list_volume_dir <- function(volumes, volume = NULL, path = "") {

  if (is.null(volume)) {
    volume <- names(volumes)[1L]
  }

  if (!is_string(volume) || !volume %in% names(volumes)) {
    blockr_abort(
      "The file browser has no volume {volume}.",
      class = "filebrowser_volume_invalid"
    )
  }

  if (!is_string(path) || is.na(path)) {
    blockr_abort(
      "The file browser expects a path within a volume as a string.",
      class = "filebrowser_path_invalid"
    )
  }

  root <- normalizePath(volumes[[volume]], winslash = "/", mustWork = TRUE)
  dir <- normalizePath(file.path(root, path), winslash = "/", mustWork = TRUE)

  if (!identical(dir, root) && !startsWith(dir, sub("/?$", "/", root))) {
    blockr_abort(
      "The path {path} lies outside volume {volume}.",
      class = "filebrowser_path_invalid"
    )
  }

  entries <- list.files(dir)
  is_dir <- dir.exists(file.path(dir, entries))

  list(dirs = entries[is_dir], files = entries[!is_dir])
}

#' @rdname new_filebrowser_block
#' @format NULL
#' @export
filebrowser_block <- new_block_class(
  "filebrowser_block",
  expr = function(file_path, volumes = filebrowser_volumes()) {
    bbquote(file.path(.(file_path)))
  },
  ui = js_module("js/filebrowser-block.js"),
  requests = list(
    list_dir = new_block_request(list_volume_dir, volatile = TRUE)
  ),
  output = as_text_output
)

#' File browser block
#'
#' The file browser block gives the path of a file on the server's file system,
#' which a user picks, for a block downstream to read, such as the CSV block.
#' The block class `filebrowser_block` defines it, and its constructor
#' `new_filebrowser_block()` makes a block of the class (see
#' [new_block_class()]).
#'
#' @details
#' In order to make data on the server available to blockr, this block lets a
#' user browse the directories under a set of volumes and pick a file. Its
#' result is the path of that file, which a parser block, such as the CSV
#' block, reads.
#'
#' @section Block class:
#' The block has no data inputs. Its state arg `file_path` has no default, so a
#' new block leaves it unset and is `invalid` until something sets it, and the
#' block expression is that path, as `file.path("/data/sales.csv")`. Its state
#' arg `volumes` names the directories the browser starts from, a named
#' character vector, which the client sends back as a named list. Its UI lists
#' a directory through the volatile request `list_dir`, which reads `volumes`
#' and takes as parameters the name of a volume and a path within it, and
#' answers with the names of the directories and files there. Its results show
#' as text, through [as_text_output()].
#'
#' @param file_path File path
#' @param volumes Named character vector of the directories the browser starts
#'   from
#' @inheritParams new_head_block
#'
#' @block file browser block
#' @blockDescr Browse local files
#' @blockCategory input
#' @blockIcon folder2-open
#' @blockGuidance Selects a file from the server's file system for a downstream
#'   parser such as a CSV block. `file_path` is the chosen path; `volumes`
#'   bounds the root directories the browser may reach.
#' @blockKeywords file, browser, path, filesystem, select
#' @blockArg file_path Path of the selected file.
#'   [type] arg_string()
#'
#' @examples
#' vols <- c(pkg = system.file(package = "blockr.core"))
#'
#' new_filebrowser_block(volumes = vols)
#'
#' run_block_request(filebrowser_block, "list_dir", args = list(volumes = vols))
#'
#' @return The block class `filebrowser_block` is an object of class
#' `block_class`, and its constructor `new_filebrowser_block()` returns a block
#' of that class, while `filebrowser_volumes()` returns a named character
#' vector.
#'
#' @export
new_filebrowser_block <- block_constructor(filebrowser_block)

#' @param default Default volumes specification (use the blockr option
#' "volumes" to override)
#' @rdname new_filebrowser_block
#' @export
filebrowser_volumes <- function(default = c(home = path.expand("~"))) {

  res <- blockr_option("volumes", default)

  if (is_string(res) && grepl(":", res, fixed = TRUE)) {
    res <- strsplit(res, ":", fixed = TRUE)[[1L]]
  }

  if (is.null(names(res))) {
    if (length(res) == 1L) {
      names(res) <- "volume"
    } else if (length(res) > 1L) {
      names(res) <- paste0("volume", seq_along(res))
    }
  }

  stopifnot(
    is.character(res),
    length(res) >= 1L,
    length(unique(names(res))) == length(res)
  )

  res
}
