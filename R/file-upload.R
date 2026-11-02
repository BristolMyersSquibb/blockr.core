#' @include block-class.R output-text.R
NULL

# The directory where R keeps uploaded files, one directory per file content,
# in the session's temporary directory, which goes with the session's process.
upload_dir <- function() {
  file.path(tempdir(), "uploads")
}

keep_upload <- function(file) {

  path <- file[["datapath"]]

  if (!is_string(path) || is.na(path) || !file.exists(path)) {
    blockr_abort(
      "An upload expects the path of the file that R received as `datapath`.",
      class = "upload_file_invalid"
    )
  }

  name <- file[["name"]]

  if (!is_string(name) || is.na(name) || !nzchar(basename(name))) {
    name <- path
  }

  dir <- file.path(upload_dir(), rlang::hash_file(path))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  dest <- file.path(dir, basename(name))

  if (!file.copy(path, dest, overwrite = TRUE)) {
    blockr_abort(
      "Could not keep the uploaded file {basename(name)}.",
      class = "upload_file_invalid"
    )
  }

  dest
}

#' @rdname new_upload_block
#' @format NULL
#' @export
upload_block <- new_block_class(
  "upload_block",
  expr = function(file_path) bbquote(file.path(.(file_path))),
  ui = js_module("js/upload-block.js"),
  requests = list(upload = new_block_request(keep_upload, action = TRUE)),
  output = as_text_output
)

#' Upload block
#'
#' The upload block gives the path of a file that a user uploads, for a block
#' downstream to read, such as the CSV block. The block class `upload_block`
#' defines it, and its constructor `new_upload_block()` makes a block of the
#' class (see [new_block_class()]).
#'
#' @details
#' In order to make user data available to blockr, this block lets a user
#' upload a file. Given that data provided in this way are only available for
#' the life-time of the R session, exported code is not self-contained and a
#' script containing code from an upload block cannot be run in a new session.
#'
#' @section Block class:
#' The block has no data inputs. Its UI sends the file a user picks to R
#' through the action `upload`, which receives it in its argument `file` as a
#' list of the path of the temporary file R wrote, as `datapath`, and the
#' file's `name`, `type` and `size`. The action keeps the file in the R
#' session's temporary directory and answers with its path, which the UI sets
#' as the block's state arg `file_path`. That arg has no default, so a new
#' block is `invalid` until a file is uploaded, and the block expression is the
#' path, as `file.path("/tmp/Rtmp/uploads/3f9a2c/sales.csv")`. Its results
#' show as text, through [as_text_output()].
#'
#' @param file_path Path of the uploaded file
#' @inheritParams new_head_block
#'
#' @block data upload block
#' @blockDescr Upload data
#' @blockCategory input
#' @blockIcon upload
#' @blockGuidance Provides the path of a file that a user uploads in the running
#'   app, for a downstream parser such as a CSV block. The upload itself sets
#'   `file_path`. Uploaded data lives only for the session, so reproducible
#'   export is unavailable.
#' @blockKeywords upload, file, import, data, session
#'
#' @examples
#' new_upload_block()
#'
#' csv <- tempfile(fileext = ".csv")
#' write.csv(datasets::BOD, csv, row.names = FALSE)
#'
#' run_block_request(
#'   upload_block,
#'   "upload",
#'   args = list(file = list(datapath = csv, name = "bod.csv"))
#' )
#'
#' @return The block class `upload_block` is an object of class
#' `block_class`, and its constructor `new_upload_block()` returns a block of
#' that class.
#'
#' @export
new_upload_block <- block_constructor(upload_block)
