#' @include block-class.R
NULL

# The directory where R keeps uploaded files. By default it is in the session's
# temporary directory, which goes with the session's process, while a
# deployment that offers a durable path names it in the blockr option
# `upload_dir`.
upload_dir <- function() {
  blockr_option("upload_dir", file.path(tempdir(), "uploads"))
}

# A new folder with a random name in the upload directory, one per upload,
# joined with "/" on every platform
upload_folder <- function() {
  file.path(upload_dir(), basename(tempfile("")))
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

  dir <- upload_folder()
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
  expr = function(file_path) {
    if (missing(file_path) || !isTRUE(nzchar(file_path))) {
      blockr_invalid("Upload a file.")
    }
    call("file.path", file_path)
  },
  ui = js_module("js/upload-block.js"),
  requests = list(upload = new_block_request(keep_upload, action = TRUE))
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
#' upload a file. Unless the app keeps uploads in a durable directory, data
#' provided in this way are only available for the life-time of the R session,
#' so a script containing code from an upload block cannot be run in a new
#' session.
#'
#' @section Block class:
#' The block has no data inputs. Its UI sends the file a user picks to R
#' through the action `upload`, which receives it in its argument `file` as a
#' list of the path of the temporary file R wrote, as `datapath`, and the
#' file's `name`, `type` and `size`. The action copies the file into a new
#' folder with a random name, where it keeps its own name, and answers with
#' its path, which the UI sets as the block's state arg `file_path`. That arg
#' has no default, so a new block is `invalid` until a file is uploaded, and
#' the block expression is the path, as
#' `file.path("/tmp/Rtmp/uploads/3f9a2c/sales.csv")`. Its results show as
#' text, through [as_text_output()].
#'
#' @section Upload directory:
#' The folders go in the session's temporary directory, which R removes with
#' the session's process. A deployment that offers a durable path names it in
#' the blockr option `upload_dir`, through the R option `blockr.upload_dir` or
#' the environment variable `BLOCKR_UPLOAD_DIR`, so that a board saved with an
#' upload can read its file in a later session.
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
#'   `file_path`. Unless the app keeps uploads in a durable directory, uploaded
#'   data lives only for the session.
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
