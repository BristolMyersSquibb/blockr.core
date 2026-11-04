# R holds no board, and it loads no package for one. Installing and loading
# block packages is the deployment's job: the app's startup script fills the
# block registry, and the client counts a block whose class the registry lacks
# as missing. So R reads nothing of a saved file but its format.

# The highest format of a saved file that R reads, as `FORMAT` in
# srcts/store/open.ts is the client's.
saved_file_format <- function() 1L

# Reads the saved file that `serve()` is given, as the app starts. R reads only
# its format, and refuses a file whose format the client's store would refuse,
# such as a file of today's stack, which has no format. So the script fails as
# it runs, rather than the page as it starts. It returns the file's text as it
# is, as a string of class `json`, which R sends in the `file` of its reply to
# every `hello`, for the client to open.
read_saved_file <- function(path) {

  if (!is_string(path) || is.na(path)) {
    blockr_abort(
      "The path of a saved file is expected to be a string.",
      class = "saved_file_path_invalid"
    )
  }

  if (!file.exists(path) || dir.exists(path)) {
    blockr_abort(
      "There is no saved file {path}.",
      class = "saved_file_missing"
    )
  }

  text <- readChar(path, file.size(path), useBytes = TRUE)
  Encoding(text) <- "UTF-8"

  if (!yyjsonr::validate_json_str(text)) {
    blockr_abort(
      "The saved file {path} holds no JSON.",
      class = "saved_file_json_invalid"
    )
  }

  # Marks each array of length one, so that R refuses a format given as an
  # array of one number, as the client does.
  file <- yyjsonr::read_json_str(
    text,
    opts = json_read_opts(length1_array_asis = TRUE)
  )

  if (!is.list(file) || is.null(names(file))) {
    blockr_abort(
      "The saved file {path} holds no saved board.",
      class = "saved_file_invalid"
    )
  }

  if (!"format" %in% names(file)) {
    blockr_abort(
      "The saved file {path} has no format. A file of today's stack has none, ",
      "and a conversion outside core turns it into a saved file first.",
      class = "saved_file_format_missing"
    )
  }

  version <- file[["format"]]

  if (!is_number(version) || inherits(version, "AsIs") ||
        version != trunc(version)) {
    blockr_abort(
      "The format of the saved file {path} is no whole number.",
      class = "saved_file_format_invalid"
    )
  }

  if (version > saved_file_format()) {
    blockr_abort(
      "The saved file {path} has format {version}, while R reads format ",
      "{saved_file_format()} at most.",
      class = "saved_file_format_unknown"
    )
  }

  structure(text, class = "json")
}
