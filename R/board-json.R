# R holds no board, and it loads no package for one. Installing and loading
# block packages is the deployment's job: the app's startup script fills the
# block registry, and the client counts a block whose class the registry lacks
# as missing. So R reads nothing of a JSON board but its format.

# The highest format of a JSON board that R reads, as `FORMAT` in
# srcts/store/open.ts is the client's.
board_json_format <- function() 1L

# Reads the JSON board in the file that `serve()` is given, as the app starts.
# R reads only its format, and refuses a JSON board whose format the client's
# store would refuse, such as a file of today's stack, which has no format. So
# the script fails as it runs, rather than the page as it starts. It returns
# the file's text as it is, as a string of class `json`, which R sends with its
# reply to every `hello`, for the client to open.
read_board_json <- function(path) {

  if (!is_string(path) || is.na(path)) {
    blockr_abort(
      "The path of a JSON board is expected to be a string.",
      class = "board_json_path_invalid"
    )
  }

  if (!file.exists(path) || dir.exists(path)) {
    blockr_abort(
      "There is no file {path}.",
      class = "board_json_missing"
    )
  }

  text <- readChar(path, file.size(path), useBytes = TRUE)
  Encoding(text) <- "UTF-8"

  if (!yyjsonr::validate_json_str(text)) {
    blockr_abort(
      "The file {path} holds no JSON.",
      class = "board_json_json_invalid"
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
      "The file {path} holds no JSON board.",
      class = "board_json_invalid"
    )
  }

  if (!"format" %in% names(file)) {
    blockr_abort(
      "The JSON board in {path} has no format. A file of today's stack has ",
      "none, and a conversion outside core turns it into a JSON board first.",
      class = "board_json_format_missing"
    )
  }

  version <- file[["format"]]

  if (!is_number(version) || inherits(version, "AsIs") ||
        version != trunc(version)) {
    blockr_abort(
      "The format of the JSON board in {path} is no whole number.",
      class = "board_json_format_invalid"
    )
  }

  if (version > board_json_format()) {
    blockr_abort(
      "The JSON board in {path} has format {version}, while R reads format ",
      "{board_json_format()} at most.",
      class = "board_json_format_unknown"
    )
  }

  structure(text, class = "json")
}
