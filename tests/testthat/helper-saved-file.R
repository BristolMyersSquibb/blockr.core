# The path of a saved file that holds `text` as it is, which goes once the
# calling test ends.
local_saved_file <- function(text, frame = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".json", .local_envir = frame)
  writeBin(charToRaw(enc2utf8(text)), path)
  path
}
