# Has core work out its logging afresh, from the blockr options and the
# environment variables as the calling test has set them, and again once the
# test ends, after the test's own settings are undone (see `reset_logging()`).
local_reset_logging <- function(frame = parent.frame()) {
  reset_logging()
  withr::defer(reset_logging(), envir = frame)
  invisible()
}
