# Restores what `env` holds of what packages declare once the calling test
# ends, so that a test can declare for packages of its own
local_declared <- function(env, frame = parent.frame()) {

  old <- as.list(env, all.names = TRUE)

  withr::defer(
    {
      rm(list = ls(env, all.names = TRUE), envir = env)
      list2env(old, envir = env)
    },
    envir = frame
  )
}

# The rows of `available_blocks()` for core's block classes, as an app that
# offers core's classes alone gives them to `serve()` in `blocks`.
core_block_rows <- function() {
  rows <- available_blocks()
  rows[rows[["package"]] == "blockr.core", ]
}

test_module <- function(path, package = "blockr.test") {
  js_module(path, package = package)
}
