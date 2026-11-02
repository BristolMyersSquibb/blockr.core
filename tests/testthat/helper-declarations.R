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

test_module <- function(path, package = "blockr.test") {
  js_module(path, package = package)
}
