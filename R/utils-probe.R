# Startup probe counters -- TEMPORARY DIAGNOSTICS.
#
# Info-level counters for comparing a slow (prod) first load against a local
# run of the same workflow: block construction, actual evaluations (with
# duration), and skipped re-evaluations. Info level on purpose: it lands at
# the default `blockr.log_level` without any prod configuration.
#
# Counters live in a process-level env, reset at each board_server() start, so
# on a one-session-per-process host the numbers read as one clean sequence.
# Concurrent sessions in one process interleave counts; the block ids and
# timestamps in each line keep those readable.

probe_env <- new.env(parent = emptyenv())

probe_reset <- function() {
  rm(list = ls(probe_env), envir = probe_env)
  probe_env$t0 <- Sys.time()
  invisible()
}

probe_count <- function(key) {
  n <- probe_env[[key]]
  n <- if (is.null(n)) 1L else n + 1L
  probe_env[[key]] <- n
  n
}

# ms since the current board started (self-priming if no board start was seen)
probe_elapsed <- function() {
  if (is.null(probe_env$t0)) {
    probe_env$t0 <- Sys.time()
  }
  format(round(as.numeric(Sys.time() - probe_env$t0, units = "secs") * 1000))
}
