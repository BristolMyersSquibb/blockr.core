# Hardening for the browser test of the Shiny transport, which drives Chrome
# through chromote and runs its app in another R process.

# Give Chrome longer to open its remote-debugging port. The default of chromote
# is 10s (`getOption("chromote.timeout", 10)` in launch_chrome_impl), too short
# on loaded CI runners -- Windows especially -- where launch intermittently
# aborts with "Chrome debugging port not open after 10 seconds". Scoped to the
# suite so the option is restored when testing finishes.
withr::local_options(
  chromote.timeout = 30,
  .local_envir = testthat::teardown_env()
)

# Chrome leaves scratch dirs (com.google.Chrome.* / org.chromium.Chromium.*,
# scoped_dir variants included) wherever `$TMPDIR` pointed when it was spawned,
# and R CMD check flags leftovers in the temp directory it hands to subprocesses
# as "detritus in the temp directory" -- a NOTE the CI gate fails on. So does
# the temp directory of the app's R process, which the test kills.
#
# Keep them out of the inspected directory instead. Chrome and the app's R
# process read `TMPDIR` at spawn while R fixes `tempdir()` at startup, so
# pointing it at a suite-owned directory below the session temp dir puts their
# scratch one level down from what check lists -- it looks at that top level
# only, and skips the `Rtmp*` directory holding this one.
local({

  chrome_tmp <- withr::local_tempdir(
    "chrome",
    .local_envir = testthat::teardown_env()
  )

  withr::local_envvar(
    TMPDIR = chrome_tmp,
    .local_envir = testthat::teardown_env()
  )

  # Close the browser so it does not outlive the suite, since closing a
  # session leaves the shared browser to R's exit.
  withr::defer(
    if (requireNamespace("chromote", quietly = TRUE) &&
          isTRUE(chromote::has_default_chromote_object())) {
      try(chromote::default_chromote_object()$close(), silent = TRUE)
    },
    testthat::teardown_env()
  )
})
