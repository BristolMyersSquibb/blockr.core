# CI hardening for the chromote-driven shinytest2 e2e tests.

# Give Chrome longer to open its remote-debugging port. chromote's default is
# 10s (`getOption("chromote.timeout", 10)` in launch_chrome_impl), too short on
# loaded CI runners -- Windows especially -- where launch intermittently aborts
# with "Chrome debugging port not open after 10 seconds". Scoped to the suite
# so the option is restored when testing finishes.
withr::local_options(
  chromote.timeout = 30,
  .local_envir = testthat::teardown_env()
)

# Chrome leaves scratch dirs (com.google.Chrome.* / org.chromium.Chromium.*,
# scoped_dir variants included) in the session temp parent (`$TMPDIR`), which
# R CMD check then flags as "detritus in the temp directory" -- a NOTE the CI
# gate fails on. `app$stop()` only ends the shinytest2 session; the shared
# browser stays alive until R exits, so its scratch is never reclaimed. At the
# end of the suite, close the browser (a clean shutdown reclaims its scratch),
# then sweep anything that still leaked.
#
# On CI the sweep is unconditional. Closing the browser waits for its main
# process, but a Chrome helper (crashpad, zygote) can drop a fresh scratch dir
# a moment later -- past a snapshot taken here -- so settle first, then remove
# every match. A throwaway runner has no unrelated dirs to protect. Locally,
# keep the conservative `setdiff(before)` so a shared `$TMPDIR` retains its own.
local({

  pat <- "^(com\\.google\\.Chrome|org\\.chromium\\.Chromium)"
  tmp_parent <- dirname(tempdir())
  before <- list.files(tmp_parent, pattern = pat)
  on_ci <- nzchar(Sys.getenv("CI"))

  withr::defer(
    {
      had_browser <- isTRUE(chromote::has_default_chromote_object())

      if (had_browser) {
        try(chromote::default_chromote_object()$close(), silent = TRUE)
      }

      if (on_ci) {

        if (had_browser) {
          Sys.sleep(2)
        }

        leaked <- list.files(tmp_parent, pattern = pat)

      } else {
        leaked <- setdiff(list.files(tmp_parent, pattern = pat), before)
      }

      unlink(
        file.path(tmp_parent, leaked),
        recursive = TRUE,
        force = TRUE
      )
    },
    testthat::teardown_env()
  )
})
