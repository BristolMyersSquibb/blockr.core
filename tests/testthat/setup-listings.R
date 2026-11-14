# Core's tests leave out the listings of the other installed packages.
# A library may hold a package that lists a plugin class or a board option,
# as blockr.dock does.
# Its listing would otherwise reach what the tests check.
# The plugin classes that `available_plugins()` gives would hold its class.
# Where its class is a frontend, as blockr.dock's is, an app would run it in
# place of core's basic frontend.
# The replies to `hello` would hold its board options.
# So the mock below has `available_packages()` leave out every package that a
# library holds, except core, for the whole run.
# A test still adds a package of its own with `local_listing()`, or loads one
# with `pkgload::load_all()`.
# Such a package sits in no library, and R finds it among the loaded packages.
#
# The mock does not reach the apps that the browser tests serve in another R
# process.
# So each of those apps that serves a board or a path names core's plugins in
# `plugins`.
#
# This file runs before setup-registry.R.
# That file reads the listing of each available package.
# Reading a listing loads its package.
# A package loaded there would stay loaded for the whole run.
# The replies to `hello` hold the board options of every loaded package.
# The setup files run in the order of their names, so the name of this file
# sorts before that of setup-registry.R.
local({

  available <- available_packages

  testthat::local_mocked_bindings(
    available_packages = function() {
      installed <- unlst(lapply(.libPaths(), listing_exporters))
      setdiff(available(), setdiff(installed, "blockr.core"))
    },
    .package = "blockr.core",
    .env = testthat::teardown_env()
  )
})
