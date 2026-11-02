test_that("a package declares one startup module", {

  local_declared(declared_startup_modules)

  declared_startup_modules$entries <- list()

  entry <- register_startup_module(test_module("startup.js"))

  expect_identical(
    entry,
    list(package = "blockr.test", module = test_module("startup.js"))
  )

  expect_identical(startup_modules(), list(entry))

  entry <- register_startup_module(test_module("init.js"))

  expect_identical(startup_modules(), list(entry))
})

test_that("startup modules keep the order packages declare them in", {

  local_declared(declared_startup_modules)

  declared_startup_modules$entries <- list()

  expect_identical(startup_modules(), list())

  for (pkg in c("pkg.b", "pkg.a", "pkg.c")) {
    register_startup_module(test_module("startup.js", pkg))
  }

  expect_identical(
    chr_xtr(startup_modules(), "package"),
    c("pkg.b", "pkg.a", "pkg.c")
  )

  entry <- register_startup_module(test_module("init.js", "pkg.a"))

  expect_identical(
    chr_xtr(startup_modules(), "package"),
    c("pkg.b", "pkg.a", "pkg.c")
  )

  expect_identical(startup_modules()[[2L]], entry)
})

test_that("a package declares the modules it offers others", {

  local_declared(declared_modules)

  entry <- register_module("filter", test_module("filter.js"))

  expect_identical(
    entry,
    list(
      id = "blockr.test-filter",
      name = "filter",
      package = "blockr.test",
      module = test_module("filter.js")
    )
  )

  expect_identical(get_module("blockr.test-filter"), entry)
  expect_true("blockr.test-filter" %in% list_modules())

  entry <- register_module("filter", test_module("filter-again.js"))

  expect_identical(get_module("blockr.test-filter"), entry)

  expect_error(get_module("blockr.test-none"), class = "module_missing")
})

test_that("declaring a module is checked", {

  local_declared(declared_startup_modules)
  local_declared(declared_modules)

  expect_error(
    register_startup_module(js_module("startup.js", package = NULL)),
    class = "startup_module_invalid",
    regexp = "declared by a package"
  )

  expect_error(
    register_startup_module("startup.js"),
    class = "startup_module_invalid"
  )

  expect_error(
    register_module("filter", js_module("filter.js", package = NULL)),
    class = "module_invalid"
  )

  expect_error(
    register_module(NA_character_, test_module("filter.js")),
    class = "module_invalid"
  )

  expect_error(
    register_module("filter", "filter.js"),
    class = "module_invalid"
  )
})
