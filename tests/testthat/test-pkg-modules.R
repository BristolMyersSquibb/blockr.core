test_that("a package declares one startup module", {

  local_declared(declared_startup_modules)

  rm(list = ls(declared_startup_modules), envir = declared_startup_modules)

  local_mocked_bindings(pkg_dependencies = function(pkg) character())

  entry <- register_startup_module(
    test_module("startup.js"),
    package = "blockr.test"
  )

  expect_identical(
    entry,
    list(package = "blockr.test", module = test_module("startup.js"))
  )

  expect_identical(startup_modules(), list(entry))

  entry <- register_startup_module(
    test_module("init.js"),
    package = "blockr.test"
  )

  expect_identical(startup_modules(), list(entry))
})

test_that("startup modules come after those of what they depend on", {

  local_declared(declared_startup_modules)

  rm(list = ls(declared_startup_modules), envir = declared_startup_modules)

  deps <- list(
    pkg.a = character(),
    pkg.b = c("pkg.c", "shiny"),
    pkg.c = "pkg.a",
    pkg.d = "pkg.e",
    pkg.f = c("pkg.b", "pkg.a")
  )

  local_mocked_bindings(pkg_dependencies = function(pkg) deps[[pkg]])

  for (pkg in rev(names(deps))) {
    register_startup_module(test_module("startup.js", pkg), package = pkg)
  }

  res <- chr_xtr(startup_modules(), "package")

  expect_setequal(res, names(deps))

  expect_lt(match("pkg.a", res), match("pkg.c", res))
  expect_lt(match("pkg.c", res), match("pkg.b", res))
  expect_lt(match("pkg.b", res), match("pkg.f", res))

  expect_identical(chr_xtr(startup_modules(), "package"), res)

  rm(list = ls(declared_startup_modules), envir = declared_startup_modules)

  expect_identical(startup_modules(), list())
})

test_that("a package's dependencies are those its DESCRIPTION names", {

  deps <- pkg_dependencies("blockr.core")

  expect_true(all(c("shiny", "vctrs", "yyjsonr") %in% deps))
  expect_false(any(c("R", "testthat") %in% deps))
  expect_false(any(grepl("[ (]", deps)))
})

test_that("a package declares the modules it offers others", {

  local_declared(declared_modules)

  entry <- register_module("filter", test_module("filter.js"),
                           package = "blockr.test")

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

  entry <- register_module("filter", test_module("filter-again.js"),
                           package = "blockr.test")

  expect_identical(get_module("blockr.test-filter"), entry)

  expect_error(get_module("blockr.test-none"), class = "module_missing")
})

test_that("declaring a module is checked", {

  local_declared(declared_startup_modules)
  local_declared(declared_modules)

  expect_error(
    register_startup_module(test_module("startup.js"), package = NULL),
    class = "startup_module_invalid",
    regexp = "declared by a package"
  )

  expect_error(
    register_startup_module("startup.js", package = "blockr.test"),
    class = "startup_module_invalid"
  )

  expect_error(
    register_module("filter", test_module("filter.js"), package = NULL),
    class = "module_invalid"
  )

  expect_error(
    register_module(NA_character_, test_module("filter.js"),
                    package = "blockr.test"),
    class = "module_invalid"
  )

  expect_error(
    register_module("filter", "filter.js", package = "blockr.test"),
    class = "module_invalid"
  )
})
