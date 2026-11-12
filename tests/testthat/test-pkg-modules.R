test_that("core lists what it offers core in its `._BLOCKR_MODULES_`", {

  listing <- package_listing("blockr.core")

  # Its `.onLoad()` fills the object that it exports
  expect_identical(listing, core_listing())
  expect_identical(names(listing), c("blocks", "plugins", "options", "modules"))

  # Its block classes, as its inst/registry/blocks.yml lists them
  expect_identical(listing$blocks, read_blocks_yml("blockr.core"))
  expect_true("new_head_block" %in% names(listing$blocks))

  expect_identical(
    chr_xtr(listing$plugins, "id"),
    c("basic_frontend_plugin", "file_storage_plugin", "edit_board_plugin")
  )
  expect_identical(
    names(listing$options),
    c("n_rows", "show_conditions", "dark_mode")
  )

  # Every module that core lists ships with it
  paths <- c(
    chr_xtr(listing$plugins, "module"),
    chr_xtr(listing$options, "module"),
    unlist(listing$modules)
  )

  for (path in paths) {
    file <- module_file(test_module(path, "blockr.core"))
    expect_true(nzchar(file), label = path)
  }
})

test_that("R finds the installed packages that export it, loading none", {

  lib <- withr::local_tempdir()

  # The exports that each package records, as loadNamespace() reads them
  for (pkg in c("blockr.listing", "blockr.none", "blockr.broken")) {
    dir.create(file.path(lib, pkg, "Meta"), recursive = TRUE)
  }

  saveRDS(
    list(exports = c("new_thing", "._BLOCKR_MODULES_")),
    file.path(lib, "blockr.listing", "Meta", "nsInfo.rds")
  )
  saveRDS(
    list(exports = "new_thing"),
    file.path(lib, "blockr.none", "Meta", "nsInfo.rds")
  )
  writeLines(
    "not an rds file",
    file.path(lib, "blockr.broken", "Meta", "nsInfo.rds")
  )

  # A directory without the file, such as a stale lock
  dir.create(file.path(lib, "00LOCK-blockr.thing"))

  expect_identical(listing_exporters(lib), "blockr.listing")

  # R loaded none of them
  expect_false("blockr.listing" %in% loadedNamespaces())

  # Among the loaded packages, core exports it, whether installed or loaded
  # by `load_all()` of pkgload
  expect_true("blockr.core" %in% available_packages())
  expect_true(exports_listing("blockr.core"))
  expect_false(exports_listing("utils"))
})

test_that("a package that load_all() loads may export the object", {

  local_load_all_package()

  expect_true("regprobe" %in% available_packages())
  expect_identical(
    names(package_listing("regprobe")$blocks),
    "new_probe_block"
  )
})

test_that("a package lists nothing where it exports no object", {

  expect_identical(
    package_listing("utils"),
    list(blocks = list(), plugins = list(), options = list(), modules = list())
  )
})

test_that("a package's object is a list or an environment of its parts", {

  read <- function(obj) {
    local_mocked_bindings(listing_object = function(pkg) obj)
    package_listing("utils")
  }

  listing <- read(list(options = list(gap = list(module = "js/gap.js"))))

  expect_identical(listing$options, list(gap = list(module = "js/gap.js")))
  expect_identical(listing$blocks, list())

  # An environment, as a package's `.onLoad()` fills it, and a part that a
  # later version of core may read, which R ignores
  env <- list2env(list(modules = list(a = "js/a.js"), later = TRUE))

  expect_identical(read(env)$modules, list(a = "js/a.js"))
  expect_named(read(env), c("blocks", "plugins", "options", "modules"))

  for (obj in list("listing", list(1, 2), data.frame(blocks = 1))) {
    expect_error(read(obj), class = "package_listing_invalid")
  }
})

test_that("a package offers modules to other packages' UIs by name", {

  local_listing(
    "blockr.test",
    list(modules = list(filter = "js/filter.js", picker = "js/picker.js"))
  )

  mods <- listed_modules("blockr.test")

  expect_named(mods, c("blockr.test-filter", "blockr.test-picker"))
  expect_identical(mods[["blockr.test-filter"]], test_module("js/filter.js"))

  offered <- offered_modules()

  # Core offers its inputs to other packages' modules, beside blockr.test's
  expect_true(all(c("blockr.core-controls", names(mods)) %in% names(offered)))

  local_listing("blockr.none")
  expect_identical(listed_modules("blockr.none"), list())
})

test_that("core offers its inputs to other packages' modules without React", {

  module <- offered_modules("blockr.core")[["blockr.core-controls"]]

  expect_identical(
    module,
    test_module("js/blockr-core-controls.js", "blockr.core")
  )
  expect_true(nzchar(module_file(module)))

  # The page's import map names the same file `@blockr/core/controls`, so that
  # a module that imports the controls and one that reaches them through its
  # host get one copy.
  map <- yyjsonr::read_json_str(as.character(import_map()$children[[1L]]))

  expect_identical(
    map$imports[["@blockr/core/controls"]],
    paste0("./", served_url(module))
  )
})

test_that("modules that R cannot read stop what reads them", {

  local_listing("blockr.test", list(modules = list(filter = 1L, "js/a.js")))

  expect_error(
    listed_modules("blockr.test"),
    class = "package_modules_invalid"
  )

  local_listing("blockr.test", list(modules = list(filter = 1L)))

  expect_error(
    listed_modules("blockr.test"),
    paste(
      "Package blockr.test lists the modules that it offers others in its",
      "`._BLOCKR_MODULES_` that R cannot read. Its module filter is no path,",
      "a non-empty string."
    ),
    fixed = TRUE,
    class = "package_listing_invalid"
  )
})

test_that("a package lists its board options by ID, each with its module", {

  local_listing(
    "blockr.test",
    list(options = list(grid_gap = list(module = "js/grid-gap.js")))
  )

  expect_identical(
    listed_options("blockr.test"),
    list(
      grid_gap = list(
        package = "blockr.test",
        module = test_module("js/grid-gap.js")
      )
    )
  )

  local_listing(
    "blockr.test",
    list(options = list(grid_gap = list(), zoom = list(module = "")))
  )

  err <- expect_error(
    listed_options("blockr.test"),
    class = "package_options_invalid"
  )

  expect_match(
    conditionMessage(err),
    "Its entry for grid_gap lacks `module`.",
    fixed = TRUE
  )
  expect_match(
    conditionMessage(err),
    "Its entry for zoom gives `module` as a value other than a non-empty",
    fixed = TRUE
  )

  local_listing("blockr.test", list(options = list(list(module = "js/a.js"))))

  expect_error(
    listed_options("blockr.test"),
    class = "package_listing_invalid"
  )
})

test_that("two packages that list one board option stop the app", {

  local_listing(
    "blockr.one",
    list(options = list(grid_gap = list(module = "js/gap.js")))
  )
  local_listing(
    "blockr.two",
    list(options = list(grid_gap = list(module = "js/grid-gap.js")))
  )

  expect_error(
    packages_options(c("blockr.one", "blockr.two")),
    paste(
      "The board option grid_gap is listed by more than one package, grid_gap",
      "by blockr.one and blockr.two"
    ),
    fixed = TRUE,
    class = "package_options_conflict"
  )

  expect_named(packages_options("blockr.one"), "grid_gap")
})

test_that("an app has the board options of the packages that it uses", {

  local_empty_declarations()

  local_listing(
    "blockr.test",
    list(options = list(grid_gap = list(module = "js/grid-gap.js")))
  )
  local_listing(
    "blockr.other",
    list(options = list(zoom = list(module = "js/zoom.js")))
  )
  local_board_option_version("grid_gap", 2L)

  # An app that uses neither package, whose block classes and plugins come
  # from elsewhere
  expect_length(app_options(), 0L)

  # One that runs a plugin of blockr.test, which gives grid_gap the version
  # its method of `board_option_version()` gives
  plugins <- list(`blockr.test-grid_plugin` = list(name = "Grid"))

  expect_identical(
    app_options(block_registry, plugins),
    list(
      grid_gap = list(
        package = "blockr.test",
        module = test_module("js/grid-gap.js"),
        version = 2L
      )
    )
  )

  # One whose block registry holds a class of blockr.other
  registry <- new.env(parent = emptyenv())
  registry[["blockr.other-zoom_block"]] <- structure(
    list(package = "blockr.other"),
    class = "registry_entry"
  )

  local_mocked_bindings(registry_entries = function(registry) as.list(registry))

  expect_named(app_options(registry), "zoom")
})

test_that("R reads the schema that a module ships beside it", {

  expect_null(module_schema(NULL))
  expect_null(module_schema(test_module("js/none.js", "blockr.core")))

  schema <- module_schema(test_module("js/n-rows-option.js", "blockr.core"))

  expect_identical(schema$type, "integer")

  # R finds the schema of a board option beside the module of the package that
  # lists it, and of no board option that no package lists
  expect_identical(board_option_schema("n_rows"), schema)
  expect_null(board_option_schema("no_such_option"))

  # Core's plugin classes keep no state, so they ship no schema
  expect_null(plugin_class_schema("blockr.core-basic_frontend_plugin"))
  expect_null(plugin_class_schema("blockr.none-none_plugin"))
})
