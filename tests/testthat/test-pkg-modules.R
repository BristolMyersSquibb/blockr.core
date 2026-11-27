test_that("core lists what it offers core in its `._BLOCKR_MODULES_`", {

  listing <- package_listing("blockr.core")

  # Its `.onLoad()` fills the object that it exports
  expect_identical(listing, core_listing())
  expect_identical(
    names(listing),
    c("blocks", "plugins", "options", "modules", "families")
  )

  # Its block classes, as its inst/registry/blocks.yml lists them
  expect_identical(listing$blocks, read_blocks_yml("blockr.core"))
  expect_true("new_head_block" %in% names(listing$blocks))

  expect_identical(
    chr_xtr(listing$plugins, "id"),
    c("basic_frontend_plugin", "file_storage_plugin", "edit_board_plugin")
  )
  expect_identical(
    names(listing$options),
    c("n_rows", "dark_mode")
  )

  # Its output families, by the names under which its namespace binds them
  expect_identical(
    listing$families,
    c("table_output", "text_output", "plot_output")
  )
  expect_identical(
    listed_families("blockr.core"),
    list(
      table_output = table_output,
      text_output = text_output,
      plot_output = plot_output
    )
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
    list(blocks = list(), plugins = list(), options = list(), modules = list(),
         families = list())
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
  expect_named(
    read(env),
    c("blocks", "plugins", "options", "modules", "families")
  )

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

  expect_named(mods, c("filter", "picker"))
  expect_identical(mods[["filter"]], test_module("js/filter.js"))

  # By package and then by name
  offered <- offered_modules()

  # Core offers its inputs to other packages' modules, beside blockr.test's
  expect_true(all(c("blockr.core", "blockr.test") %in% names(offered)))
  expect_true("controls" %in% names(offered[["blockr.core"]]))
  expect_identical(offered[["blockr.test"]], mods)

  local_listing("blockr.none")
  expect_identical(listed_modules("blockr.none"), list())
})

test_that("core offers its inputs to other packages' modules without React", {

  module <- offered_modules("blockr.core")[["blockr.core"]][["controls"]]

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

test_that("a package lists its output families by name", {

  local_family_package()

  probe <- get("probe_output", envir = asNamespace("famprobe"))

  expect_identical(listed_families("famprobe"), list(probe_output = probe))

  # An app holds them beside core's, by class
  families <- app_families()

  expect_identical(families[["probe_output"]], probe)
  expect_true(
    all(c("table_output", "text_output", "plot_output") %in% names(families))
  )

  # A list of names does as a character vector does, and a name given twice
  # counts once
  expect_identical(
    listed_families(
      "blockr.core",
      list(families = list("text_output", "text_output"))
    ),
    list(text_output = text_output)
  )
  expect_identical(
    listed_families("blockr.core", list(families = list())),
    list()
  )
})

test_that("output families that R cannot read stop what reads them", {

  read <- function(families) {
    listed_families("blockr.core", list(families = families))
  }

  for (families in list(1L, list("table_output", 2L), NA_character_, "",
                        list(table_output = list()))) {
    expect_error(
      read(families),
      "They are no names of output families, each a non-empty string.",
      fixed = TRUE,
      class = "package_families_invalid"
    )
  }

  # Names under which core binds no output family of that class: a function
  # that is no family, and nothing
  expect_error(
    read(c("new_head_block", "table_output", "nope")),
    paste(
      "Package blockr.core lists output families in its `._BLOCKR_MODULES_`",
      "that R cannot read. It binds no output family of class new_head_block",
      "under that name. It binds no output family of class nope under that",
      "name."
    ),
    fixed = TRUE,
    class = "package_listing_invalid"
  )

  # A family bound under a name other than its class
  local_family_package()

  expect_error(
    listed_families("famprobe", list(families = "probe_alias")),
    "It binds no output family of class probe_alias under that name.",
    fixed = TRUE,
    class = "package_families_invalid"
  )
})

test_that("two packages that list a family of one class stop the app", {

  local_mocked_bindings(
    listed_families = function(pkg, ...) list(table_output = table_output)
  )

  expect_error(
    app_families(c("blockr.one", "blockr.two")),
    paste(
      "The output family table_output is listed by more than one package,",
      "table_output by blockr.one and blockr.two, where R tells families",
      "apart by their class."
    ),
    fixed = TRUE,
    class = "package_families_conflict"
  )

  expect_named(app_families("blockr.one"), "table_output")
})

test_that("a reply names the board options of every available package", {

  local_listing(
    "blockr.test",
    list(options = list(grid_gap = list(module = "js/grid-gap.js")))
  )
  local_board_option_version("grid_gap", 2L)

  ping <- function() "pong"

  testthat::local_mocked_s3_method(
    "board_option_requests",
    "grid_gap_option",
    function(x, ...) list(ping = ping)
  )

  # The version and the requests of each come from its methods of
  # `board_option_version()` and `board_option_requests()`
  expect_identical(
    app_options()[["grid_gap"]],
    list(
      package = "blockr.test",
      module = test_module("js/grid-gap.js"),
      version = 2L,
      requests = list(ping = ping)
    )
  )

  # Core's own are among them
  core <- c("n_rows", "dark_mode")

  expect_true(all(core %in% names(app_options())))

  # A package that offers no block class or plugin to the app, such as one
  # whose board option the requests of another package's blocks read
  local_listing(
    "blockr.other",
    list(options = list(zoom = list(module = "js/zoom.js")))
  )
  local_versions(blockr.test = "0.1.0", blockr.other = "0.2.0")

  reply <- loaded_reply()

  expect_identical(reply$options[["zoom"]]$package, "blockr.other")
  expect_true(all(c("blockr.test", "blockr.other") %in% names(reply$packages)))
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
  expect_null(
    plugin_class_schema(pkg_id("blockr.core", "basic_frontend_plugin"))
  )
  expect_null(plugin_class_schema(pkg_id("blockr.none", "none_plugin")))
})
