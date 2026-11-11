test_that("a package's constructor makes a plugin of its class", {

  dag <- plugin_ctor(
    function(positions = list()) NULL,
    "dag_plugin",
    "blockr.dag"
  )

  plugin <- dag(positions = list(a = list(x = 1, y = 2)))

  expect_true(is_plugin(plugin))
  expect_s3_class(plugin, c("dag_plugin", "plugin"), exact = TRUE)
  expect_identical(
    plugin_state(plugin),
    list(positions = list(a = list(x = 1, y = 2)))
  )

  # The id of its class joins the class to the package its constructor lives in
  expect_identical(plugin_class_id(plugin), "blockr.dag-dag_plugin")

  # A class extends core's frontend_plugin or storage_plugin, or plugin itself
  dock <- plugin_ctor(function() NULL, c("dock_plugin", "frontend_plugin"))

  expect_s3_class(
    dock(),
    c("dock_plugin", "frontend_plugin", "plugin"),
    exact = TRUE
  )
  expect_true(is_frontend_plugin(dock()))
  expect_false(is_storage_plugin(dock()))

  expect_false(is_plugin(list()))
  expect_error(plugin_class_id(list()), class = "plugin_inheritance_invalid")
})

test_that("a plugin's class vector defaults to its id and serves dispatch", {

  # A constructor of blockr.test that hands new_plugin() an id and a class
  # vector of its own.
  ctor <- function(id, class) {
    ns <- list2env(
      list(.packageName = "blockr.test"),
      parent = asNamespace("blockr.core")
    )
    fun <- function() new_plugin(list(), id, class)
    environment(fun) <- list2env(list(id = id, class = class), parent = ns)
    fun()
  }

  plugin <- ctor("notes_plugin", c("notes_plugin", "storage_plugin"))

  expect_s3_class(
    plugin,
    c("notes_plugin", "storage_plugin", "plugin"),
    exact = TRUE
  )
  expect_identical(plugin_class_id(plugin), "blockr.test-notes_plugin")

  # The id alone identifies the class, whatever its class vector
  other <- ctor("notes_plugin", c("my_notes_plugin", "notes_plugin"))

  expect_s3_class(other, c("my_notes_plugin", "notes_plugin", "plugin"))
  expect_identical(plugin_class_id(other), "blockr.test-notes_plugin")
})

test_that("core's basic frontend and storage plugin keep no state", {

  frontend <- new_basic_frontend_plugin()

  expect_s3_class(
    frontend,
    c("basic_frontend_plugin", "frontend_plugin", "plugin"),
    exact = TRUE
  )
  expect_identical(
    plugin_class_id(frontend),
    "blockr.core-basic_frontend_plugin"
  )
  expect_length(plugin_state(frontend), 0L)

  storage <- new_file_storage_plugin()

  expect_s3_class(
    storage,
    c("file_storage_plugin", "storage_plugin", "plugin"),
    exact = TRUE
  )
  expect_identical(plugin_class_id(storage), "blockr.core-file_storage_plugin")
  expect_length(plugin_state(storage), 0L)
})

test_that("available_plugins() gives core's plugin classes, each with a name", {

  expect_identical(
    available_plugins(),
    data.frame(
      package = c("blockr.core", "blockr.core"),
      id = c("basic_frontend_plugin", "file_storage_plugin"),
      name = c("Basic frontend", "File storage")
    )
  )

  local_plugin_classes(Notes = plugin_ctor(function() NULL, "notes_plugin"))

  expect_identical(
    available_plugins()[3L, ],
    data.frame(
      package = "blockr.test",
      id = "notes_plugin",
      name = "Notes",
      row.names = 3L
    )
  )
})

test_that("new_plugin() refuses a constructor outside a package", {

  new_notes_plugin <- function(text = "") {
    new_plugin(list(text = text), "notes_plugin")
  }

  environment(new_notes_plugin) <- globalenv()

  expect_error(new_notes_plugin(), class = "plugin_package_missing")
})

test_that("new_plugin() refuses an id, a class or a state it cannot hold", {

  # A constructor of blockr.test that hands new_plugin() what it is given.
  bad <- function(id = "notes_plugin", class = id, state = list()) {
    ns <- list2env(
      list(.packageName = "blockr.test"),
      parent = asNamespace("blockr.core")
    )
    ctor <- function() new_plugin(state, id, class)
    environment(ctor) <- list2env(
      list(id = id, class = class, state = state),
      parent = ns
    )
    ctor()
  }

  expect_s3_class(bad(), "notes_plugin")

  for (id in list("notes", "plugin", "frontend_plugin", "_plugin", 1L,
                  NA_character_, c("a_plugin", "b_plugin"))) {
    expect_error(
      bad(id = id, class = "notes_plugin"),
      class = "plugin_id_invalid"
    )
  }

  for (class in list(character(), "notes", "plugin", "frontend_plugin",
                     c("storage_plugin", "plugin"), 1L, NA_character_)) {
    expect_error(bad(class = class), class = "plugin_class_invalid")
  }

  for (state in list("text", list(1L), new_n_rows_option(1L))) {
    expect_error(bad(state = state), class = "plugin_state_invalid")
  }

  # An empty state is an empty list by name
  expect_identical(plugin_state(bad()), set_names(list(), character()))
})

test_that("a plugin class's method gives its version, or it is at version 1", {

  notes <- plugin_ctor(function() NULL, "notes_plugin")

  local_plugin_classes(Notes = notes)

  expect_identical(plugin_version(notes()), 1L)
  expect_identical(plugin_class_version("blockr.test-notes_plugin"), 1L)

  local_plugin_version("notes_plugin", 2L)

  expect_identical(plugin_version(notes()), 2L)
  expect_identical(plugin_class_version("blockr.test-notes_plugin"), 2L)

  # R calls the method on the plugin that the class's constructor makes, so
  # that the method of a class that it extends counts
  local_plugin_version("frontend_plugin", 5L)

  dock <- plugin_ctor(function() NULL, c("dock_plugin", "frontend_plugin"))
  local_plugin_classes(Dock = dock)

  expect_identical(plugin_class_version("blockr.test-dock_plugin"), 5L)

  bad <- plugin_ctor(function() NULL, "bad_plugin")
  local_plugin_classes(Bad = bad)
  local_plugin_version("bad_plugin", "two")

  expect_error(
    plugin_class_version("blockr.test-bad_plugin"),
    class = "plugin_version_invalid"
  )

  # A class that no available package lists has no version
  expect_error(
    plugin_class_version("blockr.test-gone_plugin"),
    class = "plugin_class_unknown"
  )
})

test_that("a plugin class's method gives its requests, or it has none", {

  local_plugin_classes(
    Notes = plugin_ctor(function() NULL, "notes_plugin"),
    Bad = plugin_ctor(function() NULL, "bad_plugin")
  )

  expect_identical(plugin_class_requests("blockr.test-notes_plugin"), list())

  testthat::local_mocked_s3_method(
    "plugin_requests",
    "notes_plugin",
    function(x, ...) list(save = function(text) toupper(text))
  )

  reqs <- plugin_class_requests("blockr.test-notes_plugin")

  expect_named(reqs, "save")
  expect_identical(reqs$save("a"), "A")

  for (bad in list(list(function() 1), list(save = 1), "save")) {

    testthat::local_mocked_s3_method(
      "plugin_requests",
      "bad_plugin",
      function(x, ...) bad
    )

    expect_error(
      plugin_class_requests("blockr.test-bad_plugin"),
      class = "plugin_requests_invalid"
    )
  }
})

test_that("the app runs the plugins of its rows, by the ids of their classes", {

  local_plugin_classes(
    Notes = plugin_ctor(function(notes = list()) NULL, "notes_plugin")
  )

  local_plugin_version("notes_plugin", 2L)

  expect_identical(
    app_plugins(available_plugins()),
    list(
      `blockr.core-basic_frontend_plugin` = list(
        name = "Basic frontend",
        version = 1L
      ),
      `blockr.core-file_storage_plugin` = list(
        name = "File storage",
        version = 1L
      ),
      `blockr.test-notes_plugin` = list(name = "Notes", version = 2L)
    )
  )

  # Rows that narrow the app's plugins keep their order
  rows <- data.frame(
    package = c("blockr.test", "blockr.core"),
    id = c("notes_plugin", "basic_frontend_plugin")
  )

  expect_named(
    app_plugins(rows),
    c("blockr.test-notes_plugin", "blockr.core-basic_frontend_plugin")
  )
})

test_that("core's basic frontend and storage plugin give way to others", {

  local_plugin_classes(
    Dock = plugin_ctor(function() NULL, c("dock_plugin", "frontend_plugin")),
    Grid = plugin_ctor(function() NULL, c("grid_plugin", "frontend_plugin")),
    Session = plugin_ctor(
      function() NULL,
      c("session_plugin", "storage_plugin")
    ),
    Drive = plugin_ctor(function() NULL, c("drive_plugin", "storage_plugin"))
  )

  rows <- function(...) {
    ids <- c(...)
    data.frame(package = id_package(ids), id = id_class(ids))
  }

  core <- c(
    "blockr.core-basic_frontend_plugin",
    "blockr.core-file_storage_plugin"
  )

  expect_named(app_plugins(rows(core)), core)

  expect_named(
    app_plugins(rows(core, "blockr.test-dock_plugin")),
    c("blockr.core-file_storage_plugin", "blockr.test-dock_plugin")
  )

  expect_named(
    app_plugins(rows("blockr.test-session_plugin", core)),
    c("blockr.test-session_plugin", "blockr.core-basic_frontend_plugin")
  )

  expect_named(
    app_plugins(rows("blockr.test-dock_plugin")),
    "blockr.test-dock_plugin"
  )

  # Rows that hold no frontend, two or two storage plugins stop the app
  expect_error(
    app_plugins(rows("blockr.core-file_storage_plugin")),
    "no frontend",
    class = "app_plugins_invalid"
  )

  expect_error(
    app_plugins(
      rows(core, "blockr.test-dock_plugin", "blockr.test-grid_plugin")
    ),
    "blockr.test-dock_plugin and blockr.test-grid_plugin",
    class = "app_plugins_invalid"
  )

  expect_error(
    app_plugins(
      rows(core, "blockr.test-session_plugin", "blockr.test-drive_plugin")
    ),
    "storage plugins blockr.test-session_plugin and blockr.test-drive_plugin",
    class = "app_plugins_invalid"
  )

  # So do rows that name one class twice
  expect_error(
    app_plugins(rows(core, "blockr.core-basic_frontend_plugin")),
    "blockr.core-basic_frontend_plugin more than once",
    class = "app_plugins_invalid"
  )
})

test_that("the app's rows name classes that available packages list", {

  expect_error(
    app_plugins(data.frame(package = "blockr.dag", id = "dag_plugin")),
    "blockr.dag",
    class = "app_plugins_unknown"
  )

  expect_error(
    app_plugins(data.frame(package = "blockr.core", id = "dag_plugin")),
    "dag_plugin of blockr.core",
    class = "app_plugins_unknown"
  )

  for (bad in list(list(), "x", data.frame(package = "blockr.core"),
                   data.frame(package = NA_character_, id = "a_plugin"))) {
    expect_error(app_plugins(bad), class = "app_plugins_invalid")
  }
})

test_that("the app's plugins may come from a YAML file", {

  local_plugin_classes(
    Notes = plugin_ctor(function() NULL, "notes_plugin"),
    Tools = plugin_ctor(function() NULL, "tools_plugin")
  )

  dir <- withr::local_tempdir()

  yaml <- function(...) {
    file <- tempfile(fileext = ".yml", tmpdir = dir)
    writeLines(c(...), file)
    file
  }

  # A package listed alone runs all of its plugin classes
  expect_named(
    app_plugins(yaml("- blockr.core:", "  - basic_frontend_plugin",
                     "- blockr.test")),
    c(
      "blockr.core-basic_frontend_plugin",
      "blockr.test-notes_plugin",
      "blockr.test-tools_plugin"
    )
  )

  expect_named(
    app_plugins(yaml("- blockr.core")),
    c("blockr.core-basic_frontend_plugin", "blockr.core-file_storage_plugin")
  )

  expect_error(
    app_plugins(yaml("- blockr.dag")),
    class = "app_plugins_unknown"
  )

  expect_error(
    app_plugins(yaml("- blockr.core:", "  - dag_plugin")),
    class = "app_plugins_unknown"
  )

  for (bad in list(yaml("blockr.core: [basic_frontend_plugin]"),
                   yaml("- 1"), yaml("- [a, b]"), file.path(dir, "none.yml"),
                   dir, yaml("- blockr.core: {a: 1}"), yaml("{"))) {
    expect_error(app_plugins(bad), class = "app_plugins_invalid")
  }
})

test_that("a plugin prints its class's id and its state", {

  notes <- plugin_ctor(
    function(text = "", tags = character()) NULL,
    "notes_plugin"
  )

  plugin <- notes("To do", c("a", "b"))

  expect_identical(
    format(plugin),
    c(
      "<notes_plugin> blockr.test-notes_plugin",
      "text: To do",
      "tags: a, b"
    )
  )

  expect_output(print(plugin), "<notes_plugin> blockr.test-notes_plugin")
  expect_identical(
    str_value(plugin),
    "<notes_plugin> blockr.test-notes_plugin"
  )
  expect_output(str(plugin), "<notes_plugin> blockr.test-notes_plugin")
})
