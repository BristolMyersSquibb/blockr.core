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

test_that("core's own plugins keep no state", {

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

  # The Edit board plugin is neither a frontend nor a storage plugin, so an
  # app runs it beside whichever of those it runs
  edit <- new_edit_board_plugin()

  expect_s3_class(edit, c("edit_board_plugin", "plugin"), exact = TRUE)
  expect_identical(plugin_class_id(edit), "blockr.core-edit_board_plugin")
  expect_length(plugin_state(edit), 0L)
  expect_false(is_frontend_plugin(edit))
  expect_false(is_storage_plugin(edit))
  expect_identical(plugin_version(edit), 1L)
  expect_identical(plugin_requests(edit), list())
})

test_that("core's basic frontend keeps every default of blockr.ui", {

  theme <- frontend_theme(new_basic_frontend_plugin())

  expect_true(blockr.ui::is_blockr_theme(theme))

  # The look of the basic frontend is blockr.ui's, so its theme sets no theme
  # variable, and keeps the default of each in light mode and in dark mode.
  # A theme variable that a later blockr.ui adds fails this test, until the
  # frontend sets it or adds it here as a default that it keeps.
  kept <- c(
    "--bk-color-background", "--bk-color-foreground",
    "--bk-color-card", "--bk-color-card-foreground",
    "--bk-color-popover", "--bk-color-popover-foreground",
    "--bk-color-primary", "--bk-color-primary-foreground",
    "--bk-color-primary-text",
    "--bk-color-secondary", "--bk-color-secondary-foreground",
    "--bk-color-muted", "--bk-color-muted-foreground",
    "--bk-color-accent", "--bk-color-accent-foreground",
    "--bk-color-destructive", "--bk-color-destructive-foreground",
    "--bk-color-destructive-text",
    "--bk-color-warning", "--bk-color-warning-foreground",
    "--bk-color-warning-text",
    "--bk-color-success", "--bk-color-success-foreground",
    "--bk-color-success-text",
    "--bk-color-border", "--bk-color-input", "--bk-color-ring",
    "--bk-font-sans", "--bk-font-mono",
    "--bk-text-xs", "--bk-text-sm", "--bk-text-base", "--bk-text-lg",
    "--bk-radius",
    "--bk-shadow-sm", "--bk-shadow-md", "--bk-shadow-lg",
    "--bk-spacing",
    "--bk-z-index-overlay"
  )

  expect_identical(
    blockr.ui::unset_variables(theme),
    list(light = kept, dark = kept)
  )
})

test_that("a frontend that keeps every default needs no frontend_theme()", {

  grid <- plugin_ctor(function() NULL, c("grid_plugin", "frontend_plugin"))

  # Core's method for frontend_plugin gives a theme with no values, whose
  # rules R writes as nothing.
  theme <- frontend_theme(grid())

  expect_identical(theme, blockr.ui::new_blockr_theme())
  expect_identical(format(theme), "")
})

test_that("available_plugins() gives core's plugin classes by package and id", {

  expect_identical(
    available_plugins(),
    data.frame(
      package = rep("blockr.core", 3L),
      id = c(
        "basic_frontend_plugin",
        "file_storage_plugin",
        "edit_board_plugin"
      )
    )
  )

  local_plugin_classes(Notes = plugin_ctor(function() NULL, "notes_plugin"))

  expect_identical(
    available_plugins()[4L, ],
    data.frame(package = "blockr.test", id = "notes_plugin", row.names = 4L)
  )

  # A class's name stays with its package's listing, which the app's plugins
  # take it from
  expect_identical(
    app_plugins(available_plugins())[["blockr.test-notes_plugin"]]$name,
    "Notes"
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

test_that("core's method of complete_plugin() returns the plugin as it is", {

  notes <- plugin_ctor(function(about = "") NULL, "notes_plugin")
  aliases <- c(dag = "blockr.dag-dag_plugin")

  expect_identical(
    complete_plugin(notes("dag"), new_board(plugins = notes("dag")), aliases),
    notes("dag")
  )
})

test_that("the app holds the requests of each plugin's class, or none", {

  local_plugin_classes(
    Notes = plugin_ctor(function() NULL, "notes_plugin"),
    Bad = plugin_ctor(function() NULL, "bad_plugin")
  )

  rows <- subset(available_plugins(), id != "bad_plugin")

  expect_identical(
    app_plugins(rows)[["blockr.test-notes_plugin"]]$requests,
    list()
  )

  testthat::local_mocked_s3_method(
    "plugin_requests",
    "notes_plugin",
    function(x, ...) list(save = function(text) toupper(text))
  )

  reqs <- app_plugins(rows)[["blockr.test-notes_plugin"]]$requests

  expect_named(reqs, "save")
  expect_identical(reqs$save("a"), "A")

  # A method that gives anything but a list of functions by name stops the app
  for (bad in list(list(function() 1), list(save = 1), "save")) {

    testthat::local_mocked_s3_method(
      "plugin_requests",
      "bad_plugin",
      function(x, ...) bad
    )

    expect_error(
      app_plugins(available_plugins()),
      "for plugin class blockr.test-bad_plugin returns",
      fixed = TRUE,
      class = "plugin_requests_invalid"
    )
  }
})

test_that("the app holds the board options that each plugin's class reads", {

  local_plugin_classes(
    Notes = plugin_ctor(function() NULL, "notes_plugin"),
    Bad = plugin_ctor(function() NULL, "bad_plugin")
  )

  rows <- subset(available_plugins(), id != "bad_plugin")
  plugins <- app_plugins(rows)

  # Core's basic frontend reads two, and a class without a method of its own
  # reads none
  expect_identical(
    plugins[["blockr.core-basic_frontend_plugin"]]$options,
    "dark_mode"
  )
  expect_identical(plugins[["blockr.test-notes_plugin"]]$options, character())
  expect_null(plugin_options(new_file_storage_plugin()))

  testthat::local_mocked_s3_method(
    "plugin_options",
    "notes_plugin",
    function(x, ...) new_n_rows_option
  )

  expect_identical(
    app_plugins(rows)[["blockr.test-notes_plugin"]]$options,
    "n_rows"
  )

  # A method that gives anything but board option constructors stops the app
  for (bad in list("n_rows", new_n_rows_option(), list(function() 1))) {

    testthat::local_mocked_s3_method(
      "plugin_options",
      "bad_plugin",
      function(x, ...) bad
    )

    expect_error(
      app_plugins(available_plugins()),
      "for plugin class blockr.test-bad_plugin returns",
      fixed = TRUE,
      class = "plugin_options_invalid"
    )
  }
})

test_that("the app runs the plugins of its rows, by the ids of their classes", {

  local_plugin_classes(
    Notes = plugin_ctor(function(notes = list()) NULL, "notes_plugin")
  )

  local_plugin_version("notes_plugin", 2L)

  plugins <- app_plugins(available_plugins())

  expect_identical(
    lst_xtr(plugins, "name"),
    list(
      `blockr.core-basic_frontend_plugin` = "Basic frontend",
      `blockr.core-file_storage_plugin` = "File storage",
      `blockr.core-edit_board_plugin` = "Edit board",
      `blockr.test-notes_plugin` = "Notes"
    )
  )
  expect_identical(
    unlist(lst_xtr(plugins, "version")),
    c(
      `blockr.core-basic_frontend_plugin` = 1L,
      `blockr.core-file_storage_plugin` = 1L,
      `blockr.core-edit_board_plugin` = 1L,
      `blockr.test-notes_plugin` = 2L
    )
  )

  # Each with its class's module, as its package lists it
  expect_identical(
    plugins[["blockr.core-basic_frontend_plugin"]]$module,
    js_module("js/basic.js", package = "blockr.core")
  )
  expect_identical(
    plugins[["blockr.test-notes_plugin"]]$module,
    js_module("js/notes_plugin.js", package = "blockr.test")
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
      " $ text: chr \"To do\"",
      " $ tags: chr [1:2] \"a\" \"b\""
    )
  )

  expect_output(print(plugin), "<notes_plugin> blockr.test-notes_plugin")
  expect_identical(
    str_value(plugin),
    "<notes_plugin> blockr.test-notes_plugin"
  )
  expect_output(str(plugin), "<notes_plugin> blockr.test-notes_plugin")
})

test_that("a plugin prints fields that are empty or nested", {

  views <- plugin_ctor(
    function(views = list(), active = NULL, tags = character()) NULL,
    "views_plugin"
  )

  expect_identical(
    format(views()),
    c(
      "<views_plugin> blockr.test-views_plugin",
      " $ views : list()",
      " $ active: NULL",
      " $ tags  : chr(0)"
    )
  )

  plugin <- views(
    list(
      main = list(name = "Main", panels = list()),
      side = list(name = "Side", panels = list("b", "c"))
    ),
    "main",
    1:5
  )

  # R-devel adds a space before "List of" in utils::str().
  actual <- sub(": List of", ":List of", format(plugin), fixed = TRUE)

  expect_identical(
    actual,
    c(
      "<views_plugin> blockr.test-views_plugin",
      " $ views :List of 2",
      "  ..$ main:List of 2",
      "  .. ..$ name  : chr \"Main\"",
      "  .. ..$ panels: list()",
      "  ..$ side:List of 2",
      "  .. ..$ name  : chr \"Side\"",
      "  .. ..$ panels:List of 2",
      "  .. .. ..$ : chr \"b\"",
      "  .. .. ..$ : chr \"c\"",
      " $ active: chr \"main\"",
      " $ tags  : int [1:5] 1 2 3 4 5"
    )
  )
})
