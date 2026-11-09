test_that("a package's constructor makes a plugin of its class", {

  dag <- plugin_ctor(
    function(positions = list()) NULL,
    "dag_plugin",
    "blockr.dag"
  )

  plugin <- dag(positions = list(a = list(x = 1, y = 2)), plugin_name = "DAG")

  expect_true(is_plugin(plugin))
  expect_s3_class(plugin, c("dag_plugin", "plugin"), exact = TRUE)
  expect_identical(plugin_name(plugin), "DAG")
  expect_identical(
    plugin_state(plugin),
    list(positions = list(a = list(x = 1, y = 2)))
  )

  # The id of its class joins the class to the package its constructor lives in
  expect_identical(plugin_class_id(plugin), "blockr.dag-dag_plugin")

  # A plugin without a name takes one from its class
  expect_identical(plugin_name(dag()), "Dag")
  expect_identical(default_plugin_name("notes_plugin"), "Notes")
  expect_identical(default_plugin_name("my_notes_plugin"), "My notes")

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
  expect_error(plugin_name(list()), class = "plugin_inheritance_invalid")
})

test_that("new_plugin() refuses a constructor outside a package", {

  new_notes_plugin <- function(text = "") {
    new_plugin(list(text = text), class = "notes_plugin")
  }

  environment(new_notes_plugin) <- globalenv()

  expect_error(new_notes_plugin(), class = "plugin_package_missing")
})

test_that("new_plugin() refuses a class vector or a state it cannot hold", {

  # A constructor of blockr.test that hands new_plugin() what it is given.
  bad <- function(class = "notes_plugin", state = list(), name = NULL) {
    ns <- list2env(
      list(.packageName = "blockr.test"),
      parent = asNamespace("blockr.core")
    )
    ctor <- function() new_plugin(state, name, class)
    environment(ctor) <- list2env(
      list(class = class, state = state, name = name),
      parent = ns
    )
    ctor()
  }

  expect_s3_class(bad(), "notes_plugin")

  for (class in list(character(), "notes", "plugin", "frontend_plugin",
                     c("storage_plugin", "plugin"), 1L, NA_character_)) {
    expect_error(bad(class = class), class = "plugin_class_invalid")
  }

  for (state in list("text", list(1L), new_n_rows_option(1L))) {
    expect_error(bad(state = state), class = "plugin_state_invalid")
  }

  expect_error(bad(name = 1L), class = "plugin_name_invalid")
  expect_error(bad(name = NA_character_), class = "plugin_name_invalid")

  # An empty state is an empty list by name
  expect_identical(plugin_state(bad()), set_names(list(), character()))
})

test_that("a plugin class's method gives its version, or it is at version 1", {

  notes <- plugin_ctor(function() NULL, "notes_plugin")

  expect_identical(plugin_version(notes()), 1L)
  expect_identical(plugin_class_version("blockr.test-notes_plugin"), 1L)

  local_plugin_version("notes_plugin", 2L)

  expect_identical(plugin_version(notes()), 2L)
  expect_identical(plugin_class_version("blockr.test-notes_plugin"), 2L)

  # R calls the method of the class alone, on an object it makes from the id
  local_plugin_version("frontend_plugin", 5L)

  dock <- plugin_ctor(function() NULL, c("dock_plugin", "frontend_plugin"))

  expect_identical(plugin_class_version("blockr.test-dock_plugin"), 1L)
  expect_identical(plugin_class_version(plugin_class_id(dock())), 1L)

  local_plugin_version("bad_plugin", "two")

  expect_error(
    plugin_class_version("blockr.test-bad_plugin"),
    class = "plugin_version_invalid"
  )
})

test_that("a plugin class's method gives its requests, or it has none", {

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

test_that("a plugin prints its class, its name and its state", {

  notes <- plugin_ctor(
    function(text = "", tags = character()) NULL,
    "notes_plugin"
  )

  plugin <- notes("To do", c("a", "b"), plugin_name = "Todo")

  expect_identical(
    format(plugin),
    c(
      "<notes_plugin> \"Todo\"",
      "Class: blockr.test-notes_plugin",
      "text: To do",
      "tags: a, b"
    )
  )

  expect_output(print(plugin), "Class: blockr.test-notes_plugin")
  expect_identical(str_value(plugin), "<notes_plugin> \"Todo\"")
  expect_output(str(plugin), "<notes_plugin> \"Todo\"")
})
