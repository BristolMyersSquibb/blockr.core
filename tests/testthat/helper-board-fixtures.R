# The fixtures of boards that app scripts build, in inst/board/fixtures, which
# both test suites read.
# Each is a board as constructor calls, here, which gives every id, so that
# none is drawn at random, and the JSON board that `blockr_ser()` writes for it,
# with a description beside it.
# R's suite checks the JSON board, and the client's opens it and saves it as it
# is.
# To write the fixtures again after a change, run write_board_fixtures() with
# the package loaded by pkgload::load_all().

board_fixture_dir <- function() {
  system.file("board", "fixtures", package = "blockr.core")
}

board_fixtures <- function() {
  sub("\\.json$", "", list.files(board_fixture_dir(), pattern = "\\.json$"))
}

board_fixture <- function(name) {
  file <- file.path(board_fixture_dir(), paste0(name, ".json"))
  paste(readLines(file, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

# The block classes of the fixtures, defined outside a package, as an app
# script defines them, which name their UI modules in the fixtures' modules/.
# There, only the schema of the join block's state exists, since no test loads
# a module.
fixture_classes <- list(
  letters = new_block_class(
    "letters_block",
    expr = function(n = 3L) {
      bbquote(data.frame(x = letters[seq_len(.(n))]))
    },
    ui = js_module(
      file.path(board_fixture_dir(), "modules", "letters-block.js"),
      package = NULL
    )
  ),
  join = new_block_class(
    "join_block",
    inputs = c("x", "y"),
    expr = function(by = character(), all_x = FALSE) {
      bbquote(merge(.(x), .(y), by = .(by), all.x = .(all_x)))
    },
    ui = js_module(
      file.path(board_fixture_dir(), "modules", "join-block.js"),
      package = NULL
    )
  )
)

# Gives packages that no library holds, such as those a test declares for,
# these versions until the calling test ends.
local_versions <- function(..., frame = parent.frame()) {

  versions <- list(...)
  installed <- pkg_version

  testthat::local_mocked_bindings(
    pkg_version = function(pkg = parent.frame()) {

      if (is_string(pkg) && pkg %in% names(versions)) {
        return(package_version(versions[[pkg]]))
      }

      installed(pkg)
    },
    .package = "blockr.core",
    .env = frame
  )
}

# Gives the plugin class `class`, `{name}_plugin`, a method of
# `plugin_version()` that gives `version`, from a namespace of `package`, as
# that package registers it, until `frame` ends.
local_plugin_version <- function(class, version, package = "blockr.test",
                                 frame = parent.frame()) {
  local_method("plugin_version", class, version, package, frame)
}

# A plugin constructor of `package`, which makes plugins of the class vector
# `class` with the args of `fun`, the state that its body returns, and a name,
# as a constructor that the package defines does, from a namespace of the
# package.
plugin_ctor <- function(fun, class, package = "blockr.test") {

  ns <- list2env(
    list(.packageName = package),
    parent = asNamespace("blockr.core")
  )

  ctor <- function() {
    state <- setdiff(names(formals(sys.function())), "plugin_name")
    new_plugin(mget(state, envir = environment()), plugin_name, class)
  }

  formals(ctor) <- c(formals(fun), alist(plugin_name = NULL))
  environment(ctor) <- list2env(list(class = class), parent = ns)

  ctor
}

# Gives the board option `id` a method of `board_option_version()` that gives
# `version`, from a namespace of `package`, until `frame` ends.
local_board_option_version <- function(id, version, package = "blockr.test",
                                       frame = parent.frame()) {

  local_method(
    "board_option_version",
    paste0(id, "_option"),
    version,
    package,
    frame
  )
}

# A method of `generic` for `class` that returns `value`, from a namespace of
# `package`, until `frame` ends.
# A class given a method twice in a frame loses it in the second's cleanup,
# which runs first.
local_method <- function(generic, class, value, package, frame) {

  method <- function(x, ...) value
  environment(method) <- list2env(list(.packageName = package, value = value))

  registerS3method(generic, class, method, envir = asNamespace("blockr.core"))

  table <- get(".__S3MethodsTable__.", envir = asNamespace("blockr.core"))
  key <- paste0(generic, ".", class)

  withr::defer(
    rm(list = intersect(key, ls(table, all.names = TRUE)), envir = table),
    envir = frame
  )

  invisible()
}

# The plugin constructors of the package blockr.test of the fixtures, whose
# classes it defines in JavaScript: a frontend, of the class grid_plugin, and
# the notes of the class notes_plugin.
fixture_plugins <- list(
  grid = plugin_ctor(
    function(columns = 2L) NULL,
    c("grid_plugin", "frontend_plugin")
  ),
  notes = plugin_ctor(function(notes = list()) NULL, "notes_plugin")
)

# What the package blockr.test of the fixtures declares from its `.onLoad()`:
# the module blockr.test-grid, the UI of its frontend, and a startup module,
# beside which the package ships the schema of the state of its plugin class
# notes_plugin, in notes_plugin.schema.json, its files here in the fixtures'
# blockr.test/, as it would install them, at the version 0.1.0.
# Its method of `plugin_version()` gives that class version 2.
# The block registry and what packages declare are restored once `frame` ends,
# so that the classes that writing a fixture registers go too.
local_fixture_package <- function(frame = parent.frame()) {

  envs <- list(block_registry, declared_modules, declared_startup_modules)
  saved <- lapply(envs, as.list, all.names = TRUE)

  withr::defer(
    for (i in seq_along(envs)) {
      rm(list = ls(envs[[i]], all.names = TRUE), envir = envs[[i]])
      list2env(saved[[i]], envir = envs[[i]])
    },
    envir = frame
  )

  register_module("grid", js_module("js/grid.js", package = "blockr.test"))

  register_startup_module(js_module("js/startup.js", package = "blockr.test"))

  local_plugin_version("notes_plugin", 2L, frame = frame)

  local_versions(blockr.test = "0.1.0", frame = frame)

  installed <- package_file

  testthat::local_mocked_bindings(
    package_file = function(path, package) {

      if (!identical(package, "blockr.test")) {
        return(installed(path, package))
      }

      path <- file.path(board_fixture_dir(), "blockr.test", path)

      if (file.exists(path)) path else ""
    },
    .package = "blockr.core",
    .env = frame
  )

  invisible()
}

# The R values of each fixture, by its file name: a description and the board
# as constructor calls.
board_fixture_values <- function() {

  new_letters_block <- block_constructor(fixture_classes$letters)
  new_join_block <- block_constructor(fixture_classes$join)

  list(
    board = list(
      description = paste(
        "A board that an app script builds, with every id given, so that",
        "none is drawn at random. Its block classes are defined outside a",
        "package, so their registry ids are their names, and it holds no",
        "plugin, so the JSON board records no package. The UI of the join",
        "block ships the schema of its state. R writes that block's by, a",
        "vector of length one, as an array, and its all_x as a value, while",
        "it writes the state of the letters blocks bare. The stack of one",
        "block keeps its blocks an array, by the schema of a JSON board, and",
        "the value of show_conditions is an array, by the schema that core",
        "ships beside its runtime, recorded with the version of its shape, as",
        "each board option's value is. The stack has a color, a hex color",
        "that front-ends draw it in. The block b holds a locked of false,",
        "which leaves it open under the board lock, while the blocks a and c",
        "hold null, which R writes for a lock of NA and which follows the",
        "board lock. The client opens the JSON board and saves it as it is."
      ),
      board = new_board(
        blocks = c(
          a = new_letters_block(block_name = "Letters"),
          b = new_letters_block(
            n = 5L,
            block_name = "More letters",
            locked = FALSE
          ),
          c = new_join_block(by = "x", block_name = "Join")
        ),
        links = links(
          ac = new_link("a", "c", "x"),
          bc = new_link("b", "c", "y")
        ),
        stacks = stacks(s1 = new_stack("a", "Input", color = "#66c2a5")),
        options = c(
          new_n_rows_option(25L),
          new_show_conditions_option("error")
        ),
        id = "curious_otter",
        name = "Sales review"
      )
    ),
    plugins = list(
      description = paste(
        "A board that holds three plugins of the package blockr.test, which",
        "defines their classes in JavaScript. The plugin grid, of the class",
        "blockr.test-grid_plugin, is a frontend. It takes its id from its",
        "class, since the list of the board's plugins gives it no name. The",
        "plugins check and later are two plugins of the class",
        "blockr.test-notes_plugin, each under its name in that list. The",
        "package ships the schema of the state of its notes class beside its",
        "startup module, in a file named by the class in R, here",
        "blockr.test/js/notes_plugin.schema.json. By that schema, R writes the",
        "notes of both plugins as arrays, and the blocks of each note as an",
        "array, whatever the plugin's id. It writes the state of grid, whose",
        "class the package gives no schema, bare. The package gives the notes",
        "class version 2 through its method of plugin_version(), and the",
        "grid class has no such method, so the board records it at version 1.",
        "R records blockr.test among the board's packages, as the ids of the",
        "plugins' classes name it. A client keeps the plugins of a class only",
        "where blockr.test registers the class, and drops them otherwise."
      ),
      board = new_board(
        blocks = c(a = new_letters_block(block_name = "Letters")),
        plugins = list(
          fixture_plugins$grid(plugin_name = "Grid"),
          check = fixture_plugins$notes(
            notes = list(list(text = "To check", blocks = "a")),
            plugin_name = "Check"
          ),
          later = fixture_plugins$notes(plugin_name = "Later")
        ),
        id = "quiet_heron",
        name = "Grid"
      )
    )
  )
}

write_board_fixtures <- function(dir = file.path("inst", "board",
                                                 "fixtures")) {

  local_fixture_package()

  values <- board_fixture_values()

  for (name in names(values)) {

    fixture <- c(
      list(description = values[[name]]$description),
      blockr_ser(values[[name]]$board)
    )

    writeLines(
      json_str(fixture, auto_unbox = TRUE, pretty = TRUE),
      file.path(dir, paste0(name, ".json")),
      useBytes = TRUE
    )
  }

  invisible(dir)
}
