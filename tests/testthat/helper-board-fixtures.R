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

# Gives board class `class` a method of `board_front_end()` that names
# `front_end`, from a namespace of `package`, as that package registers it,
# until `frame` ends.
local_board_front_end <- function(class, front_end, package,
                                  frame = parent.frame()) {

  ns <- list2env(list(.packageName = package, front_end = front_end))

  method <- function(x, ...) front_end
  environment(method) <- ns

  registerS3method(
    "board_front_end",
    class,
    method,
    envir = asNamespace("blockr.core")
  )

  withr::defer(
    rm(
      list = paste0("board_front_end.", class),
      envir = get(".__S3MethodsTable__.", envir = asNamespace("blockr.core"))
    ),
    envir = frame
  )

  invisible()
}

# What the package blockr.test of the fixtures declares from its `.onLoad()`:
# the module blockr.test-grid, the front-end that its board class
# `grid_board` names through its method of `board_front_end()`, and a startup
# module, beside which the package ships the schemas of the properties that
# `grid_board` adds, here in the fixtures' blockr.test/, as it would install
# them, at the version 0.1.0.
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

  local_board_front_end(
    "grid_board",
    "blockr.test-grid",
    "blockr.test",
    frame = frame
  )

  local_versions(blockr.test = "0.1.0", frame = frame)

  installed <- module_file

  testthat::local_mocked_bindings(
    module_file = function(module, suffix = NULL) {

      if (!identical(module[["package"]], "blockr.test")) {
        return(installed(module, suffix))
      }

      path <- module_path(module, suffix)
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
        "A board of core's class board, which an app script builds, with",
        "every id given, so that none is drawn at random. Its block classes",
        "are defined outside a package, so their registry ids are their",
        "names, and the board's content is core's alone. The UI of the join",
        "block ships the schema of its state. R writes that block's by, a",
        "vector of length one, as an array, and its all_x as a value, while",
        "it writes the state of the letters blocks bare. The stack of one",
        "block keeps its blocks an array, by the schema of a JSON board, and",
        "show_conditions is an array, by the schema that core ships beside",
        "its runtime. The client opens the JSON board and saves it as it is."
      ),
      board = new_board(
        blocks = c(
          a = new_letters_block(block_name = "Letters"),
          b = new_letters_block(n = 5L, block_name = "More letters"),
          c = new_join_block(by = "x", block_name = "Join", visible = "outputs")
        ),
        links = links(
          ac = new_link("a", "c", "x"),
          bc = new_link("b", "c", "y")
        ),
        stacks = stacks(s1 = new_stack("a", "Input")),
        options = c(
          new_n_rows_option(25L),
          new_show_conditions_option("error")
        ),
        id = "curious_otter",
        name = "Sales review"
      )
    ),
    `board-class` = list(
      description = paste(
        "A board of the class grid_board, which extends core's board and",
        "which the package blockr.test defines in JavaScript, with the",
        "properties that the class adds beside core's, panels and active, as",
        "the script gives them. The package ships the schemas of those",
        "properties beside its startup module, here in",
        "blockr.test/js/startup.schema.json. By them, R writes the blocks of",
        "each entry of panels as an array, and active as a string. The",
        "package names the class's front-end through its method of",
        "board_front_end(), from whose namespace R records blockr.test among",
        "the board's packages. A client builds the board as a grid_board,",
        "keeping both properties, only where blockr.test defines the class,",
        "and as core's board otherwise, without them."
      ),
      board = new_board(
        blocks = c(a = new_letters_block(block_name = "Letters")),
        panels = list(main = list(title = "Main", blocks = "a")),
        active = "main",
        id = "quiet_heron",
        name = "Grid",
        class = "grid_board"
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
