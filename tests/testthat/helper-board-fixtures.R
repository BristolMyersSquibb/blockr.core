# The fixtures of boards that app scripts build, in inst/board/fixtures, which
# both test suites read. Each is a board as constructor calls, here, which gives
# every id, so that nothing is minted, the JSON that `blockr_ser()` writes for
# it, in the fixture's `board`, and the file that the client writes for the
# board it opens that JSON to, in its `file`. R's suite checks the JSON, and
# the client's opens it and writes the file. To write the fixtures again after
# a change, run write_board_fixtures() with the package loaded by
# pkgload::load_all().

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

# What the package blockr.test of the fixtures declares from its `.onLoad()`:
# the module blockr.test-grid, the front-end that its board class
# `grid_board` names in JavaScript, and a startup module, beside which
# the package ships the schemas of the properties that `grid_board` adds,
# here in the fixtures' blockr.test/, as it would install them.
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

  installed <- module_file

  testthat::local_mocked_bindings(
    module_file = function(module, suffix) {

      if (!identical(module[["package"]], "blockr.test")) {
        return(installed(module, suffix))
      }

      path <- paste0(sub("\\.[^./]*$", "", module[["path"]]), suffix)
      path <- file.path(board_fixture_dir(), "blockr.test", path)

      if (file.exists(path)) path else ""
    },
    .package = "blockr.core",
    .env = frame
  )

  invisible()
}

# The R values of each fixture, by its file name: a description, the board as
# constructor calls and the versions of the packages that the client records
# in the file it writes.
board_fixture_values <- function() {

  new_letters_block <- block_constructor(fixture_classes$letters)
  new_join_block <- block_constructor(fixture_classes$join)

  core <- list(blockr.core = as.character(pkg_version("blockr.core")))

  list(
    board = list(
      description = paste(
        "A board of core's class board, which an app script builds, with",
        "every id given, so that the client mints none. Its block classes are",
        "defined outside a package, so their registry ids are their names.",
        "The UI of the join block ships the schema of its state. R writes",
        "that block's by, a vector of length one, as an array, and its all_x",
        "as a value, while it writes the state of the letters blocks bare.",
        "The stack of one block keeps its blocks an array, by the board's",
        "schema, and show_conditions is an array, by the schema that core",
        "ships beside its runtime. The client keys the collections by id as",
        "it opens the board, and the fixture's file is the file that the",
        "client then writes."
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
      ),
      packages = core
    ),
    `board-class` = list(
      description = paste(
        "A board of the class grid_board, which extends core's board and",
        "which the package blockr.test defines in JavaScript, with the",
        "properties that the class adds beside core's, panels and active, as",
        "the script gives them. The package ships the schemas of those",
        "properties beside its startup module, here in",
        "blockr.test/js/startup.schema.json. By them, R writes the blocks of",
        "each entry of panels as an array, and active as a string. A client",
        "builds the board as a grid_board, keeping both properties, only",
        "where blockr.test defines the class, and as core's board otherwise,",
        "without them."
      ),
      board = new_board(
        blocks = c(a = new_letters_block(block_name = "Letters")),
        panels = list(main = list(title = "Main", blocks = "a")),
        active = "main",
        id = "quiet_heron",
        name = "Grid",
        class = "grid_board"
      ),
      packages = c(core, list(blockr.test = "0.1.0"))
    )
  )
}

# The file that the client writes for the board it opens a fixture's JSON to:
# the board with its collections keyed by id, under `board`, beside the file's
# `format` and the versions of the packages that the board's parts come from.
board_fixture_file <- function(board, packages) {

  by_id <- function(entries) {
    set_names(
      lapply(entries, function(x) x[names(x) != "id"]),
      chr_xtr(entries, "id")
    )
  }

  cols <- c("blocks", "links", "stacks")
  board[cols] <- lapply(board[cols], by_id)

  list(format = 1L, packages = packages, board = board)
}

write_board_fixtures <- function(dir = file.path("inst", "board",
                                                 "fixtures")) {

  local_fixture_package()

  values <- board_fixture_values()

  for (name in names(values)) {

    board <- blockr_ser(values[[name]]$board)

    fixture <- list(
      description = values[[name]]$description,
      board = board,
      file = board_fixture_file(board, values[[name]]$packages)
    )

    writeLines(
      json_str(fixture, auto_unbox = TRUE, pretty = TRUE),
      file.path(dir, paste0(name, ".json")),
      useBytes = TRUE
    )
  }

  invisible(dir)
}
