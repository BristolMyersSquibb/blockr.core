#' Serve a board or a block
#'
#' The generic `serve()` starts an app, as an app script does at its top level.
#' Its methods take a board, the path of a JSON board, or a single block, and
#' return a Shiny app.
#' The app's page holds core's client, which draws the board with the app's
#' frontend.
#' The app's server function hands what the client sends to the evaluator of
#' the session, which evaluates the blocks.
#'
#' Which block classes an app offers is the app's choice, never a board's.
#' The methods for a board and for a path take them in `blocks`, which
#' defaults to every block class of every available package, as
#' [available_blocks()] gives them.
#' A script narrows them with plain R, as
#' `subset(available_blocks(), package != "blockr.ggplot")` offers every class
#' but those of blockr.ggplot, or gives the path of a YAML file that lists them
#' (see [register_block()]).
#' As `serve()` is called, R loads the packages of these classes and builds the
#' app's own block registry from them, from the classes that the app script
#' defined and registered itself (see [register_block()]) and from the board
#' (below).
#' The app's sessions read that registry alone, so a later `serve()` or
#' `register_block()` changes no app that `serve()` built before.
#' A class that no available package lists stops the app, naming it.
#'
#' Which plugins run is the app's choice, never a board's.
#' The methods for a board and for a path take the app's plugins in `plugins`,
#' which run on every board that the app opens, the opening board among them,
#' each with the state that the board holds for it.
#' They default to every plugin class that an available package lists, as
#' [available_plugins()] gives them.
#' A script narrows them with plain R, as
#' `subset(available_plugins(), package != "blockr.session")` runs every
#' plugin but those of blockr.session.
#' Core's basic frontend runs only where no other row is a frontend, and core's
#' storage plugin only where no other row is a storage plugin (see
#' [new_plugin()]).
#' A row that names a class that no available package lists stops the app,
#' naming it, and so do rows that name one class twice, or that then hold no
#' frontend, two frontends or two storage plugins.
#' A board that the script builds gives only the state of its plugins, and a
#' board with a plugin of a class that the app does not run stops the app too,
#' naming the class.
#' A block that `serve()` serves alone runs no plugin.
#'
#' The methods for a board and for a path hand every argument in `...` to the
#' app's frontend, as the dock takes the items of its bar in `navbar`.
#' As `serve()` is called, R hands them to the method of [frontend_args()] for
#' the class of the app's frontend.
#' The method refuses each argument that the frontend does not know, naming
#' the frontend.
#' Core's basic frontend knows none, so an app that runs it and passes
#' `navbar` fails as `serve()` is called.
#' R sends what the method returns in its reply to `hello`, so that every
#' board that the app opens gets the same.
#'
#' For a board, R writes the board's JSON once, as `serve()` is called, and
#' sends it to the client as each session starts.
#' So a board that R cannot write as JSON fails as the script runs (see
#' [blockr_ser()]).
#' Before it writes the board, R registers each class of the board's blocks
#' that the app's block registry lacks, from the class's registration (see
#' [register_block()]): from the block registry of the process where that
#' holds the class, and from the listing of the class's package otherwise.
#' So every block on the served board runs, with the name and the other
#' metadata that its class's registration gives, and so does a block that
#' `serve()` serves alone.
#' The app offers none of these classes, so that the client's pickers leave
#' them out.
#' A board with a block of a class that nothing registers fails, naming the
#' block and the class.
#' So a script registers a class that it defines itself with
#' [register_block()] before it serves a board with a block of the class.
#' A class from a package keeps its package's ID, so that a board saved from
#' the app opens in an app that offers the package.
#' A class from outside a package, whose registry ID is its id alone,
#' replaces another class that the script registered under that ID, and keeps
#' the metadata that the script registered.
#' So a script that redefines a class after it registered it runs the new
#' definition.
#' A board with blocks of two such classes of one id fails, since the client
#' and the evaluator know a block's class by its registry ID alone.
#' For the path of a JSON board, R reads only the file's format, and sends the
#' file's text as it is, for the client to open.
#' It loads no package for the file.
#' The board's ID, the state of its plugins and its board options come from
#' the board (see [new_board()]), and the client reads the query of the page's
#' URL itself.
#' Where the board lock and the load lock are both on, R reads the board's
#' blocks and links once, as `serve()` is called, and holds every evaluation
#' to them.
#'
#' A single block is served alone, with fixed data and no board around it, so
#' that its developer can try it out.
#' Its app offers no block classes, so that its block registry holds the
#' block's class alone, which R registers from the class's registration, as
#' for a board.
#' The page shows the block in a card of its own: its name, its UI, its output
#' and a line of its status.
#' R keeps the data for the process, and binds each entry to the block's input
#' of that name.
#' For a variadic block, an entry whose name is none of the block's inputs
#' goes into the block's `...`, under that name.
#' So each entry needs a name, as in
#' `serve(new_rbind_block(), a = iris, b = mtcars)`.
#'
#' Like its block registry and its plugins, the app holds the board options
#' that the available packages list, and the modules that they offer each
#' other, as `serve()` read them (see [read_blocks_yml()]).
#' So the replies to `hello` name the same ones wherever the app runs,
#' including in another R process, as when a script hands the app to
#' `callr::r_bg()`, or shinytest2 runs it.
#' The app stops where one of its components reads a board option that no
#' available package lists, and the error names the component and the board
#' option.
#' Those components are the UI module of each block class in the app's block
#' registry, the class's requests and the output families that make its
#' outputs, and each plugin that the app runs.
#'
#' @section End-to-end tests:
#' In Shiny's test mode, as the package shinytest2 starts an app, the app gives
#' end-to-end tests what they check.
#' R exports `result` through [shiny::exportTestValues()], which a test reads
#' with `AppDriver$get_values(export = TRUE)`.
#' For a board, `result` holds the latest result of each block, by block ID.
#' For a block that `serve()` serves alone, `result` is the block's result.
#' R holds no result of a block whose latest evaluation failed, so `result`
#' leaves that block out.
#' The client holds the board and each block's status.
#' It also holds three parts of what a frontend offers other plugins: which
#' block has focus, which blocks and plugins are on screen, and the
#' notifications.
#' So the page puts the object `blockrTest` on `window`, which a test reads
#' with `AppDriver$get_js()`.
#' Its function `board()` gives a copy of the board that the client holds, and
#' its function `statuses()` gives each block's status by block ID, as in
#' `app$get_js("blockrTest.statuses()")`.
#' Its function `focus()` gives the ID of the block that has focus, or `null`
#' where none has.
#' Its function `onScreen()` gives the IDs of the blocks and the plugins on
#' screen, in `blocks` and `plugins`, or `null` until the frontend sets them.
#' Its function `notifications()` gives the notifications that hold, in the
#' order they came.
#' Its function `show()` has the frontend show a block or a plugin, as a
#' plugin asks it to, as in `app$get_js("blockrTest.show({ block: 'a' })")`.
#' Showing takes no undo step, and under core's basic frontend it changes
#' nothing on the board.
#' Another frontend may change the state that the board holds for it, as the
#' dock does where it brings a panel forward.
#'
#' @param x A board, the path of a JSON board, or a block
#' @param ... For a board or the path of a JSON board, the arguments of the
#'   app's frontend, which R hands to the method of [frontend_args()] for the
#'   frontend's class.
#'   For a block, its data where `data` is empty.
#'   R ignores it otherwise, with a warning.
#' @param blocks The block classes that the app offers, as rows with the
#'   class's package in `package` and its id in `id`, such as those of
#'   [available_blocks()], or as the path of a YAML file that lists them
#' @param plugins The plugins that the app runs, as rows with the package of
#'   each plugin's class in `package` and the class's id in `id`, such as those
#'   of [available_plugins()]
#'
#' @examples
#' if (interactive()) {
#'   serve(
#'     new_board(
#'       blocks = c(
#'         a = new_dataset_block("BOD"),
#'         b = new_dataset_block("ChickWeight"),
#'         c = new_merge_block("Time")
#'       ),
#'       links = c(
#'         ac = new_link("a", "c", "x"),
#'         bc = new_link("b", "c", "y")
#'       )
#'     )
#'   )
#'
#'   serve(
#'     new_merge_block("Time"),
#'     data = list(x = datasets::BOD, y = datasets::ChickWeight)
#'   )
#' }
#'
#' @return A Shiny app, as [shiny::shinyApp()] returns it.
#'
#' @export
serve <- function(x, ...) {
  UseMethod("serve")
}

#' @rdname serve
#' @export
serve.board <- function(x, ..., blocks = available_blocks(),
                        plugins = available_plugins()) {
  plugins <- app_plugins(plugins)
  check_board_plugins(x, plugins)
  registry <- app_block_registry(blocks)
  offered_ids <- ls(registry)
  register_board_classes(x, registry)
  frontend <- frontend_reply(..., plugins = plugins)
  serve_app(
    encode_message(blockr_ser(x)),
    registry,
    offered_ids,
    plugins = plugins,
    frontend = frontend
  )
}

#' @rdname serve
#' @export
serve.character <- function(x, ..., blocks = available_blocks(),
                            plugins = available_plugins()) {
  plugins <- app_plugins(plugins)
  registry <- app_block_registry(blocks)
  frontend <- frontend_reply(..., plugins = plugins)
  serve_app(
    read_board_json(x),
    registry,
    ls(registry),
    plugins = plugins,
    frontend = frontend
  )
}

#' @param data The block's data, a list by input
#' @rdname serve
#' @export
serve.block <- function(x, ..., data = list()) {

  dots <- coal(...names(), character(...length()), fail_all = FALSE)

  if (length(dots) && length(data)) {
    dots <- arg_labels(dots)
    blockr_warn(
      "R takes the block's data from `data`, and ignores {dots} in `...`. ",
      "It takes data from `...` only where `data` is empty.",
      class = "serve_dots_ignored"
    )
  } else if (length(dots)) {
    data <- list(...)
  }

  check_block_data(x, data)

  board <- new_board(x)

  registry <- new.env(parent = emptyenv())
  register_board_classes(board, registry)

  serve_app(
    encode_message(blockr_ser(board)),
    registry,
    character(),
    data,
    alone = TRUE
  )
}

# Refuses a board that the script builds with a plugin of a class
# that the app does not run, since the board would open without that plugin,
# naming the class of each such plugin.
# The app's plugins are given by the IDs of their classes.
check_board_plugins <- function(board, plugins) {

  unknown <- setdiff(names(board_plugins(board)), names(plugins))

  if (length(unknown)) {
    blockr_abort(
      "The board holds {qty(unknown)}{?a plugin/plugins} of the ",
      "class{?es} {unknown}, which the app does not run, so that the board ",
      "would open without {?it/them}.",
      class = "board_plugins_invalid"
    )
  }

  invisible()
}

# What the app's frontend reads of the arguments that `serve()` does not take
# itself, as JSON, which R sends in `frontend` in its reply to `hello`.
# The app's plugins, as `app_plugins()` gives them, hold one frontend. R finds
# it as `app_plugins()` does, by the R class of the plugin that its class's
# constructor makes, and calls the method of `frontend_args()` on that plugin.
# R checks what the method returns and writes it as a JSON object, once, as
# `serve()` is called. So a method that returns anything but a list by name
# stops the app then, naming the frontend.
# The app's plugins come after `...`, since R would match an argument of
# `serve()` named `p` to a `plugins` before it.
frontend_reply <- function(..., plugins) {

  available <- available_plugin_classes()
  plugins <- lapply(set_names(nm = names(plugins)), plugin_of_class, available)

  front <- plugins[lgl_ply(plugins, is_frontend_plugin)][[1L]]
  args <- frontend_args(front, ...)

  if (!is.list(args) || is.object(args) || !has_unique_names(args)) {
    blockr_abort(
      "The method of `frontend_args()` for the frontend ",
      "{plugin_class_id(front)} returns no list by name, which R writes as ",
      "a JSON object.",
      class = "frontend_args_invalid"
    )
  }

  tryCatch(
    encode_message(as_json_object(args)),
    error = function(e) {
      blockr_abort(
        "The method of `frontend_args()` for the frontend ",
        "{plugin_class_id(front)} returns a list that R cannot write as ",
        "JSON: {conditionMessage(e)}",
        class = "frontend_args_invalid"
      )
    }
  )
}

# Registers the class of each block on a board that an app's block registry,
# `registry`, lacks, from the class's registration (see `register_class()`), so
# that every block on the served board runs, with the metadata that the
# registration gives. A class from a package keeps its package's id.
# A board with a block of a class that has no registration stops the app,
# naming each such block and its class, as for a class that the app script
# defined and did not register.
# A class from outside a package has no package that checks the ids of its
# classes, so R checks it here. It replaces another class that the registry
# holds under its id, as a class does that the script redefined after it
# registered it, and keeps the metadata that the script registered.
# Two such classes of one id on a board fail.
register_board_classes <- function(board, registry) {

  classes <- lapply(as.list(board_blocks(board)), block_class)

  check_outside_classes(classes)

  unregistered <- character()

  for (blk in names(classes)) {

    cls <- classes[[blk]]
    id <- class_registry_id(cls)

    held <- exists(id, envir = registry, inherits = FALSE) ||
      register_class(cls, registry)

    if (!held) {
      unregistered <- c(unregistered, paste0(blk, " (", id, ")"))
      next
    }

    old <- get_registry_entry(id, registry)

    if (is.null(cls[["package"]]) && !identical(old[["block_class"]], cls)) {
      assign(id, class_entry(cls, entry_metadata(old)), envir = registry)
    }
  }

  if (length(unregistered)) {
    blockr_abort(
      "{qty(unregistered)}The board holds {?a block/blocks} {unregistered} ",
      "of {?a class/classes} that nothing registers. A package lists its ",
      "block classes, and an app script registers a class of its own with ",
      "`register_block()` before it serves a board with a block of the class.",
      class = "board_classes_unregistered"
    )
  }

  invisible()
}

# Two classes from outside a package with one id give one registry id, the
# class id, under which the client and the evaluator know only one class. The
# classes are given by block id.
check_outside_classes <- function(classes) {

  outside <- lgl_ply(classes, function(cls) is.null(cls[["package"]]))
  ids <- chr_ply(classes, class_registry_id)

  for (id in unique(ids[outside])) {

    same <- classes[outside & ids == id]
    other <- names(same)[!lgl_ply(same, identical, same[[1L]])]

    if (length(other)) {
      blockr_abort(
        "Blocks {names(same)[1L]} and {other[1L]} are of two classes with the ",
        "id {id}, defined outside a package, which give one registry id.",
        class = "board_blocks_invalid"
      )
    }
  }

  invisible()
}

# The app that `serve()` returns, for the opening board given as JSON, and
# for a block that `serve()` serves alone, with the data that R binds to the
# block's inputs.
# Each session's evaluator reads the app's block registry, `registry`, alone.
# Its reply to `hello` marks the entries of the classes that the app offers,
# by their registry ids, `offered_ids`, which a block that `serve()` serves
# alone leaves empty.
# The app's plugins, `plugins`, which `app_plugins()` gives, go to the
# evaluator of every session, which sends them in its reply to `hello`, and so
# does what the app's frontend reads of the arguments of `serve()`,
# `frontend`, as `frontend_reply()` gives it, an empty JSON object for a block
# that `serve()` serves alone.
# So do the board options, the modules and the output families that the
# available packages list, which R reads here, so that the replies name the
# same ones in whatever process runs the app.
# A component of the app that reads a board option that no available package
# lists stops the app here (see `check_component_options()`).
# R writes or reads the opening board here, as `serve()` is called, rather than
# as a session starts, and so reads it for the locks.
# Every session's evaluator holds its evaluations to what R read.
# The server function exports the latest results of the session's evaluator
# for end-to-end tests.
# Shiny answers for exported values only in test mode.
# Core drops what it keeps about logging, so that the app logs as its script
# set the blockr options, whatever core logged before.
# It works out the logger afresh, so that a logger that cannot work stops the
# app here, and so does a logger of the client's that cannot (see
# `write_log()`).
# R also works out here which modules the page imports as it starts, which the
# page names for the browser to fetch at once (see `page_modules()`).
serve_app <- function(board, registry, offered_ids, data = list(),
                      alone = FALSE, plugins = list(),
                      frontend = as_json_object(list())) {

  reset_logging()
  get_logger()
  check_client_logger()

  force(board)
  force(offered_ids)

  lock <- lock_board(board, data, registry)

  # Two packages that list one board option, or a family of one class, stop
  # the app here
  options <- app_options()
  offered <- offered_modules()
  families <- app_families()

  check_component_options(registry, plugins, options, families)

  modules <- page_modules(board, registry, plugins, options, families, offered)
  dark_mode <- board_dark_mode(board)

  shiny::shinyApp(
    ui = function(req) {
      serve_page(
        alone,
        modules = modules,
        color_scheme = page_color_scheme(dark_mode)
      )
    },
    server = function(input, output, session) {

      ev <- new_evaluator(
        board, data, lock, registry,
        offered_ids = offered_ids,
        plugins = plugins,
        frontend = frontend,
        options = options,
        modules = offered,
        families = families,
        session_id = session$token
      )

      shiny::exportTestValues(result = latest_results(ev, alone))

      shiny_transport(
        function(message, send) receive_requests(ev, message, send),
        session
      )
    }
  )
}

# Stops the app where one of its components reads a board option that no
# available package lists, since the client then draws or runs nothing that
# requires it.
# The error names each such component, with each board option that it reads
# and that no available package lists.
# The board options that the available packages list are `options`, by ID.
# The components come from the app's block registry, `registry`, its plugins,
# `plugins`, and the output families that the available packages list,
# `families` (see `component_options()`).
check_component_options <- function(registry, plugins, options, families) {

  read <- component_options(registry, plugins, families)

  faults <- unlst(
    Map(
      function(ids, what) {
        unknown <- setdiff(ids, names(options))
        if (length(unknown)) {
          glue_plur(
            "{what} reads {qty(unknown)}the board option{?s} {unknown}, ",
            "which no available package lists."
          )
        }
      },
      read,
      names(read)
    )
  )

  if (length(faults)) {
    faults <- paste(faults, collapse = " ")
    blockr_abort("{faults}", class = "board_options_unknown")
  }

  invisible()
}

# The IDs of the board options that each component of the app reads, by the
# component, as an error names it.
# The components are the UI module of each block class of the app's block
# registry, `registry`, each request of such a class, each output family that
# such a class reaches and the plugin of each class that the app runs,
# `plugins`, as `app_plugins()` gives them.
# An open family reaches the families that the available packages list,
# `families`, as it does for the entry of its block class (see
# `family_option_ids()`).
component_options <- function(registry, plugins, families) {

  entries <- registry_entries(registry)

  classes <- Map(
    function(x, id) {
      reqs <- x[["requests"]]
      c(
        set_names(
          list(option_ctor_ids(x[["block_class"]][["options"]])),
          paste("The UI module of block class", id)
        ),
        set_names(
          lapply(unname(reqs), function(req) unname(req[["options"]])),
          sprintf("The request %s of block class %s", names(reqs), id)
        )
      )
    },
    entries,
    names(entries)
  )

  reached <- unlst(
    lapply(
      unname(entries),
      function(x) reached_families(x[["block_class"]][["output"]], families)
    ),
    use_names = TRUE
  )

  reached <- reached[!duplicated(names(reached))]

  c(
    unlst(unname(classes), use_names = TRUE),
    set_names(
      lapply(unname(reached), function(x) option_ctor_ids(attr(x, "options"))),
      sprintf("Output family %s", names(reached))
    ),
    set_names(
      lst_xtr(unname(plugins), "options"),
      sprintf("The plugin of class %s", names(plugins))
    )
  )
}

# The modules that the page imports as it starts, for the opening board given
# as JSON.
# Core imports the module of each of the app's plugins, `plugins`, and of each
# of its board options, `options`, before it opens the board.
# It also imports the module that draws the menus of board actions, among the
# modules that the available packages offer, `offered` (see `menu_module()`).
# The frontend then imports the UI module of the class of each block on the
# board, and the module of the class's renderer, from the app's block
# registry, `registry`.
# A class whose output family is open has the renderers of the families that
# the available packages list, `families`.
# A block whose class the registry lacks adds nothing, since the board fails
# to open with it.
page_modules <- function(board, registry, plugins = list(),
                         options = app_options(), families = app_families(),
                         offered = offered_modules()) {

  json <- if (inherits(board, "json")) board else encode_message(board)
  blocks <- decode_message(json)[["board"]][["blocks"]]

  ids <- unique(as.character(unlst(lst_xtr(blocks, "registry_id"))))
  ids <- intersect(ids, ls(envir = registry))

  entries <- lapply(ids, get_registry_entry, registry)
  renderers <- unlist(
    lapply(entries, entry_renderers, families),
    recursive = FALSE
  )

  modules <- c(
    lst_xtr(plugins, "module"),
    lst_xtr(options, "module"),
    list(menu_module(offered)),
    lst_xtr(entries, "block_class", "ui"),
    lst_xtr(renderers, "module")
  )

  unique(Filter(not_null, unname(modules)))
}

# The module that draws the menus of board actions, which the blockr option
# `board_action_menu` names by its ID, among the modules that the available
# packages offer, `offered`.
# The option names core's own menu, `blockr.core-board_action_menu`, unless an
# app names another, as the client reads it.
# It is `NULL` where the option names no module that a package offers, or
# holds no ID, as where it is set twice, since the client then opens no board
# and says why.
menu_module <- function(offered = offered_modules()) {

  id <- tryCatch(
    blockr_option("board_action_menu", "blockr.core-board_action_menu"),
    error = function(e) NULL
  )

  if (!is_nonempty_string(id)) {
    return(NULL)
  }

  offered[[id]]
}

# The value of the board option `dark_mode` that the opening board given as
# JSON holds, or `NULL` where it holds none.
board_dark_mode <- function(board) {
  json <- if (inherits(board, "json")) board else encode_message(board)
  decode_message(json)[["board"]][["options"]][["dark_mode"]][["value"]]
}

# The theme that the page takes as it loads, as the content of its meta tag
# `color-scheme`: the theme of the board option `dark_mode`, where R knows it.
# That is the opening board's value, `value`, or else the default that the
# blockr option `dark_mode` gives, which R reads as the client reads it.
# TRUE or FALSE, as a logical or as text, stands for dark or light, and the
# text "light" or "dark" for itself.
# Where R knows neither, the page takes the theme that the user's system
# prefers, as `dark_mode` does then, and so it does where the blockr option is
# set twice, or to a value that the board option does not take, which the
# client reports as it reads it.
# A board that the storage plugin opens in place of the opening board may hold
# another value, which R cannot know as it makes the page.
page_color_scheme <- function(value = NULL) {

  if (is.null(value)) {
    value <- tryCatch(
      blockr_option("dark_mode", NULL),
      error = function(e) NULL
    )
  }

  if (is_string(value) && !is.na(as.logical(trimws(value)))) {
    value <- as.logical(trimws(value))
  }

  if (isTRUE(value)) {
    return("dark")
  }

  if (isFALSE(value)) {
    return("light")
  }

  if (is_string(value) && value %in% c("light", "dark")) {
    return(value)
  }

  "light dark"
}

# The latest result of each block that the evaluator holds one for, by block
# ID, as R exports them for end-to-end tests.
# For a block that `serve()` serves alone, it is that block's result,
# or `NULL` before R holds one.
# R holds no result of a block whose latest evaluation failed.
latest_results <- function(ev, alone = FALSE) {

  ids <- ls(ev$results, all.names = TRUE)
  res <- lst_xtr(mget(ids, envir = ev$results), "result")

  if (alone) {
    return(if (length(res)) res[[1L]])
  }

  res
}

# The page of the app that `serve()` returns, which holds no Bootstrap.
# A browser takes an import map only ahead of the page's first module script,
# so the page names the map's dependency first, and Shiny puts it in the head
# after its own scripts, which are no modules.
# The page then holds core's stylesheet, the element that the client draws
# in, and the module script that starts the client with `startPage()`.
# The element holds the loading note from the start, so that the page shows it
# before the client's code has loaded, which the client keeps until the board
# opens.
# Until then, the page takes the theme of the board option `dark_mode` where R
# knows it, or else the theme that the user's system prefers, as `dark_mode`
# does by default, in `color_scheme` (see `page_color_scheme()`).
# So a dark board opens on a dark page rather than on a light page that turns
# dark.
# Its head names the files that the page imports as it starts, the `modules`
# given among them, for the browser to fetch at once (see `preload_links()`).
# R makes the page for each request, so that its URLs name the files that R
# serves now.
# In Shiny's test mode, as shinytest2 starts an app, the module script also
# imports `blockrTest()` from core's module for end-to-end tests, and puts what
# it returns on `window`, for the tests to use.
# A page outside test mode loads none of that module.
serve_page <- function(alone = FALSE,
                       test = isTRUE(shiny::getShinyOption("testmode")),
                       modules = list(), color_scheme = "light dark") {

  runtime <- served_module(core_runtime())

  script <- page_script(
    runtime[["module"]],
    alone,
    if (test) {
      served_url(js_module("js/blockr-test.js", package = "blockr.core"))
    }
  )

  shiny::tagList(
    import_map_dependency(),
    shiny::tags$head(
      shiny::tags$meta(name = "color-scheme", content = color_scheme),
      shiny::tags$link(rel = "stylesheet", href = runtime[["css"]]),
      preload_links(runtime[["module"]], modules)
    ),
    shiny::tags$div(id = "blockr-page", loading_note()),
    shiny::tags$script(type = "module", shiny::HTML(script))
  )
}

# The loading note, as the client's `loadingNote()` makes it
# (srcts/core/frontends.ts): a spinner over the line "Loading the board…",
# which core's stylesheet draws.
loading_note <- function() {
  shiny::tags$div(
    role = "status",
    class = "bc:loading-note",
    shiny::tags$progress(
      class = "bc:spinner",
      `aria-label` = "Loading the board"
    ),
    shiny::tags$span(`aria-hidden` = "true", "Loading the board\u2026")
  )
}

# The import map as an HTML dependency, which puts it in the page's head.
import_map_dependency <- function() {
  htmltools::htmlDependency(
    "blockr-import-map",
    as.character(utils::packageVersion("blockr.core")),
    src = c(href = ""),
    head = as.character(import_map())
  )
}

# The module script that imports `startPage()` from core's runtime, at the URL
# given, and starts the client in the page's element.
# The client draws a block that `serve()` serves alone where `alone` is `TRUE`.
# Where `test` gives the URL of core's module for end-to-end tests, the script
# also imports `blockrTest()` from it, and puts what that returns on `window`
# as `blockrTest`.
page_script <- function(runtime, alone = FALSE, test = NULL) {

  start <- paste0(
    "startPage({ el: document.getElementById(\"blockr-page\")",
    if (alone) ", alone: true" else "",
    " })"
  )

  paste0(
    "import { startPage } from \"./", runtime, "\";\n",
    if (is.null(test)) {
      paste0(start, ";\n")
    } else {
      paste0(
        "import { blockrTest } from \"./", test, "\";\n",
        "const started = ", start, ";\n",
        "window.blockrTest = blockrTest(() => started.core());\n"
      )
    }
  )
}

# Checks the classes that `serve()` takes in `blocks` or in `plugins` as the
# rows of a data frame with the character columns `package`, for the class's
# package, and `id`, for its id, and gives them back as they are, invisibly.
# The errors name the classes as `what` gives them, such as "block classes
# that an app offers", and the function that lists them, `listing`, are of the
# class `class`, and with `yaml`, say that the argument takes the path of a
# YAML file too.
check_class_rows <- function(x, what, listing, class, yaml = FALSE) {

  valid <- is.data.frame(x) && all(c("package", "id") %in% names(x)) &&
    is.character(x[["package"]]) && is.character(x[["id"]]) &&
    !anyNA(x[["package"]]) && !anyNA(x[["id"]])

  if (!valid) {
    blockr_abort(
      "The {what} are expected as a data frame with the character columns ",
      "`package` and `id`, as `{listing}` gives",
      if (yaml) ", or as the path of a YAML file" else "",
      ".",
      class = class
    )
  }

  invisible(x)
}

# The arguments of the names given, as a message names them: each name in
# backticks, or "an unnamed argument" for an argument that has none.
arg_labels <- function(nms) {
  ifelse(nzchar(nms), paste0("`", nms, "`"), "an unnamed argument")
}

# Checks the data that R binds to the inputs of a block that `serve()` serves
# alone: a list in which each entry has a name of its own.
# Each name is an input of the block, or any name for a variadic block, whose
# `...` takes the entries that name none of its inputs.
check_block_data <- function(x, data) {

  if (!is.list(data) || is.object(data)) {
    blockr_abort(
      "The data of a block that `serve()` serves alone is a list by input, as ",
      "in `data = list(data = iris)`.",
      class = "serve_data_invalid"
    )
  }

  nms <- coal(names(data), character(length(data)), fail_all = FALSE)
  unnamed <- sum(is.na(nms) | !nzchar(nms))
  inputs <- block_data_inputs(x)
  variadic <- "..." %in% inputs

  if (unnamed) {
    blockr_abort(
      "R binds each entry of a block's data to the block's input of that ",
      "name, and {unnamed} entr{?y/ies} {?has/have} no name.",
      if (variadic) {
        paste0(
          " A variadic block takes its data under any names, as in ",
          "`serve(new_rbind_block(), a = iris, b = mtcars)`."
        )
      } else {
        ""
      },
      class = "serve_data_invalid"
    )
  }

  twice <- unique(nms[duplicated(nms)])

  if (length(twice)) {
    blockr_abort(
      "R binds each entry of a block's data to the block's input of that ",
      "name, and the data names {twice} more than once.",
      class = "serve_data_invalid"
    )
  }

  unknown <- setdiff(nms, inputs)

  if (variadic || !length(unknown)) {
    return(invisible(data))
  }

  if (!length(inputs)) {
    blockr_abort(
      "The block has no inputs, so it takes no data, and it was given ",
      "{unknown}.",
      class = "serve_data_invalid"
    )
  }

  blockr_abort(
    "The block has no {qty(unknown)}input{?s} {unknown}. ",
    "Its {qty(inputs)}input{?s} {?is/are} {inputs}.",
    class = "serve_data_invalid"
  )
}
