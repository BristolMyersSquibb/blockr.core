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
#' An app offers every block class of its block registry.
#' The methods for a board and for a path take those of packages in `blocks`,
#' which defaults to every block class of every available package, as
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
#' An app gives the page a theme of its own in `theme`, as
#' [blockr.ui::new_blockr_theme()] makes it.
#' A theme sets values of the theme variables, in light mode and in dark mode.
#' Each method takes the app's theme, so that a block that `serve()` serves
#' alone draws in it too.
#' The argument defaults to the blockr option `theme`.
#' Where nothing sets that option, it falls back on a theme with no values.
#' A site-wide R profile can set the option for every app that runs on its
#' installation of R, and an app's script overrides it.
#' Only an R option sets it, as `options(blockr.theme = theme)` does.
#' An environment variable gives a string, which is no theme.
#' Each method refuses a `theme` that is not a theme, naming what it got.
#'
#' The frontend gives the values that it wants its own as a theme too.
#' The methods for a board and for a path call the method of
#' [frontend_theme()] for the class of the app's frontend as `serve()` is
#' called, and a method that returns anything but a theme stops the app,
#' naming the frontend.
#' R writes the rules of the frontend's theme and then those of the app's theme
#' into the page's head, after the stylesheet of blockr.ui.
#' So the page draws in them from the first paint, its loading note included.
#' A value of the app's theme wins over the frontend's, and both win over the
#' defaults of blockr.ui.
#' A theme with no values writes no rules.
#' For each font of the two themes, R serves the font's file and writes the
#' font's face with `@font-face`.
#' R also names the file in the page's head, so that the browser fetches it as
#' the page loads.
#' Until the file has loaded, the text draws in the next face of the stack.
#' A font that gives no family or no file stops the app, and so does a font
#' whose file does not exist.
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
#' The app offers these classes too, so that a user can add a block of any
#' class that the board shows.
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
#' Its block registry holds the block's class alone, which R registers from
#' the class's registration, as for a board.
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
#' The blockr option `renderers` names the renderers that draw the outputs of
#' output families, each under the class of a family (see [new_renderer()]).
#' The app's output families are those that the available packages list and
#' those that the block classes in its block registry reach.
#' R ignores a key of the option that names no output family of the app that
#' gives a renderer, such as the typo `tabel_output`.
#' As `serve()` is called, R warns of each such key, naming it.
#' The app still serves, and draws with the renderers that it would draw with
#' anyway.
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
#' @param theme The app's theme, as [blockr.ui::new_blockr_theme()] makes it
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
                        plugins = available_plugins(),
                        theme = blockr_option(
                          "theme", blockr.ui::new_blockr_theme()
                        )) {
  check_app_theme(theme)
  plugins <- app_plugins(plugins)
  check_board_plugins(x, plugins)
  registry <- app_block_registry(blocks)
  register_board_classes(x, registry)
  frontend <- frontend_reply(..., plugins = plugins)
  themes <- list(app_frontend_theme(plugins), theme)
  serve_app(
    encode_message(blockr_ser(x)),
    registry,
    plugins = plugins,
    frontend = frontend,
    themes = themes
  )
}

#' @rdname serve
#' @export
serve.character <- function(x, ..., blocks = available_blocks(),
                            plugins = available_plugins(),
                            theme = blockr_option(
                              "theme", blockr.ui::new_blockr_theme()
                            )) {
  check_app_theme(theme)
  plugins <- app_plugins(plugins)
  registry <- app_block_registry(blocks)
  frontend <- frontend_reply(..., plugins = plugins)
  themes <- list(app_frontend_theme(plugins), theme)
  serve_app(
    read_board_json(x),
    registry,
    plugins = plugins,
    frontend = frontend,
    themes = themes
  )
}

#' @param data The block's data, a list by input
#' @rdname serve
#' @export
serve.block <- function(x, ..., data = list(),
                        theme = blockr_option(
                          "theme", blockr.ui::new_blockr_theme()
                        )) {

  check_app_theme(theme)

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
    data,
    alone = TRUE,
    themes = list(theme)
  )
}

# Refuses an app's theme that is not one, as `blockr.ui::new_blockr_theme()`
# makes it, naming what it is instead.
# A string is what the environment variable `BLOCKR_THEME` gives the blockr
# option `theme`, so the error says how an app sets the option.
# R also checks the theme's fonts here (see `check_theme_fonts()`), so that
# each method of `serve()` stops the app as it is called.
check_app_theme <- function(theme) {

  if (!blockr.ui::is_blockr_theme(theme)) {
    blockr_abort(
      "The app's theme is expected to be a theme, as ",
      "`blockr.ui::new_blockr_theme()` makes it, and `serve()` got ",
      "{value_label(theme)}. ",
      "An app passes its theme to `serve()` in `theme`, or sets the blockr ",
      "option `theme` with an R option, as `options(blockr.theme = theme)` ",
      "does, since an environment variable gives a string.",
      class = "serve_theme_invalid"
    )
  }

  check_theme_fonts(theme, "The app's theme")
}

# What a value is, as an error names it: `NULL`, a string in quotes, or any
# other value by its class, as "an object of class list".
value_label <- function(x) {

  if (is.null(x)) {
    return("`NULL`")
  }

  if (is_string(x) && !is.na(x)) {
    return(paste0("the string \"", x, "\""))
  }

  paste("an object of class", class(x)[1L])
}

# Checks the fonts of a theme, `theme`, whose file R serves and whose face R
# writes into the page's head (see `theme_head()`).
# The theme's constructor, `blockr.ui::new_blockr_theme()`, checks nothing of
# its fonts, and leaves that to R, which reads them here.
# A font gives its family and the path of its file, and the weight and the
# style of its face, each as a string.
# The constructor gives a font that lacks a weight or a style `"normal"`.
# R writes the family in quotes, and the weight and the style as they are.
# So it refuses a font whose family, weight or style holds `"`, `}` or `<`,
# which would end the string, the rule or the element that holds it, as the
# constructor refuses such a value of a theme variable.
# A font whose file does not exist stops the app as well, rather than leave
# the page to draw its text in another face.
# The errors name the theme as `whose` gives it, as "The app's theme".
check_theme_fonts <- function(theme, whose) {

  fonts <- theme[["fonts"]]

  for (i in seq_along(fonts)) {

    font <- fonts[[i]]
    text <- c("family", "weight", "style")

    valid <- is.list(font) &&
      all(lgl_ply(font[text], is_nonempty_string)) &&
      is_string(font[["file"]]) && !is.na(font[["file"]])

    if (!valid) {
      blockr_abort(
        "{whose} gives a font, font {i}, that lacks the family, the path of ",
        "its file, the weight or the style of its face as a string.",
        class = "theme_font_invalid"
      )
    }

    family <- font[["family"]]
    file <- font[["file"]]

    if (any(grepl("[\"}<]", c(family, font[["weight"]], font[["style"]])))) {
      blockr_abort(
        "{whose} gives the font {family} a family, a weight or a style that ",
        "holds `\"`, `}` or `<`, which would end the string, the rule or the ",
        "element that holds it.",
        class = "theme_font_invalid"
      )
    }

    if (!utils::file_test("-f", file)) {
      blockr_abort(
        "{whose} gives the font {family} the file \"{file}\", which does not ",
        "exist.",
        class = "theme_font_invalid"
      )
    }
  }

  invisible()
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
# R calls the method of `frontend_args()` on the app's frontend (see
# `app_frontend()`).
# R checks what the method returns and writes it as a JSON object, once, as
# `serve()` is called. So a method that returns anything but a list by name
# stops the app then, naming the frontend.
# The app's plugins come after `...`, since R would match an argument of
# `serve()` named `p` to a `plugins` before it.
frontend_reply <- function(..., plugins) {

  front <- app_frontend(plugins)
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

# The app's frontend, among the app's plugins, as `app_plugins()` gives them,
# which hold one frontend.
# R finds it as `app_plugins()` does, by the R class of the plugin that its
# class's constructor makes, and gives that plugin.
app_frontend <- function(plugins) {

  available <- available_plugin_classes()
  plugins <- lapply(set_names(nm = names(plugins)), plugin_of_class, available)

  plugins[lgl_ply(plugins, is_frontend_plugin)][[1L]]
}

# The theme of the app's frontend, which R writes into the page's head before
# the app's theme (see `theme_head()`).
# R calls the method of `frontend_theme()` on the app's frontend (see
# `app_frontend()`), once, as `serve()` is called, as it calls the method of
# `frontend_args()`.
# So a method that returns anything but a theme, as
# `blockr.ui::new_blockr_theme()` makes it, stops the app then, naming the
# frontend, and so does a font of the theme that R cannot serve (see
# `check_theme_fonts()`).
app_frontend_theme <- function(plugins) {

  front <- app_frontend(plugins)
  theme <- frontend_theme(front)

  if (!blockr.ui::is_blockr_theme(theme)) {
    blockr_abort(
      "The method of `frontend_theme()` for the frontend ",
      "{plugin_class_id(front)} returns {value_label(theme)}, where a theme ",
      "is expected, as `blockr.ui::new_blockr_theme()` makes it.",
      class = "frontend_theme_invalid"
    )
  }

  check_theme_fonts(
    theme,
    paste("The theme of the frontend", plugin_class_id(front))
  )

  theme
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
# R also warns here of each key of the blockr option `renderers` that it
# ignores (see `check_chosen_renderers()`).
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
# The page's head holds the `themes` given, the frontend's and then the app's,
# or the app's alone for a block that `serve()` serves alone (see
# `theme_head()`).
# Neither changes while the page runs, so nothing else carries them.
serve_app <- function(board, registry, data = list(), alone = FALSE,
                      plugins = list(), frontend = as_json_object(list()),
                      themes = list()) {

  reset_logging()
  get_logger()
  check_client_logger()

  force(board)

  lock <- lock_board(board, data, registry)

  # Two packages that list one board option, or a family of one class, stop
  # the app here
  options <- app_options()
  offered <- offered_modules()
  families <- app_families()

  check_component_options(registry, plugins, options, families)
  check_chosen_renderers(registry, families)

  modules <- page_modules(board, registry, plugins, options, families, offered)

  shiny::shinyApp(
    ui = function(req) {
      serve_page(alone, modules = modules, themes = themes)
    },
    server = function(input, output, session) {

      ev <- new_evaluator(
        board, data, lock, registry, plugins, frontend,
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

  reached <- registry_families(registry, families)

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

# The output families that the block classes of the app's block registry,
# `registry`, reach, by class, each once.
# An open family reaches the families that the available packages list,
# `families`.
registry_families <- function(registry, families) {

  reached <- unlst(
    lapply(
      unname(registry_entries(registry)),
      function(x) reached_families(x[["block_class"]][["output"]], families)
    ),
    use_names = TRUE
  )

  reached[!duplicated(names(reached))]
}

# Warns of each key of the blockr option `renderers` that names no output
# family of the app that gives a renderer.
# R ignores the renderer under such a key, as under the typo `tabel_output`.
# The app's output families are those that the available packages list,
# `families`, and those that the block classes of the app's block registry,
# `registry`, reach.
# A family that gives no renderer draws no outputs of its own, so R ignores a
# key that names it too.
# R warns rather than stops the app, since the app works.
# It draws with the renderers that it would draw with anyway.
check_chosen_renderers <- function(registry, families) {

  drawing <- drawing_families(
    c(families, registry_families(registry, families))
  )

  ignored <- setdiff(names(chosen_renderers()), names(drawing))

  if (length(ignored)) {
    blockr_warn(
      "R ignores the key{?s} {ignored} of the blockr option `renderers`, ",
      "since {?it names/they name} no output family of the app that gives a ",
      "renderer.",
      class = "renderers_blockr_option_ignored"
    )
  }

  invisible()
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
# The page then holds blockr.ui's stylesheet and core's, the element that the
# client draws in, and the module script that starts the client with
# `startPage()`.
# The stylesheet of blockr.ui holds the theme variables with their defaults
# and the faces of its fonts, which every UI reads, core's loading note among
# them, so it is on the page from the start, ahead of core's, as The
# stylesheet on the page has it
# (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.4).
# Core's comes after it, so that a class of core's that core passes to one of
# blockr.ui's components wins over the component's own class of the same
# weight.
# The rules of the `themes` given follow the two stylesheets, with the faces of
# their fonts and the links that have the browser fetch the fonts' files (see
# `theme_head()`).
# So the page draws in the themes from the first paint, as An app's theme has
# it (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.5).
# The element holds the loading note from the start, so that the page shows it
# before the client's code has loaded, which the client keeps until the board
# opens.
# The page's `color-scheme` tag takes `light dark`, while blockr.ui's
# stylesheet sets the browser's color-scheme under the attribute `data-bk-mode`
# of each mode, which the board option `dark_mode` sets on the page's root,
# as Light mode and dark mode has it
# (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.2).
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
                       modules = list(), themes = list()) {

  runtime <- served_module(core_runtime())
  ui_css <- served_url(ui_stylesheet())

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
      shiny::tags$meta(name = "color-scheme", content = "light dark"),
      shiny::tags$link(rel = "stylesheet", href = ui_css),
      shiny::tags$link(rel = "stylesheet", href = runtime[["css"]]),
      theme_head(themes),
      preload_links(runtime[["module"]], modules)
    ),
    shiny::tags$div(id = "blockr-page", loading_note()),
    shiny::tags$script(type = "module", shiny::HTML(script))
  )
}

# What R writes into the page's head for the `themes` given, the frontend's and
# then the app's, as An app's theme has it
# (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.5).
# A `<style>` holds, for each theme in turn, the faces of its fonts and the
# rules that blockr.ui's method of `format()` returns for it.
# The rules of the two themes have the same selectors, so the app's rules win
# by coming later.
# The app's face of a font wins in the same way over a face of the frontend's
# with the same family, weight and style.
# R serves the file of each font here, at the URL that the font's face names,
# and names the file in a preload link, so that the browser fetches it as the
# page loads (see `font_face()`).
# A link for a font needs `crossorigin`, since the browser fetches a font in
# that mode, and would not use a file that it fetched otherwise.
# A theme with no values and no fonts writes nothing, so the page holds no
# `<style>` where neither theme gives anything.
theme_head <- function(themes) {

  urls <- lapply(
    themes,
    function(theme) {
      chr_ply(theme[["fonts"]], function(font) served_url(font_file(font)))
    }
  )

  text <- unlst(
    map(
      function(theme, urls) {
        c(chr_mply(font_face, theme[["fonts"]], urls), format(theme))
      },
      themes,
      urls
    )
  )

  text <- text[nzchar(text)]

  shiny::tagList(
    if (length(text)) {
      shiny::tags$style(shiny::HTML(paste(text, collapse = "\n\n")))
    },
    lapply(
      unique(unlst(urls)),
      function(url) {
        shiny::tags$link(
          rel = "preload", href = paste0("./", url), as = "font",
          crossorigin = NA
        )
      }
    )
  )
}

# The file of a font of a theme, as R serves it: from no package, at the path
# that the font gives.
# So R serves the file alone, in a directory named by its hash, as it serves a
# module that a script defines (see `served_url()`).
font_file <- function(font) {
  list(path = font[["file"]], package = NULL)
}

# The face of a font of a theme, `font`, as R writes it with `@font-face`, at
# the URL at which R serves the font's file, `url`.
# The face swaps the font in once its file has loaded, so that the text draws
# in the next face of the stack until then, rather than not at all.
font_face <- function(font, url) {
  paste0(
    "@font-face {\n",
    "  font-family: \"", font[["family"]], "\";\n",
    "  font-style: ", font[["style"]], ";\n",
    "  font-weight: ", font[["weight"]], ";\n",
    "  font-display: swap;\n",
    "  src: url(\"./", url, "\");\n",
    "}"
  )
}

# The stylesheet of blockr.ui, which its build writes beside its JS, with the
# files of its fonts, which R serves beside it.
ui_stylesheet <- function() {
  js_module("js/blockr-ui.css", package = "blockr.ui")
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
