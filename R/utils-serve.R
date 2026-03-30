#' Serve object
#'
#' Intended as entry point to start up a shiny app, the generic function
#' `serve()` can be dispatched either on a single block (mainly for previewing
#' purposes during block development) or an entire board
#'
#' @param x Object
#' @param ... Generic consistency
#'
#' @examplesShinylive
#' webr::install("blockr.core", repos = "https://cynkra.github.io/blockr.webR/")
#' library(blockr.core)
#' serve(
#'   new_merge_block("Time"),
#'   data = list(
#'     x = datasets::BOD,
#'     y = datasets::ChickWeight
#'   )
#' )
#'
#' @return The generic `serve()` is expected to return the result of a call to
#' [shiny::shinyApp()].
#'
#' @export
serve <- function(x, ...) {
  update_serve_obj("initial", x)
  UseMethod("serve")
}

#' @param id Block ID
#' @param data Data inputs
#' @rdname serve
#' @export
serve.block <- function(x, id = "block", ..., data = list()) {

  init_data <- function(x, is_variadic) {
    if (is_variadic) do.call(reactiveValues, x) else reactiveVal(x)
  }

  if (...length() && !length(data)) {
    data <- list(...)
  }

  dot_args <- !names(data) %in% block_inputs(x)

  if (!is.na(block_arity(x)) && any(dot_args)) {
    blockr_abort(
      "Unexpected arguments {names(data)[dot_args]}.",
      class = "unexpected_var_args"
    )
  }

  if (any(dot_args)) {
    data <- c(data[!dot_args], list(...args = data[dot_args]))
  }

  ui <- bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),
    title = id,
    expr_ui(id, x),
    block_ui(id, x)
  )

  server <- function(input, output, session) {

    res <- block_server(id, x, Map(init_data, data, names(data) == "...args"))

    exportTestValues(
      result = export_safely(res$result())()
    )

    invisible()
  }

  shinyApp(ui, server)
}

#' @param id Board namespace ID
#' @param plugins Board plugins
#' @param options Board options
#'
#' @rdname serve
#'
#' @examplesShinylive
#' webr::install("blockr.core", repos = "https://cynkra.github.io/blockr.webR/")
#' library(blockr.core)
#' serve(
#'   new_board(
#'     blocks = c(
#'       a = new_dataset_block("BOD"),
#'       b = new_dataset_block("ChickWeight"),
#'       c = new_merge_block("Time")
#'     ),
#'     links = c(
#'       ac = new_link("a", "c", "x"),
#'       bc = new_link("b", "c", "y")
#'     ),
#'     stacks = list(ac = c("a", "c"))
#'   )
#' )
#'
#' @export
serve.board <- function(x, id = rand_names(), plugins = blockr_app_plugins,
                        options = blockr_app_options, ...) {


  stopifnot(is_string(id), is.function(plugins), is.function(options))

  shinyApp(
    serve_board_ui(id, plugins, options),
    serve_board_srv(id, plugins, options, ...)
  )
}

#' @rdname serve
#' @export
blockr_app_plugins <- function(x) {
  UseMethod("blockr_app_plugins")
}

#' @export
blockr_app_plugins.board <- function(x) {
  board_plugins(x)
}

#' @rdname serve
#' @export
custom_plugins <- function(x) {

  custom <- as_plugins(x)

  function(x) {

    default <- blockr_app_plugins(x)

    hit <- match(names(custom), names(default), nomatch = NA_integer_)

    if (all(is.na(hit))) {
      c(default, custom)
    } else {
      c(default[-hit[!is.na(hit)]], custom)
    }
  }
}

#' @rdname serve
#' @export
blockr_app_options <- function(x) {
  UseMethod("blockr_app_options")
}

#' @export
blockr_app_options.board <- function(x, ...) {
  combine_board_options(
    board_options(x),
    lapply(board_blocks(x), board_options),
    lapply(available_blocks(), board_options)
  )
}

#' @rdname serve
#' @export
custom_options <- function(x) {

  custom <- as_board_options(x)

  function(x) {

    default <- blockr_app_options(x)

    combine_board_options(
      custom,
      default
    )
  }
}

#' @rdname serve
#' @export
blockr_app_ui <- function(id, x, ...) {
  UseMethod("blockr_app_ui", x)
}

#' @export
blockr_app_ui.board <- function(id, x, ...) {
  bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),
    board_ui(id, x, ...)
  )
}

#' @rdname serve
#' @export
blockr_app_server <- function(id, x, ...) {
  UseMethod("blockr_app_server", x)
}

#' @export
blockr_app_server.board <- function(id, x, ...) {
  board_server(id, x, ...)
}

serve_board_ui <- function(id, plugins, options, ...) {

  args <- list(...)

  function(req) {

    log_info(
      "[RELOAD-DEBUG] serve_board_ui called | ",
      "URL: {coal(req$QUERY_STRING, '(none)')} | ",
      "is_reloading: {is_reloading('reload')}"
    )

    if (!is_reloading("reload")) {
      preload <- get0("preload_fn", envir = serve_obj, inherits = FALSE)
      if (!is.null(preload)) {
        query <- parseQueryString(coal(req$QUERY_STRING, ""))
        result <- tryCatch(preload(query, req), error = function(e) NULL)
        if (not_null(result)) {
          log_info("[RELOAD-DEBUG] preload successful, writing to serve_obj")
          update_serve_obj("reload", result$board, meta = result$meta)
        } else {
          log_info("[RELOAD-DEBUG] preload returned NULL")
        }
      }
    } else {
      log_info("[RELOAD-DEBUG] skipping preload, reload already in progress")
    }

    x <- get_serve_obj("reload")
    id <- coal(attr(x, "id"), id)

    log_info("[RELOAD-DEBUG] building UI for board {id}")

    do.call(
      blockr_app_ui,
      c(list(id, x, plugins = plugins(x), options = options(x)), args)
    )
  }
}

serve_board_srv <- function(id, plugins, options, ...) {

  args <- list(...)

  function(input, output, session) {

    log_info(
      "[RELOAD-DEBUG] serve_board_srv called | ",
      "session: {substr(session$token, 1, 8)} | ",
      "is_reloading: {is_reloading('reload')}"
    )

    onStop(revert(trace_observe()), session)

    x <- get_serve_obj("reload")
    id <- coal(attr(x, "id"), id)

    res <- do.call(
      blockr_app_server,
      c(list(id, x, plugins = plugins(x), options = options(x)), args)
    )

    blockr_test_exports(x, res)

    invisible()
  }
}

serve_obj <- new.env()

#' @param meta Optional metadata to store alongside the board.
#' @rdname serve
#' @export
update_serve_obj <- function(id, x, meta = NULL) {
  session <- getDefaultReactiveDomain()
  stk <- substr(paste(sys.calls(), collapse = " > "), 1, 200)
  log_info(
    "[RELOAD-DEBUG] update_serve_obj('{id}') | ",
    "session: {if (!is.null(session)) substr(session$token, 1, 8) else 'none'} | ",
    "meta_url: {coal(meta$url, '(null)')} | ",
    "caller: {stk}"
  )
  assign(id, list(board = x, meta = meta), envir = serve_obj)
  invisible(x)
}

is_reloading <- function(id = "reload") {
  exists(id, envir = serve_obj, inherits = FALSE)
}

finalize_reload <- function(id = "reload") {

  obj <- get0(id, envir = serve_obj, inherits = FALSE)
  session <- getDefaultReactiveDomain()

  log_info(
    "[RELOAD-DEBUG] finalize_reload('{id}') | ",
    "session: {if (!is.null(session)) substr(session$token, 1, 8) else 'none'} | ",
    "found: {!is.null(obj)} | ",
    "meta_url: {if (!is.null(obj)) coal(obj$meta$url, '(null)') else '(null)'}"
  )

  if (is.null(obj)) {
    return(invisible(NULL))
  }

  rm(list = id, envir = serve_obj, inherits = FALSE)
  invisible(obj$meta)
}

#' @param preload A function with signature `function(query, req)` that
#'   receives parsed URL query parameters and the HTTP request object. It
#'   should return `NULL` or a list with `board` and `meta` components. When
#'   non-`NULL`, the board is used as the initial state, avoiding a redundant
#'   `session$reload()` on fresh page loads from URL.
#'
#' @rdname serve
#' @export
register_board_preload <- function(preload) {

  stopifnot(is.function(preload) || is.null(preload))

  old <- get0("preload_fn", envir = serve_obj, inherits = FALSE)

  assign("preload_fn", preload, envir = serve_obj)

  invisible(old)
}

#' @rdname serve
#' @export
get_serve_obj <- function(id = NULL) {

  obj <- coal(
    get0(coal(id, "initial"), envir = serve_obj, inherits = FALSE),
    get("initial", envir = serve_obj, inherits = FALSE)
  )

  if (is.list(obj) && is_board(obj$board)) obj$board else obj
}

revert <- function(...) {
  funs <- Filter(is.function, list(...))
  function() {
    invisible(
      map(do.call, what = funs, MoreArgs = list(args = list()))
    )
  }
}

#' @export
serve.character <- function(x, ...) {

  stopifnot(is_string(x), file.exists(x))

  serve(
    blockr_deser(read_json(x)),
    ...
  )
}

#' @param rv Board [shiny::reactiveValues()]
#' @rdname serve
#' @export
blockr_test_exports <- function(x, rv, ...) {
  UseMethod("blockr_test_exports", x)
}

#' @export
blockr_test_exports.board <- function(x, rv, ...) {
  exportTestValues(
    result = lapply(
      lapply(
        lapply(lst_xtr(rv[[1L]]$blocks, "server", "result"), export_safely),
        reval
      ),
      reval
    )
  )
}
