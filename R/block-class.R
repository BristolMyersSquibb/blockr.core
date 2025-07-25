#' Blocks
#'
#' Steps in a data analysis pipeline are represented by blocks. Each block
#' combines data input with user inputs to produce an output. In order to
#' create a block, which is implemented as a shiny module, we require a server
#' function, a function that produces some UI and a class vector.
#'
#' A block constructor may have arguments, which taken together define the
#' block state. It is good practice to expose all user-selectable arguments of
#' a block (i.e. everything excluding the "data" input) as block arguments such
#' that block can be fully initialized via the constructor. Some default values
#' are required such that blocks can be constructed via constructor calls
#' without arguments. Where it is sensible to do so, specific default values
#' are acceptable, but if in any way data dependent, defaults should map to
#' an "empty" input. For example, a block that provides [utils::head()]
#' functionality, one such argument could be `n` and a reasonable default value
#' could be `6L` (in line with corresponding default S3 method implementation).
#' On the other hand, a block that performs a [base::merge()] operation might
#' expose a `by` argument, but a general purpose default value (that does not
#' depend on the data) is not possible. Therefore, [new_merge_block()] has
#' `by = character()`.
#'
#' The return value of a block constructor should be the result of a call to
#' `new_block()` and `...` should be contained in the constructor signature
#' such that general block arguments (e.g. `name`) are available from the
#' constructor.
#'
#' @section Server:
#' The server function (passed as `server`) is expected to be a function that
#' returns a [shiny::moduleServer()]. This function is expected to have at
#' least an argument `id` (string-valued), which will be used as the module ID.
#' Further arguments may be used in the function signature, one for each "data"
#' input. A block implementing [utils::head()] for example could have a single
#' extra argument `data`, while a block that performs [base::merge()] requires
#' two extra arguments, e.g. `x` and `y`. Finally, a variadic block, e.g.
#' a block implementing something like [base::rbind()], needs to accommodate for
#' an arbitrary number of inputs. This is achieved by passing a
#' [shiny::reactiveValues()] object as `...args` and thus such a variadic block
#' needs `...args` as part of the server function signature. All per-data input
#' arguments are passed as [shiny::reactive()] or [shiny::reactiveVal()]
#' objects.
#'
#' The server function may implement arbitrary shiny logic and is expected to
#' return a list with components `expr` and `state`. The expression corresponds
#' to the R code necessary to perform the block task and is expected to be
#' a reactive quoted expression. It should contain user-chosen values for all
#' user inputs and placeholders for all data inputs (using the same names for
#' data inputs as in the server function signature). Such an expression for a
#' [base::merge()] block could be created using [base::bquote()] as
#'
#' ```r
#' bquote(
#'   merge(x, y, by = .(cols)),
#'   list(cols = current_val())
#' }
#' ```
#'
#' where `current_val()` is a reactive that evaluates to the current user
#' selection of the `by` columns. This should then be wrapped in a
#' [shiny::reactive()] call such that `current_val()` can be evaluated whenever
#' the current expression is required.
#'
#' The `state` component is expected to be a named list with either reactive or
#' "static" values. In most cases, components of `state` will be reactives,
#' but it might make sense in some scenarios to have constructor arguments that
#' are not exposed via UI components but are fixed at construction time. An
#' example for this could be the `dataset_block` implementation where we have
#' constructor arguments `dataset` and `package`, but only expose `dataset`
#' as UI element. This means that `package` is fixed at construction time.
#' Nevertheless, `package` is required as state component, as this is used for
#' re-creating blocks from saved state.
#'
#' State component names are required to match block constructor arguments and
#' re-creating saved objects basically calls the block constructor with values
#' obtained from block state.
#'
#' @section UI:
#' Block UI is generated using the function passed as `ui` to the `new_block`
#' constructor. This function is required to take a single argument `id` and
#' shiny UI components have to be namespaced such that they are nested within
#' this ID (i.e. by creating IDs as `shiny::NS(id, "some_value")`). Some care
#' has to be taken to properly initialize inputs with constructor values. As a
#' rule of thumb, input elements exposed to the UI should have corresponding
#' block constructor arguments such that blocks can be created with a given
#' initial state.
#'
#' Block UI should be limited to displaying and arranging user inputs to set
#' block arguments. For outputs, use generics [block_output()] and
#' [block_ui()].
#'
#' @section Sub-classing:
#' In addition to the specific class of a block, the core package uses virtual
#' classes to group together blocks with similar behavior (e.g.
#' `transform_block`) and makes use of this inheritance structure in S3
#' dispatch for methods like [block_output()] and [block_ui()]. This pattern is
#' not required but encouraged.
#'
#' @section Initialization/evaluation:
#' Some control over when a block is considered "ready for evaluation" is
#' available via arguments `dat_valid` and `allow_empty_state`. Data input
#' validation can optionally be performed by passing a predicate function with
#' the same arguments as in the server function (not including `id`) and the
#' block expression will not be evaluated as long as this function throws an
#' error.
#'
#' Other conditions (messages and warnings) may be thrown as will be caught
#' and displayed to the user but they will not interrupt evaluation. Errors
#' are safe in that they will be caught as well but the will interrupt
#' evaluation as long as block data input does not satisfy validation.
#'
#' @param server A function returning [shiny::moduleServer()]
#' @param ui A function with a single argument (`ns`) returning a `shiny.tag`
#' @param class Block subclass
#' @param ctor String-valued constructor name or function/frame number (mostly
#'   for internal use or when defining constructors for virtual classes)
#' @param ctor_pkg String-valued package name when passing a string-valued
#'   constructor name or `NULL`
#' @param dat_valid (Optional) input data validator
#' @param name Block name
#' @param allow_empty_state Either `TRUE`, `FALSE` or a character vector of
#' `state` values that may be empty while still moving forward with block eval
#' @param ... Further (metadata) attributes
#'
#' @examples
#' new_identity_block <- function() {
#'   new_transform_block(
#'     function(id, data) {
#'       moduleServer(
#'         id,
#'         function(input, output, session) {
#'           list(
#'             expr = reactive(quote(identity(data))),
#'             state = list()
#'           )
#'         }
#'       )
#'     },
#'     function(id) {
#'       tagList()
#'     },
#'     class = "identity_block"
#'   )
#' }
#'
#' blk <- new_identity_block()
#' is_block(blk)
#'
#' @return Both `new_block()` and `as_block()` return an object inheriting from
#' `block`, while `is_block()` returns a boolean indicating whether an object
#' inherits from `block` or not. Block vectors, created using `blocks()`,
#' `as_blocks()`, or by combining multiple blocks using [base::c()] all inherit
#' frm `blocks` and `iss_block()` returns a boolean indicating whether an object
#' inherits from `blocks` or not.
#'
#' @export
new_block <- function(server, ui, class, ctor, ctor_pkg, dat_valid = NULL,
                      allow_empty_state = FALSE, name = NULL, ...) {

  stopifnot(is.character(class), length(class) > 0L)

  if (missing(ui)) {
    ui <- function(id) {
      tagList()
    }
  }

  if (missing(ctor)) {
    ctor <- sys.parent()
  }

  if (is.numeric(ctor)) {

    fun <- sys.function(ctor)
    call <- deparse(sys.call(ctor)[[1L]])

    if (grepl("::", call, fixed = TRUE)) {

      call <- strsplit(call, "::", fixed = TRUE)[[1L]]

      stopifnot(length(call) == 2L)

      ctor <- call[2L]

      if (missing(ctor_pkg)) {
        ctor_pkg <- call[1L]
      } else {
        stopifnot(identical(ctor_pkg, call[1L]))
      }

    } else {

      if (missing(ctor_pkg)) {
        ctor_pkg <- pkg_name(environment(fun))
      }

      if (not_null(ctor_pkg)) {
        ctor <- call
      }
    }

    if (is_string(ctor_pkg) && is_string(ctor)) {
      try <- get0(ctor, asNamespace(ctor_pkg), mode = "function",
                  inherits = FALSE)
    } else {
      try <- NULL
    }

    if (is.null(try)) {
      ctor <- fun
      ctor_pkg <- NULL
    }
  }

  if (is.null(ctor_pkg)) {
    stopifnot(is.function(ctor))
  } else {
    stopifnot(is_string(ctor), is_string(ctor_pkg))
  }

  if (is.null(name)) {
    name <- gsub("_", " ", class[1L])
    name <- paste0(toupper(substr(name, 1L, 1L)), substring(name, 2L))
  }

  validate_block(
    new_vctr(
      list(
        expr_server = server,
        expr_ui = ui,
        dat_valid = dat_valid
      ),
      ...,
      ctor = ctor,
      ctor_pkg = ctor_pkg,
      name = name,
      allow_empty_state = allow_empty_state,
      class = c(class, "block")
    ),
    ui_eval = TRUE
  )
}

validate_block_server <- function(server) {

  if (!is.function(server)) {
    abort(
      "A block server component is expected to be a function.",
      class = "block_server_function_invalid"
    )
  }

  args <- names(formals(server))

  if (!identical(args[1L], "id")) {
    abort(
      paste(
        "A block server function is expected to have an argument `id` in",
        "first poistion."
      ),
      class = "block_server_arg_id_invalid"
    )
  }

  if ("..." %in% args) {
    abort(
      paste(
        "Variadic blocks are supported not by passing arguments as `...`",
        "but using a special argument `...args`."
      ),
      class = "block_server_dots_invalid"
    )
  }

  if (any(grepl("^[1-9][0-9]*$", args))) {
    abort(
      paste(
        "Integer-valued argument names are reserved as poistional arguments",
        "in `...args.`"
      ),
      class = "block_server_args_invalid"
    )
  }

  invisible(server)
}

validate_block_ui <- function(ui) {

  if (!is.function(ui)) {
    abort(
      "A block UI component is expected to be a function.",
      class = "block_ui_function_invalid"
    )
  }

  if (!identical(names(formals(ui)), "id")) {
    abort(
      "A block UI function is expected to have a single argument `id`.",
      class = "block_ui_arg_id_invalid"
    )
  }

  invisible(ui)
}

validate_data_validator <- function(validator, server) {

  server_args <- setdiff(names(formals(server)), "id")

  if (!length(server_args) && not_null(validator)) {
    abort(
      "A nullary server function cannot accopmany a data input validator.",
      class = "block_validator_nullary_invalid"
    )
  }

  if (not_null(validator)) {

    if (!is.function(validator)) {
      abort(
        "Data input validator is expected to be a function.",
        class = "block_validator_function_invalid"
      )
    }

    val_args <- names(formals(validator))

    if (!identical(server_args, val_args)) {
      abort(
        paste0(
          "Server args ", paste_enum(server_args), " do not match validator ",
          "args ", paste_enum(val_args), "."
        ),
        class = "block_validator_args_invalid"
      )
    }
  }

  invisible(validator)
}

validate_block <- function(x, ui_eval = FALSE) {

  if (!is_block(x)) {
    abort(
      "Expecting blocks to inherit from \"block\".",
      class = "block_class_invalid"
    )
  }

  if (class(x)[1L] == "block") {
    abort(
      "Expecting blocks to have at least one sub-class.",
      class = "block_class_invalid"
    )
  }

  if (!is.list(x)) {
    abort(
      "Expecting blocks to behave list-like.",
      class = "block_list_like_invalid"
    )
  }

  if (!is.function(try(block_ctor(x), silent = TRUE))) {
    abort(
      "Cannot reconstruct blocks without constructors.",
      class = "block_no_ctor"
    )
  }

  if (!is_string(block_name(x))) {
    abort(
      "Expecting a string-valued block name.",
      class = "block_name_invalid"
    )
  }

  srv <- block_expr_server(x)

  validate_block_server(srv)
  validate_block_ui(block_expr_ui(x))
  validate_data_validator(block_data_validator(x), srv)

  if (isTRUE(ui_eval)) {

    ui <- expr_ui("block", x)

    if (!inherits(ui, "shiny.tag", "shiny.tag.list", agg = any)) {
      abort(
        paste(
          "A block UI function is expected to return a shiny UI object,",
          "i.e. either a `shiny.tag` or a `shiny.tag.list`."
        ),
        class = "block_ui_eval_invalid"
      )
    }
  }

  x
}

#' @param x An object inheriting from `"block"`
#' @rdname new_block
#' @export
is_block <- function(x) {
  inherits(x, "block")
}

#' @rdname new_block
#' @export
as_block <- function(x, ...) {
  UseMethod("as_block")
}

#' @export
as_block.block <- function(x, ...) {
  validate_block(x)
}

#' @export
as_block.list <- function(x, ...) {

  stopifnot(
    all(c("constructor", "payload", "package", "object") %in% names(x))
  )

  pkg <- x[["package"]]
  ctr <- x[["constructor"]]

  if (is.null(pkg)) {
    ctor <- unserialize(jsonlite::base64_dec(ctr))
    pkg <- list(NULL)
    ctr <- ctor
  } else {
    ctor <- get(ctr, asNamespace(pkg), mode = "function")
  }

  stopifnot(is.function(ctor))

  args <- c(
    x[["payload"]],
    ctor = ctr,
    ctor_pkg = pkg
  )

  res <- do.call(ctor, args)

  if (!identical(class(res), x[["object"]])) {
    stop("Could not deserialize block.")
  }

  res
}

#' @export
as.list.block <- function(x, state = NULL, ...) {

  pkg <- attr(x, "ctor_pkg")

  if (is.null(state)) {
    state <- initial_block_state(x)
  }

  attrs <- attributes(x)

  state <- c(
    state,
    attrs[
      setdiff(
        names(attrs),
        c("names", "ctor", "ctor_pkg", "class", "allow_empty_state")
      )
    ]
  )

  ctor <- attr(x, "ctor")

  if (is.function(ctor)) {

    stopifnot(is.null(pkg))

    ctor <- serialize(ctor, NULL)

    pkg <- NULL
    ver <- NULL

  } else {

    stopifnot(is_string(ctor), not_null(pkg))

    ver <- as.character(pkg_version(pkg))
  }

  list(
    object = class(x),
    payload = state,
    constructor = ctor,
    package = pkg,
    version = ver
  )
}

#' @export
c.block <- function(...) {

  res <- unlist(
    lapply(list(...), harmonize_list_of_blocks),
    recursive = FALSE
  )

  as_blocks(res)
}

#' Block utilities
#'
#' Several utilities for working (and manipulating) `block` objects are exported
#' and developers are encouraged to use these instead of relying on object
#' implementation to extract or modify attributes. If functionality for working
#' with blocks in lacking, please consider opening an
#' [issue](https://github.com/BristolMyersSquibb/blockr.core/issues/new).
#'
#' @section Block name:
#' Each block can have a name (by default constructed from the class vector)
#' intended for users to easily identify different blocks. This name can freely
#' be changed during the lifetime of a block and no uniqueness restrictions are
#' in place. The current block name can be retrieved with `block_name()` and
#' set as `block_name(x) <- "some name"`.
#'
#' @section Input validation:
#' Data input validation is available via `validate_data_inputs()` which uses
#' the (optional) validator function passed to [new_block()] at construction
#' time. This mechanism can be used to prevent premature evaluation of the
#' block expression as this might lead to unexpected errors.
#'
#' @section Block arity/inputs:
#' The set of explicit (named) data inputs for a block is available as
#' `block_inputs()`, while the block arity can be queried with `block_arity()`.
#' In case of variadic blocks (i.e. blocks that take a variable number of
#' inputs like for example a block providing [base::rbind()]-like
#' functionality), `block_arity()` returns `NA` and the special block server
#' function argument `...args`, signalling variadic behavior is stripped from
#' `block_inputs()`.
#'
#' @param x An object inheriting from `"block"`
#'
#' @examples
#' blk <- new_dataset_block()
#' block_name(blk)
#' block_name(blk) <- "My dataset block"
#' block_name(blk)
#'
#' block_inputs(new_dataset_block())
#' block_arity(new_dataset_block())
#'
#' block_inputs(new_merge_block())
#' block_arity(new_merge_block())
#'
#' block_inputs(new_rbind_block())
#' block_arity(new_rbind_block())
#'
#' @return Return types vary among the set of exported utilities:
#' * `block_name()`: string valued block name,
#' * `block_name<-()`: `x` (invisibly),
#' * `validate_data_inputs()`: `NULL` if no validator is set and the result of
#'   the validator function otherwise,
#' * `block_inputs()`: a (possibly empty) character vector of data input names,
#' * `block_arity()`: a scalar integer with `NA` in case of variadic behavior.
#'
#' @export
block_name <- function(x) {
  stopifnot(is_block(x))
  attr(x, "name")
}

#' @param value New value
#' @rdname block_name
#' @export
`block_name<-` <- function(x, value) {
  stopifnot(is_block(x), is_string(value))
  attr(x, "name") <- value
  invisible(x)
}

block_ctor <- function(x) {

  stopifnot(is_block(x))

  fun <- attr(x, "ctor")

  if (is.function(fun)) {
    return(fun)
  }

  pkg <- attr(x, "ctor_pkg")

  get(fun, envir = asNamespace(pkg), mode = "function", inherits = FALSE)
}

block_expr_server <- function(x) {
  x[["expr_server"]]
}

block_expr_ui <- function(x) {
  x[["expr_ui"]]
}

block_data_validator <- function(x) {
  x[["dat_valid"]]
}

block_has_data_validator <- function(x) {
  not_null(block_data_validator(x))
}

block_allow_empty_state <- function(x) {
  attr(x, "allow_empty_state")
}

#' @param data Data input values
#' @rdname block_name
#' @export
validate_data_inputs <- function(x, data) {

  if (block_has_data_validator(x)) {
    return(do.call(block_data_validator(x), data))
  }

  NULL
}

#' @rdname block_name
#' @export
block_inputs <- function(x) {
  UseMethod("block_inputs", x)
}

#' @export
block_inputs.block <- function(x) {
  setdiff(block_expr_inputs(x), "...args")
}

block_expr_inputs <- function(x) {
  setdiff(names(formals(block_expr_server(x))), "id")
}

block_ctor_inputs <- function(x) {
  setdiff(names(formals(block_ctor(x))), "...")
}

initial_block_state <- function(x) {
  lapply(
    set_names(nm = block_ctor_inputs(x)),
    get,
    envir = environment(block_expr_server(x))
  )
}

block_base_attrs <- function() {
  setdiff(
    names(formals(new_block)),
    c("server", "ui", "class", "ctor", "ctor_pkg", "dat_valid",
      "allow_empty_state")
  )
}

#' @rdname block_name
#' @export
block_arity <- function(x) {

  if (is_board(x)) {
    return(
      set_names(int_ply(board_blocks(x), block_arity), board_block_ids(x))
    )
  }

  args <- block_expr_inputs(x)

  if ("...args" %in% args) {
    return(NA_integer_)
  }

  length(args)
}

#' @export
format.block <- function(x, ...) {

  out <- ""

  for (cl in rev(setdiff(class(x), c("list", "vctrs_vctr")))) {
    out <- paste0("<", cl, out, ">")
  }

  out <- c(
    out,
    paste0("Name: \"", attr(x, "name"), "\"")
  )

  arity <- block_arity(x)

  if (is.na(arity)) {
    dat <- "Indefinite arity"
  } else if (arity > 0L) {
    dat <- paste0("Data inputs: ", paste_enum(block_inputs(x), quotes = "\""))
  } else {
    dat <- "No data inputs"
  }

  out <- c(out, dat)

  if (length(block_ctor_inputs(x))) {

    args <- initial_block_state(x)
    args <- trimws(utils::capture.output(utils::str(args))[-1L], "right")

    out <- c(out, "Initial block state:", args)

  } else {

    out <- c(out, "Stateless block")
  }

  if (!is.null(attr(x, "ctor_pkg"))) {
    ctor <- paste0(attr(x, "ctor_pkg"), "::", attr(x, "ctor"), "()")
  } else {
    ctor <- "<local function>"
  }
  c(out, paste("Constructor:", ctor))
}

#' @export
print.block <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
