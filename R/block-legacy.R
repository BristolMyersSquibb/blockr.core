#' Blocks of the Shiny contract
#'
#' The constructor `new_block()` makes a block of the contract that
#' [new_block_class()] replaces, implemented as a Shiny module. Steps in a data
#' analysis pipeline are represented by blocks. Each block
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
#' `reactives::reactive_exprs()` collection as `...args` and thus such a
#' variadic block needs `...args` as part of the server function signature.
#' The fixed per-data input arguments are passed as [shiny::reactive()] or
#' [shiny::reactiveVal()] objects, while each slot of the collection holds the
#' reactive for one input: reading a slot with `[[` or `$`, as in `...args[[1]]`
#' or `...args$x`, returns the input's data, and reading it with `[`, as in
#' `...args[1]`, returns the reactive. The inputs are listed by `names()` and
#' counted by `length()`, `as.list()` returns their reactives and
#' `reactives::as_values()` the data of all of them.
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
#' block arguments.
#'
#' @section Sub-classing:
#' In addition to the specific class of a block, the core package uses virtual
#' classes to group together blocks with similar behavior (e.g.
#' `transform_block`) and makes use of this inheritance structure in S3
#' dispatch for methods like [block_eval()]. This pattern is not required but
#' encouraged.
#'
#' @section Initialization/evaluation:
#' Some control over when a block is considered "ready for evaluation" is
#' available via arguments `dat_valid` and `allow_empty_state`. Data input
#' validation can optionally be performed by passing a predicate function with
#' the same arguments as in the server function (not including `id`) and the
#' block expression will not be evaluated as long as this function throws an
#' error.
#'
#' Ahead of validation, a block must have its inputs available: every required
#' *data* input connected to a ready upstream (variadic blocks need at least the
#' `...args` minimum declared via `allow_empty_state`) and every required *user*
#' input (`state`) provided. Until then -- including while an upstream is itself
#' still pending -- neither the validator nor the block expression run and no
#' output is produced.
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
#' @param block_name Block name
#' @param allow_empty_state Relaxes the block's readiness requirements. By
#' default every data input must be connected and a variadic block needs at
#' least one `...args` input. As `TRUE`, `FALSE` or a character vector of
#' `state` names it relaxes *user* inputs (which `state` values may be empty).
#' A `list(input = ..., data = ...)` also relaxes *data* inputs: `input` takes
#' the same `TRUE`/`FALSE`/character form, while `data` names non-variadic data
#' inputs that may stay unconnected and, via a `...args` entry, overrides the
#' required number of variadic inputs, e.g.
#' `list(input = "n", data = list("y", ...args = 2))`
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
#'       shiny::tagList()
#'     },
#'     class = "identity_block"
#'   )
#' }
#'
#' blk <- new_identity_block()
#' is_block(blk)
#'
#' @return The constructor `new_block()` returns an object inheriting from
#' `block`.
#'
#' @export
new_block <- function(server, ui, class, ctor = sys.parent(), ctor_pkg = NULL,
                      dat_valid = NULL, allow_empty_state = FALSE,
                      block_name = default_block_name, ...) {

  stopifnot(is.character(class), length(class) > 0L)

  if (missing(ui)) {
    ui <- function(id) {
      tagList()
    }
  }

  if (is.null(block_name)) {
    block_name <- default_block_name
  }

  class <- c(class, "block")

  if (is.function(block_name)) {
    block_name <- block_name(class)
  }

  stopifnot(is_string(block_name))

  validate_block(
    new_vctr(
      list(
        expr_server = server,
        expr_ui = ui,
        dat_valid = dat_valid
      ),
      ...,
      ctor = resolve_ctor(ctor, ctor_pkg),
      block_name = block_name,
      allow_empty_state = allow_empty_state,
      class = class
    ),
    ui_eval = TRUE
  )
}

internal_block_attributes <- function() {
  c(
    "ctor",
    "class",
    "allow_empty_state"
  )
}

validate_block_server <- function(server) {

  if (!is.function(server)) {
    blockr_abort(
      "A block server component is expected to be a function.",
      class = "block_server_function_invalid"
    )
  }

  args <- names(formals(server))

  if (!identical(args[1L], "id")) {
    blockr_abort(
      "A block server function is expected to have an argument `id` in ",
      "first postion.",
      class = "block_server_arg_id_invalid"
    )
  }

  if ("..." %in% args) {
    blockr_abort(
      "Variadic blocks are supported not by passing arguments as `...` ",
      "but using a special argument `...args`.",
      class = "block_server_dots_invalid"
    )
  }

  invisible(server)
}

validate_block_ui <- function(ui) {

  if (!is.function(ui)) {
    blockr_abort(
      "A block UI component is expected to be a function.",
      class = "block_ui_function_invalid"
    )
  }

  if (!identical(names(formals(ui)), "id")) {
    blockr_abort(
      "A block UI function is expected to have a single argument `id`.",
      class = "block_ui_arg_id_invalid"
    )
  }

  invisible(ui)
}

validate_data_validator <- function(validator, server) {

  server_args <- setdiff(names(formals(server)), "id")

  if (!length(server_args) && not_null(validator)) {
    blockr_abort(
      "A nullary server function cannot accompany a data input validator.",
      class = "block_validator_nullary_invalid"
    )
  }

  if (not_null(validator)) {

    if (!is.function(validator)) {
      blockr_abort(
        "Data input validator is expected to be a function.",
        class = "block_validator_function_invalid"
      )
    }

    val_args <- names(formals(validator))

    if (!identical(server_args, val_args)) {
      blockr_abort(
        "Server args {server_args} do not match validator args {val_args}.",
        class = "block_validator_args_invalid"
      )
    }
  }

  invisible(validator)
}

validate_allow_empty_state <- function(x) {

  spec <- attr(x, "allow_empty_state")

  if (!is.list(spec)) {
    return(invisible(x))
  }

  nms <- names(spec)

  if (length(spec) &&
        (is.null(nms) || any(!nzchar(nms)) ||
           length(setdiff(nms, c("input", "data"))))) {
    blockr_abort(
      "A list-valued `allow_empty_state` may only contain `input` and ",
      "`data` entries.",
      class = "allow_empty_state_invalid"
    )
  }

  data <- spec[["data"]]

  if (is.null(data)) {
    return(invisible(x))
  }

  dnms <- coal(names(data), character(length(data)))
  is_args <- dnms == "...args"

  if (sum(is_args) > 1L) {
    blockr_abort(
      "`allow_empty_state$data` accepts at most one `...args` entry.",
      class = "allow_empty_state_invalid"
    )
  }

  if (any(is_args)) {

    if (!is.na(block_arity(x))) {
      blockr_abort(
        "A `...args` entry in `allow_empty_state$data` is only valid for a ",
        "variadic block (one with a `...args` server argument).",
        class = "allow_empty_state_invalid"
      )
    }

    if (!is_count(data[[which(is_args)]])) {
      blockr_abort(
        "The `...args` entry in `allow_empty_state$data` is expected to be a ",
        "single non-negative number.",
        class = "allow_empty_state_invalid"
      )
    }
  }

  unknown <- setdiff(unlst(data[!is_args]), legacy_block_inputs(x))

  if (length(unknown)) {
    blockr_abort(
      "`allow_empty_state$data` names unknown block inputs {unknown}.",
      class = "allow_empty_state_invalid"
    )
  }

  invisible(x)
}

validate_legacy_block <- function(x, ui_eval = FALSE) {

  ctor <- block_ctor(x)

  if (!is_blockr_ctor(ctor)) {
    blockr_abort(
      "Expecting a block constructor to inherit from `blockr_ctor`.",
      class = "block_ctor_invalid"
    )
  }

  if (!is_string(block_name(x))) {
    blockr_abort(
      "Expecting a string-valued block name.",
      class = "block_name_invalid"
    )
  }

  srv <- block_expr_server(x)

  validate_block_server(srv)
  validate_block_ui(block_expr_ui(x))
  validate_data_validator(block_data_validator(x), srv)
  validate_allow_empty_state(x)

  if (isTRUE(ui_eval)) {

    ui <- block_expr_ui(x)(NS("block", "expr"))

    if (!inherits(ui, "shiny.tag", "shiny.tag.list", agg = any)) {
      blockr_abort(
        "A block UI function is expected to return a shiny UI object, i.e. ",
        "either a `shiny.tag` or a `shiny.tag.list`.",
        class = "block_ui_eval_invalid"
      )
    }
  }

  x
}

is_legacy_block <- function(x) {
  is.null(block_class(x))
}

block_ctor <- function(x) {
  stopifnot(is_block(x))
  attr(x, "ctor")
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

#' @param data Data input values
#' @rdname block_name
#' @export
validate_data_inputs <- function(x, data) {

  if (block_has_data_validator(x)) {
    return(do.call(block_data_validator(x), data))
  }

  NULL
}

legacy_block_inputs <- function(x) {
  setdiff(block_expr_inputs(x), "...args")
}

legacy_block_arity <- function(x) {

  args <- block_expr_inputs(x)

  if ("...args" %in% args) {
    return(NA_integer_)
  }

  length(args)
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

format_legacy_block <- function(x, ..., state = NULL) {

  out <- format_block_head(x)

  inputs <- block_ctor_inputs(x)

  if (length(inputs)) {

    if (is.null(state)) {
      args <- initial_block_state(x)
      lbl <- "Initial block state:"
    } else {
      stopifnot(is.list(state), all(inputs %in% names(state)))
      args <- state[inputs]
      lbl <- "Block state:"
    }

    out <- c(out, lbl, format_block_state(args))

  } else {

    out <- c(out, "Stateless block")
  }

  ctor <- block_ctor(x)

  if (is.null(ctor_name(ctor))) {
    ctor <- "<local function>"
  } else {
    ctor <- paste0(ctor_pkg(ctor), "::", ctor_name(ctor), "()")
  }

  c(out, paste("Constructor:", ctor))
}
