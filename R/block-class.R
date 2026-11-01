#' Block classes
#'
#' A block class is a value made by `new_block_class()`, holding everything
#' that is the same for every block of the class, and the constructor that app
#' scripts call is a function that `block_constructor()` makes from it. A
#' package exports both: the class, from whose fields another class can be
#' built, and the constructor.
#'
#' Every class has an `expr`, a function whose formals are the state args and
#' whose body checks them, as a constructor checks its args, and returns what
#' interpolation makes the block expression from (see [interpolate()]): a call,
#' a string, a function of the data inputs or an object of a class that
#' inherits from `blockr_expr`, such as what [bbquote()] returns. An arg
#' without a default starts unset and stays out of the block's state until
#' something sets it. A body that uses an unset arg, or signals `invalid()`
#' with a message, makes R run nothing and report the block as `invalid`, and
#' a body that can do without an unset arg tests it with [missing()]. Called
#' without args, `expr` gives the state a new block starts with.
#'
#' The data inputs, apart from the state args, are given as `inputs`: a
#' character vector names them, such as `"data"`, `c("x", "y")` or `"..."` for
#' a variadic block, a list from [alist()] gives them as formals, and a
#' function gives them as its formals and checks them in its body, as
#' `function(data) stopifnot(is.data.frame(data))` does. No input has a
#' default. A class names its UI module with `js_module()`, which keeps the
#' module's path in the package's `inst/` and the package whose code calls it,
#' for R to resolve when it serves the module, and which resolves the path
#' against the working directory outside a package. A class further takes a
#' named list of `requests`, functions for what its UI asks of R, which name
#' the board options they read with [reads_options()] (see [block_request()]),
#' and the coercion that makes an output from a result, as `output`. A field a
#' class leaves out is `NULL`.
#'
#' A class is checked once, when it is defined: no state arg is `...`,
#' `block_name` or `visible`, no input or state arg is `options` or shares the
#' name of another, the arguments of each request name inputs or state args,
#' but for parameters, which have a default, for the `file` of a request marked
#' by [action()] and for the `options` of one marked by [reads_options()], and
#' `expr`, called without args, cannot run yet or returns a call, a string, a
#' function whose formals are the inputs or an object of a class that inherits
#' from `blockr_expr`, holding no function.
#'
#' The constructor has the formals of `expr`, a `block_name` and a `visible`,
#' and no `...`, so a misspelled state arg fails as an unused argument. It
#' calls `expr` with its args and returns a block, a list of the state that
#' the checks in `expr` left in the frame it ran in, which leaves out an arg
#' that is still unset. A block holds its name, which defaults to one made
#' from its class's name by `default_block_name()`, and core's attribute
#' `visible`, which says which of the block's sections show, and refers to its
#' class, which every block of the class shares.
#'
#' @section Block vectors:
#' Multiple blocks can be combined into a `blocks` object, a container for
#' an (ordered) set of blocks. Block IDs are handled at the `blocks` level,
#' which ensures that those given are unique and, as every ID, strings that
#' are neither empty nor made of digits alone. A block passed without a name
#' has no ID, which the client mints when it opens the board.
#'
#' @param name Class name, which leads the class vector of every block of the
#'   class
#' @param inputs Data inputs, as a character vector of names, a list of formals
#'   or a function of them
#' @param expr Function of the state args that returns what interpolation
#'   makes the block expression from
#' @param ui The class's UI module, made by `js_module()`
#' @param requests Named list of request functions
#' @param output Coercion that makes an output from a result
#'
#' @examples
#' head_block <- new_block_class(
#'   "head_block",
#'   inputs = function(data) stopifnot(is.data.frame(data)),
#'   expr = function(n = 6L, direction = c("head", "tail")) {
#'     direction <- match.arg(direction)
#'     fn <- if (direction == "tail") quote(utils::tail) else quote(utils::head)
#'     bbquote(.(fn)(.(data), n = .(n)))
#'   },
#'   ui = js_module("head-block.js"),
#'   requests = list(n_max = function(data) nrow(data))
#' )
#'
#' new_head_block <- block_constructor(head_block)
#'
#' blk <- new_head_block(n = 10L)
#' blk
#'
#' is_block_class(head_block)
#' is_block(blk)
#'
#' blks <- c(a = new_head_block(), b = new_head_block(direction = "tail"))
#'
#' is_blocks(blks)
#' names(blks)
#'
#' @return The class constructor `new_block_class()` returns an object of class
#' `block_class` and `block_constructor()` a function of class
#' `block_constructor`, which returns a block, an object inheriting from
#' `block`, as does `as_block()`. A UI module made by `js_module()` is an
#' object of class `js_module`, while `invalid()` signals a condition of class
#' `invalid` and returns nothing. The predicates `is_block_class()` and
#' `is_block()` return a scalar logical. Block vectors, created using
#' `blocks()`, `as_blocks()`, or by combining multiple blocks using [base::c()]
#' all inherit from `blocks` and `is_blocks()` returns a scalar logical.
#'
#' @export
new_block_class <- function(name, inputs = NULL, expr, ui, requests = NULL,
                            output = NULL) {

  if (!is_string(name)) {
    blockr_abort(
      "A block class name is expected to be a string.",
      class = "block_class_name_invalid"
    )
  }

  validate_block_class(
    structure(
      list(
        class = c(name, "block"),
        inputs = inputs,
        expr = expr,
        ui = ui,
        requests = requests,
        output = output
      ),
      class = "block_class"
    )
  )
}

#' @param x Object
#' @rdname new_block_class
#' @export
is_block_class <- function(x) {
  inherits(x, "block_class")
}

#' @param cls Block class
#' @rdname new_block_class
#' @export
block_constructor <- function(cls) {

  if (!is_block_class(cls)) {
    blockr_abort(
      "A block constructor is made from a block class.",
      class = "block_class_invalid"
    )
  }

  ctor <- function() {
    new_class_block(cls, environment())
  }

  formals(ctor) <- c(
    formals(cls[["expr"]]),
    alist(block_name = NULL, visible = NULL)
  )

  structure(
    ctor,
    block_class = cls,
    class = c("block_constructor", "function")
  )
}

#' @param path Path of the module, relative to the package's `inst/`
#' @param package The package whose code names the module (`NULL` outside a
#'   package)
#' @rdname new_block_class
#' @export
js_module <- function(path, package = pkg_name(parent.frame())) {

  if (!is_string(path)) {
    blockr_abort(
      "A module path is expected to be a string.",
      class = "js_module_path_invalid"
    )
  }

  if (is.null(package)) {
    path <- normalizePath(path, mustWork = FALSE)
  } else if (!is_string(package)) {
    blockr_abort(
      "A module's package is expected to be a string or `NULL`.",
      class = "js_module_package_invalid"
    )
  }

  structure(list(path = path, package = package), class = "js_module")
}

#' @param message Why the state cannot run
#' @rdname new_block_class
#' @export
invalid <- function(message) {

  if (!is_string(message)) {
    blockr_abort(
      "The message of `invalid()` is expected to be a string.",
      class = "invalid_message_invalid"
    )
  }

  stop(
    structure(
      class = c("invalid", "error", "condition"),
      list(message = message, call = NULL)
    )
  )
}

is_js_module <- function(x) {
  inherits(x, "js_module")
}

validate_block_class <- function(x) {

  validate_class_inputs(x[["inputs"]])

  if (!is.function(x[["expr"]])) {
    blockr_abort(
      "A block class's `expr` is expected to be a function.",
      class = "block_class_expr_invalid"
    )
  }

  if (!is_js_module(x[["ui"]])) {
    blockr_abort(
      "A block class's `ui` is expected to be made by `js_module()`.",
      class = "block_class_ui_invalid"
    )
  }

  if (!is.null(x[["output"]]) && !is.function(x[["output"]])) {
    blockr_abort(
      "A block class's `output` is expected to be a function.",
      class = "block_class_output_invalid"
    )
  }

  inputs <- class_inputs(x)
  args <- class_state_args(x)

  reserved <- c(
    intersect(args, c("...", "block_name", "visible")),
    intersect(c(inputs, args), "options")
  )

  if (length(reserved)) {
    blockr_abort(
      "A block class cannot have {qty(reserved)}input or state arg{?s} ",
      "{reserved}.",
      class = "block_class_args_invalid"
    )
  }

  known <- c(inputs, args)
  clash <- unique(known[duplicated(known)])

  if (length(clash)) {
    blockr_abort(
      "A block class needs distinct names for its inputs and state args, but ",
      "repeats {clash}.",
      class = "block_class_args_invalid"
    )
  }

  validate_class_requests(x[["requests"]], known)
  validate_initial_expr(x[["expr"]], inputs)

  x
}

validate_class_inputs <- function(inputs) {

  if (is.null(inputs)) {
    return(invisible())
  }

  if (!is.character(inputs) && !is.list(inputs) && !is.function(inputs)) {
    blockr_abort(
      "Block inputs are expected to be names, formals or a function of them.",
      class = "block_class_inputs_invalid"
    )
  }

  nms <- if (is.character(inputs)) inputs else names(input_formals(inputs))

  if (length(inputs) && (is.null(nms) || anyNA(nms) || any(!nzchar(nms)) ||
                           anyDuplicated(nms))) {
    blockr_abort(
      "Block inputs are expected to have unique, non-empty names.",
      class = "block_class_inputs_invalid"
    )
  }

  if (!all(lgl_ply(input_formals(inputs), is_empty_arg))) {
    blockr_abort(
      "Block inputs cannot have defaults.",
      class = "block_class_inputs_invalid"
    )
  }

  invisible()
}

validate_class_requests <- function(requests, known) {

  if (is.null(requests)) {
    return(invisible())
  }

  nms <- names(requests)

  if (!is.list(requests) || !all(lgl_ply(requests, is.function)) ||
        (length(requests) && (is.null(nms) || any(!nzchar(nms)) ||
                                anyDuplicated(nms)))) {
    blockr_abort(
      "The `requests` of a block class are expected to be a list of ",
      "functions with unique names.",
      class = "block_class_requests_invalid"
    )
  }

  for (nm in nms) {

    fun <- requests[[nm]]
    fmls <- formals(fun)

    if ("options" %in% names(fmls) != not_null(request_option_ctors(fun))) {
      blockr_abort(
        "Request {nm} is expected to take `options` exactly where it is ",
        "marked by `reads_options()`.",
        class = "block_class_requests_invalid"
      )
    }

    params <- setdiff(names(fmls), c(known, request_file_arg(fun), "options"))
    bare <- params[lgl_ply(fmls[params], is_empty_arg)]

    if (length(bare)) {
      blockr_abort(
        "Request {nm} takes {bare} without a default, which {qty(bare)}",
        "{?is/are} neither an input nor a state arg of its class.",
        class = "block_class_requests_invalid"
      )
    }
  }

  invisible()
}

validate_initial_expr <- function(expr, inputs) {

  res <- tryCatch(
    expr(),
    error = function(e) if (is_invalid_cnd(e)) NULL else stop(e)
  )

  if (!is.null(res) && !is.call(res) && !is.name(res) &&
        !is.character(res) && !is.function(res) &&
        !inherits(res, "blockr_expr")) {
    blockr_abort(
      "A block class's `expr()` is expected to return a call, a string, a ",
      "function or a `blockr_expr`, not an object of class {class(res)}.",
      class = "block_class_expr_invalid"
    )
  }

  if (is.function(res) && !identical(formal_names(res), inputs)) {
    blockr_abort(
      "A function returned by `expr()` is expected to have the block's ",
      "inputs {inputs} as its formals.",
      class = "block_class_expr_invalid"
    )
  }

  if (!is.function(res) && holds_function(res)) {
    blockr_abort(
      "What `expr()` returns may not hold a function, as base `bquote()` ",
      "puts in for an input's `.(data)`, which `bbquote()` keeps instead.",
      class = "block_class_expr_invalid"
    )
  }

  invisible()
}

holds_function <- function(x) {

  if (is.function(x)) {
    return(TRUE)
  }

  if (!is.call(x)) {
    return(FALSE)
  }

  for (i in seq_along(x)) {
    if (!is_empty_arg(x[[i]]) && holds_function(x[[i]])) {
      return(TRUE)
    }
  }

  FALSE
}

is_empty_arg <- function(x) {
  identical(x, quote(expr = ))
}

formal_names <- function(fun) {
  coal(names(formals(fun)), character(), fail_all = FALSE)
}

input_formals <- function(inputs) {

  if (is.function(inputs)) {
    return(formals(inputs))
  }

  if (is.list(inputs)) inputs
}

class_inputs <- function(cls) {

  inputs <- cls[["inputs"]]

  if (is.character(inputs)) {
    return(inputs)
  }

  coal(names(input_formals(inputs)), character(), fail_all = FALSE)
}

class_state_args <- function(cls) {
  formal_names(cls[["expr"]])
}

class_board_options <- function(cls) {
  combine_board_options(
    call_option_ctors(
      unlst(lapply(cls[["requests"]], request_option_ctors))
    )
  )
}

call_option_ctors <- function(ctors) {
  # Called by name, not via do.call(), as new_board_option() deparses the call
  lapply(ctors, function(new) new())
}

is_invalid_cnd <- function(cnd) {
  inherits(cnd, "invalid") || is_missing_arg_error(cnd)
}

is_missing_arg_error <- function(cnd) {
  inherits(cnd, "missingArgError") ||
    (inherits(cnd, "error") &&
       grepl(missing_arg_pattern(), conditionMessage(cnd)))
}

missing_arg_pattern <- function() {

  msg <- gettext("argument \"%s\" is missing, with no default", domain = "R")
  parts <- strsplit(msg, "%s", fixed = TRUE)[[1L]]

  paste0(
    "^",
    paste(gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", parts), collapse = ".+"),
    "$"
  )
}

new_class_block <- function(cls, env) {

  args <- class_state_args(cls)
  given <- args[!lgl_ply(args, arg_is_missing, env)]

  block_name <- coal(
    env[["block_name"]],
    default_block_name(cls[["class"]]),
    fail_all = FALSE
  )

  if (!is_string(block_name)) {
    blockr_abort(
      "Expecting a string-valued block name.",
      class = "block_name_invalid"
    )
  }

  visible <- env[["visible"]]

  if (!is.null(visible) && !is.character(visible)) {
    blockr_abort(
      "Expecting `visible` to be a character vector.",
      class = "block_visible_invalid"
    )
  }

  structure(
    expr_state(cls, mget(given, envir = env)),
    block_name = block_name,
    visible = visible,
    block_class = cls,
    class = cls[["class"]]
  )
}

arg_is_missing <- function(arg, env) {
  eval(call("missing", as.name(arg)), env)
}

expr_state <- function(cls, args) {

  frame <- NULL
  expr <- cls[["expr"]]

  body(expr) <- bquote(
    {
      .(function(env) frame <<- env)(environment())
      .(body(expr))
    }
  )

  tryCatch(
    do.call(expr, args, quote = TRUE),
    error = function(e) if (!is_invalid_cnd(e)) stop(e)
  )

  fmls <- formals(expr)
  nms <- formal_names(expr)

  keep <- !lgl_ply(fmls, is_empty_arg) |
    !lgl_ply(nms, arg_is_missing, frame)

  mget(nms[keep], envir = frame)
}

block_class <- function(x) {
  attr(x, "block_class", exact = TRUE)
}

constructor_class <- function(ctor) {

  if (!inherits(ctor, "block_constructor")) {
    blockr_abort(
      "Expecting a block constructor made by `block_constructor()`.",
      class = "block_constructor_invalid"
    )
  }

  attr(ctor, "block_class", exact = TRUE)
}

block_state <- function(x) {

  res <- unclass(x)
  attributes(res) <- NULL
  names(res) <- names(x)

  res
}

#' @param class A block class's class vector
#' @rdname new_block_class
#' @export
default_block_name <- function(class) {
  res <- gsub("_", " ", sub("_block$", "", class[1L]))
  paste0(toupper(substr(res, 1L, 1L)), substring(res, 2L))
}

validate_block <- function(x, ...) {

  if (!is_block(x)) {
    blockr_abort(
      "Expecting blocks to inherit from \"block\".",
      class = "block_class_invalid"
    )
  }

  if (class(x)[1L] == "block") {
    blockr_abort(
      "Expecting blocks to have at least one sub-class.",
      class = "block_class_invalid"
    )
  }

  if (!is.list(x)) {
    blockr_abort(
      "Expecting blocks to behave list-like.",
      class = "block_list_like_invalid"
    )
  }

  if (is_legacy_block(x)) {
    return(validate_legacy_block(x, ...))
  }

  if (!is_block_class(block_class(x))) {
    blockr_abort(
      "Expecting a block to refer to its block class.",
      class = "block_class_invalid"
    )
  }

  if (!is_string(block_name(x))) {
    blockr_abort(
      "Expecting a string-valued block name.",
      class = "block_name_invalid"
    )
  }

  x
}

#' @rdname new_block_class
#' @export
is_block <- function(x) {
  inherits(x, "block")
}

#' @param ... For `blocks()`, the blocks to combine, and otherwise generic
#'   consistency
#' @rdname new_block_class
#' @export
as_block <- function(x, ...) {
  UseMethod("as_block")
}

#' @export
as_block.block <- function(x, ...) {
  validate_block(x)
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
#' intended for users to easily identify different blocks. No uniqueness
#' restrictions are in place and the block name can be retrieved with
#' `block_name()`.
#'
#' @section Input validation:
#' Data input validation is available via `validate_data_inputs()` which uses
#' the (optional) validator function passed to [new_block()] at construction
#' time. A block class checks its data inputs in the body of a function given
#' as its `inputs` instead (see [new_block_class()]).
#'
#' @section Block arity:
#' The number of data inputs of a block is available as `block_arity()`, which
#' reads the inputs of the block's class. In case of variadic blocks (i.e.
#' blocks that take a variable number of inputs like for example a block
#' providing [base::rbind()]-like functionality), `block_arity()` returns `NA`.
#'
#' @param x An object inheriting from `"block"`
#'
#' @examples
#' blk <- new_dataset_block()
#' block_name(blk)
#'
#' block_arity(new_dataset_block())
#' block_arity(new_merge_block())
#' block_arity(new_rbind_block())
#'
#' @return Return types vary among the set of exported utilities:
#' * `block_name()`: string valued block name,
#' * `validate_data_inputs()`: `NULL` if no validator is set and the result of
#'   the validator function otherwise,
#' * `block_arity()`: a scalar integer with `NA` in case of variadic behavior.
#'
#' @export
block_name <- function(x) {
  stopifnot(is_block(x))
  attr(x, "block_name")
}

#' @rdname block_name
#' @export
block_arity <- function(x) {

  if (is_board(x)) {
    return(
      set_names(int_ply(board_blocks(x), block_arity), board_block_ids(x))
    )
  }

  if (is_legacy_block(x)) {
    return(legacy_block_arity(x))
  }

  inputs <- block_data_inputs(x)

  if ("..." %in% inputs) {
    return(NA_integer_)
  }

  length(inputs)
}

block_data_inputs <- function(x) {

  if (is_legacy_block(x)) {
    return(legacy_block_inputs(x))
  }

  class_inputs(block_class(x))
}

block_state_args <- function(x) {

  if (is_legacy_block(x)) {
    return(block_ctor_inputs(x))
  }

  class_state_args(block_class(x))
}

format_block_head <- function(x) {

  out <- ""

  for (cl in rev(setdiff(class(x), c("list", "vctrs_vctr")))) {
    out <- paste0("<", cl, out, ">")
  }

  arity <- block_arity(x)

  if (is.na(arity)) {
    dat <- "Indefinite arity"
  } else if (arity > 0L) {
    dat <- paste0(
      "Data inputs: ",
      paste_enum(block_data_inputs(x), quotes = "\"")
    )
  } else {
    dat <- "No data inputs"
  }

  c(out, paste0("Name: \"", block_name(x), "\""), dat)
}

format_block_state <- function(state) {
  trimws(utils::capture.output(utils::str(state))[-1L], "right")
}

#' @export
format.block <- function(x, ...) {

  if (is_legacy_block(x)) {
    return(format_legacy_block(x, ...))
  }

  state <- block_state(x)

  if (length(state)) {
    lines <- c("Block state:", format_block_state(state))
  } else {
    lines <- "Stateless block"
  }

  c(format_block_head(x), lines)
}

#' @export
print.block <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
format.block_class <- function(x, ...) {

  inputs <- class_inputs(x)
  args <- class_state_args(x)
  requests <- names(x[["requests"]])
  ui <- x[["ui"]]

  c(
    paste0("<block_class> ", x[["class"]][1L]),
    if (length(inputs)) {
      paste0("Data inputs: ", paste_enum(inputs, quotes = "\""))
    },
    if (length(args)) {
      paste0("State args: ", paste_enum(args, quotes = "\""))
    },
    if (length(requests)) {
      paste0("Requests: ", paste_enum(requests, quotes = "\""))
    },
    paste0(
      "UI: ",
      ui[["path"]],
      if (not_null(ui[["package"]])) paste0(" (", ui[["package"]], ")")
    )
  )
}

#' @export
print.block_class <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
format.block_constructor <- function(x, ...) {

  usage <- deparse(args(x))

  c(
    paste0("<block_constructor> ", constructor_class(x)[["class"]][1L]),
    usage[-length(usage)]
  )
}

#' @export
print.block_constructor <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
str_value.block <- function(x, ...) {

  out <- paste0("<", class(x)[1L], ">")

  args <- block_state_args(x)

  if (length(args)) {
    out <- paste0(out, " ", paste0(args, collapse = ", "))
  }

  out
}

#' @importFrom utils str
#' @export
str.block <- function(object, ...) {
  cat_str_value(object, ...)
}

#' @export
board_options.block <- function(x, ...) {

  if (is_legacy_block(x)) {
    return(new_board_options())
  }

  class_board_options(block_class(x))
}
