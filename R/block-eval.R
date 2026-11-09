#' Block evaluation
#'
#' A block is evaluated from its class, its state and the results of the
#' blocks upstream of it, building no block. Given a registry id or a block
#' class, `evaluate_block()` runs the checks of a function given as the class's
#' `inputs` with the upstream results, calls the class's `expr` with the
#' state, interpolates what it returns into the block expression (see
#' [interpolate()]) and runs that with the generic `block_eval()`, before
#' describing the result with the generic `result_summary()`.
#'
#' A [blockr_invalid()] that `expr` or a `block_eval()` method signals, or an
#' unset arg that the body of `expr` uses or interpolation fills in, makes R
#' run nothing and report the status `invalid`, while a failed check in
#' `inputs`, or any other error, reports `error`, and a block that runs reports
#' `ok`. A check in `inputs` returns `NULL`, and any other value fails it too.
#' Every condition raised on the way is recorded with its severity, its class,
#' its message and the phase that raised it: `inputs`, `expr`, which covers
#' interpolation, `block_eval` or `result_summary`. Warnings and messages are
#' recorded rather than signaled.
#'
#' The `block_eval()` generic runs the block expression, `expr`, in `env`,
#' where each upstream block's result is bound to its id, dispatching on the
#' block's class, as `x` is the block's state with its class's class vector,
#' and gets in `...` the parameters of the one-time evaluation that asked for
#' the block, if any. Core's method evaluates the block expression, and a
#' package adds a method where its class runs otherwise, deciding whether and
#' how the block expression runs, never what it computes. Each evaluation
#' environment is created by `eval_env()`.
#'
#' The `result_summary()` generic describes a result in a few words, such as
#' `<data.frame> [150 x 5]`, dispatching on the result's class, so that a
#' package describes its results wherever they appear, such as in a block's
#' header and in the cells of a table. Core's method names the result's class
#' in full, or its type for an object without a class, with `<function>` for
#' any function and `<tibble>` for a tibble. It adds the result's dimensions,
#' or the length of a vector.
#' A method returns a string, and any other value fails the summary,
#' as an error does.
#' The block's status then stays `ok`, since the block ran, but its evaluation
#' holds no summary.
#'
#' @param x For `evaluate_block()`, a registry id or a block class, and for
#'   `block_eval()`, a block's state with its class's class vector
#' @param state The block's state, a named list of its state args' values
#' @param inputs Upstream block ids by data input, as [interpolate()] takes
#'   them
#' @param results Upstream results by block id
#' @param expr Block expression to evaluate in `env`
#' @param env Environment in which to evaluate `expr`
#' @param ... For `evaluate_block()` and `block_eval()`, the parameters of a
#'   one-time evaluation
#'
#' @examples
#' head_block <- new_block_class(
#'   "head_block",
#'   inputs = "data",
#'   expr = function(n = 6L) bbquote(utils::head(.(data), n = .(n))),
#'   ui = js_module("head-block.js")
#' )
#'
#' evaluate_block(head_block, list(n = 3L), list(data = "a"),
#'                list(a = datasets::iris))
#'
#' evaluate_block(head_block, list(n = 3L, m = 2L), list(data = "a"),
#'                list(a = datasets::iris))
#'
#' result_summary(datasets::mtcars)
#'
#' @return The evaluation `evaluate_block()` returns an object of class
#' `block_evaluation`, a list holding its `status`, `"ok"`, `"invalid"` or
#' `"error"`, the block expression as `expr`, the `result`, its `summary` and
#' the `conditions` raised, where what was not reached is `NULL`. The result
#' is what `block_eval()` returns, while `result_summary()` returns a string
#' and `eval_env()` an environment.
#'
#' @export
evaluate_block <- function(x, state = list(), inputs = list(),
                           results = list(), ...) {
  eval_block_class(as_block_class(x), state, inputs, results, list(...))
}

# Evaluates a block of the class `cls`, as `evaluate_block()` does,
# with the parameters of a one-time evaluation given as a list,
# so that a parameter reaches `block_eval()` whatever its name.
# Where `built` gives the block expression that `build_block_expr()` built
# before, as for a locked block where both locks are on, R runs that,
# and calls no `expr`.
eval_block_class <- function(cls, state, inputs, results, params,
                             built = NULL) {

  data <- input_results(class_inputs(cls), inputs, results)

  checks <- run_phase(
    "inputs",
    if (is.function(cls[["inputs"]])) check_inputs(cls, data)
  )

  if (phase_failed(checks)) {
    return(new_block_evaluation("error", conditions = checks[["conditions"]]))
  }

  if (is.null(built)) {
    built <- build_block_expr(cls, state, inputs)
  }

  conditions <- c(checks[["conditions"]], built[["conditions"]])

  if (phase_failed(built)) {
    return(new_block_evaluation(phase_status(built), conditions = conditions))
  }

  ran <- run_phase(
    "block_eval",
    do.call(
      block_eval,
      c(
        list(
          structure(state, class = cls[["class"]]),
          built[["value"]],
          eval_env(results[unique(unlst(inputs, recursive = TRUE))])
        ),
        params
      ),
      quote = TRUE
    )
  )

  conditions <- c(conditions, ran[["conditions"]])

  if (phase_failed(ran)) {
    return(
      new_block_evaluation(
        phase_status(ran),
        expr = built[["value"]],
        conditions = conditions
      )
    )
  }

  summary <- run_phase("result_summary", check_summary(ran[["value"]]))

  new_block_evaluation(
    "ok",
    expr = built[["value"]],
    result = ran[["value"]],
    summary = summary[["value"]],
    conditions = c(conditions, summary[["conditions"]])
  )
}

# Builds the block expression of a block of the class `cls` from its state and
# the upstream ids of its inputs, as the phase `expr` that `run_phase()`
# records: the expression as its value, with the conditions raised, and the
# error where building it failed, which `phase_status()` reads.
build_block_expr <- function(cls, state, inputs) {
  run_phase(
    "expr",
    interpolate(do.call(cls[["expr"]], state, quote = TRUE), inputs)
  )
}

# The checks of a function given as a class's inputs, which signal an error
# where the inputs do not fit and otherwise return NULL, so that a predicate
# that returns FALSE fails rather than letting every input through
check_inputs <- function(cls, data) {

  res <- do.call(cls[["inputs"]], data)

  if (!is.null(res)) {
    blockr_abort(
      "The inputs check of block class {cls[['class']][1L]} is expected to ",
      "return `NULL` and to signal an error where the inputs do not fit, but ",
      "it returned an object of class {class(res)[1L]}.",
      class = "block_inputs_check_invalid"
    )
  }

  invisible()
}

# The summary of a result from `result_summary()`.
# Core calls `result_summary()` only through this check.
# A method is expected to return a string.
# Where it returns anything else, the check signals an error,
# just as a method that fails does.
check_summary <- function(result) {

  res <- result_summary(result)

  if (!is_string(res)) {
    blockr_abort(
      "The `result_summary()` method for a result of class ",
      "{class(result)[1L]} is expected to return a string, but it returned an ",
      "object of class {class(res)[1L]} and length {length(res)}.",
      class = "result_summary_invalid"
    )
  }

  res
}

as_block_class <- function(x) {

  if (is_block_class(x)) {
    return(x)
  }

  if (!is_string(x)) {
    blockr_abort(
      "Expecting a block class or a registry id.",
      class = "block_class_invalid"
    )
  }

  get_registry_entry(x)[["block_class"]]
}

input_results <- function(names, inputs, results) {

  missing <- setdiff(names, names(inputs))

  if (length(missing)) {
    blockr_abort(
      "Data {qty(missing)}input{?s} {missing} {?is/are} not linked.",
      class = "block_inputs_unlinked"
    )
  }

  ids <- unlst(inputs[names], recursive = TRUE)
  unknown <- setdiff(ids, names(results))

  if (length(unknown)) {
    blockr_abort(
      "No result for upstream {qty(unknown)}block{?s} {unknown}.",
      class = "block_results_missing"
    )
  }

  named <- setdiff(names, "...")
  res <- set_names(results[unlst(inputs[named])], named)

  if ("..." %in% names) {
    dots <- inputs[["..."]]
    res <- c(res, set_names(results[unlst(dots)], names(dots)))
  }

  res
}

new_block_evaluation <- function(status, expr = NULL, result = NULL,
                                 summary = NULL, conditions = list()) {

  if (!is_string(status) || !status %in% c("ok", "invalid", "error")) {
    blockr_abort(
      "Expecting the status of a block evaluation to be \"ok\", \"invalid\" ",
      "or \"error\".",
      class = "block_evaluation_status_invalid"
    )
  }

  if (!(is.null(expr) || is.language(expr))) {
    blockr_abort(
      "Expecting the expression of a block evaluation to be `NULL` or a ",
      "language object.",
      class = "block_evaluation_expr_invalid"
    )
  }

  if (!(is.null(summary) || is_string(summary))) {
    blockr_abort(
      "Expecting the summary of a block evaluation to be `NULL` or a string.",
      class = "block_evaluation_summary_invalid"
    )
  }

  if (!is.list(conditions)) {
    blockr_abort(
      "Expecting the conditions of a block evaluation to be a list.",
      class = "block_evaluation_conditions_invalid"
    )
  }

  structure(
    list(
      status = status,
      expr = expr,
      result = result,
      summary = summary,
      conditions = conditions
    ),
    class = "block_evaluation"
  )
}

#' @export
format.block_evaluation <- function(x, ...) {

  cnds <- x[["conditions"]]

  c(
    paste0("<block_evaluation> ", x[["status"]]),
    if (not_null(x[["expr"]])) {
      paste0("Block expression: ", deparse1(x[["expr"]]))
    },
    if (not_null(x[["summary"]])) {
      paste0("Summary: ", x[["summary"]])
    },
    if (length(cnds)) {
      c(
        "Conditions:",
        paste0(
          "  ", chr_xtr(cnds, "severity"), " in ", chr_xtr(cnds, "phase"),
          ": ", chr_xtr(cnds, "message")
        )
      )
    }
  )
}

#' @export
print.block_evaluation <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

# Runs a phase of an evaluation, keeping a record of each message, warning and
# error that it raises, in order, in a queue, which takes each in constant time,
# while growing a list with `c()` copies it for every one.
run_phase <- function(phase, expr) {

  conditions <- fastmap::fastqueue()

  record <- function(cnd) {
    conditions$add(condition_record(cnd, phase))
  }

  res <- try_catch_continue(
    list(value = expr),
    message = function(m) {
      record(m)
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      record(w)
      invokeRestart("muffleWarning")
    },
    error = function(e) {
      record(e)
      list(error = e)
    }
  )

  list(
    phase = phase,
    value = res[["value"]],
    conditions = conditions$as_list(),
    error = res[["error"]]
  )
}

phase_failed <- function(x) {
  not_null(x[["error"]])
}

phase_status <- function(x) {

  cnd <- x[["error"]]

  if (inherits(cnd, "blockr_invalid") ||
        (identical(x[["phase"]], "expr") && is_missing_arg_error(cnd))) {
    return("invalid")
  }

  "error"
}

condition_record <- function(cnd, phase) {

  severity <- if (inherits(cnd, "error")) {
    "error"
  } else if (inherits(cnd, "warning")) {
    "warning"
  } else {
    "message"
  }

  message <- conditionMessage(cnd)

  # A message that is not valid UTF-8 is matched byte by byte,
  # as `sub()` refuses it otherwise, and its reply makes it valid.
  list(
    severity = severity,
    class = class(cnd),
    phase = phase,
    message = sub("\n$", "", message, useBytes = !all(validUTF8(message)))
  )
}

#' @rdname evaluate_block
#' @export
block_eval <- function(x, expr, env, ...) {
  UseMethod("block_eval")
}

#' @rdname evaluate_block
#' @export
block_eval.block <- function(x, expr, env, ...) {
  eval(expr, env)
}

#' @param data Input data (a named list)
#' @rdname evaluate_block
#' @export
eval_env <- function(data) {

  defaults <- as.logical(blockr_option("attach_default_packages", FALSE))

  if (!is_bool(defaults)) {
    blockr_abort(
      "The blockr option `attach_default_packages` is TRUE or FALSE.",
      class = "attach_default_packages_invalid"
    )
  }

  if (defaults) {
    parent <- default_eval_parent()
  } else {
    parent <- baseenv()
  }

  list2env(data, parent = parent)
}

eval_env_default_pkgs <- c(
  "stats", "graphics", "grDevices", "utils", "datasets", "methods"
)

default_eval_parent <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    parent <- baseenv()
    for (pkg in rev(eval_env_default_pkgs)) {
      parent <- pkg_export_env(pkg, parent)
    }
    cached <<- parent
    cached
  }
})

pkg_export_env <- function(pkg, parent) {
  ns <- loadNamespace(pkg)
  e <- new.env(parent = parent)
  exports <- getNamespaceExports(ns)
  importIntoEnv(e, exports, ns, exports)
  lazydata <- getNamespaceInfo(ns, "lazydata")
  for (nm in ls(lazydata, all.names = TRUE)) {
    do.call(delayedAssign,
            list(nm, as.symbol(nm), eval.env = lazydata, assign.env = e))
  }
  lockEnvironment(e, bindings = TRUE)
  e
}

#' @param result A block's result
#' @rdname evaluate_block
#' @export
result_summary <- function(result) {
  UseMethod("result_summary")
}

#' @rdname evaluate_block
#' @export
result_summary.default <- function(result) {

  res <- paste0("<", type_desc(result), ">")
  dims <- dim_desc(result)

  if (length(dims)) {
    res <- paste0(res, " [", dims, "]")
  }

  res
}

type_desc <- function(x) {

  if (inherits(x, "tbl_df")) {
    return("tibble")
  }

  if (is.function(x)) {
    return("function")
  }

  if (is.object(x)) class(x)[1L] else typeof(x)
}

dim_desc <- function(x) {

  dims <- dim(x)

  if (is.null(dims) && obj_is_vector(x)) {
    dims <- vec_size(x)
  }

  if (is.null(dims)) {
    return(character())
  }

  paste0(big_mark(dims), collapse = " \u00d7 ")
}

big_mark <- function(x) {

  mark <- if (identical(getOption("OutDec"), ",")) "." else ","

  res <- formatC(x, big.mark = mark, format = "d")
  res[is.na(x)] <- "??"

  res
}
