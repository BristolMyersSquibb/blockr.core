#' Block evaluation
#'
#' A block is evaluated from its class, its state and the results of the
#' blocks upstream of it, building no block. Given a registry id or a block
#' class, `evaluate_block()` runs the checks of a function given as the class's
#' `inputs` with the upstream results, calls the class's `expr` with the
#' state, interpolates what it returns into the block expression (see
#' [interpolate()]) and runs that with the generic `block_eval()`, before
#' describing the result with the generic `block_summary()`.
#'
#' An `invalid()` that `expr` or a `block_eval()` method signals, or an unset
#' arg that the body of `expr` uses or interpolation fills in, makes R run
#' nothing and report the status `invalid`, while a failed check in `inputs`,
#' or any other error, reports `error`, and a block that runs reports `ok`.
#' Every condition raised on the way is recorded with its severity, its class,
#' its message and the phase that raised it: `inputs`, `expr`, which covers
#' interpolation, `block_eval` or `block_summary`. Warnings and messages are
#' recorded rather than signaled.
#'
#' The `block_eval()` generic runs the block expression, `expr`, in `env`,
#' where each upstream block's result is bound to its id, dispatching on the
#' block's class, as `x` is the block's state with its class's class vector,
#' and gets in `...` the parameters of the one-time evaluation that asked for
#' the block, if any. Core's method keeps whatever the block expression draws
#' as the result, a `recordedplot` holding in its attribute `rerun` a function
#' that runs the block expression again, and its value otherwise. A package
#' adds a method where its class runs otherwise, deciding whether and how the
#' block expression runs, never what it computes. Each evaluation environment
#' is created by `eval_env()`.
#'
#' The `block_summary()` generic describes a result in a few words, such as
#' `<data.frame> [150 x 5]`, dispatching on the result's class, so that a
#' package describes its results wherever they appear. The `block_migrate()`
#' generic maps a state that an older version of the class's package saved to
#' the current state, given that version, where `x` is the saved state with the
#' class's class vector. Core's method leaves a state as it is.
#'
#' @param x For `evaluate_block()`, a registry id or a block class, and for
#'   `block_eval()` and `block_migrate()`, a block's state with its class's
#'   class vector
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
#' res <- evaluate_block(head_block, list(n = 3L), list(data = "a"),
#'                       list(a = datasets::iris))
#'
#' res$status
#' res$expr
#' res$summary
#'
#' evaluate_block(head_block, list(n = 3L, m = 2L), list(data = "a"),
#'                list(a = datasets::iris))$status
#'
#' block_summary(datasets::mtcars)
#'
#' @return The evaluation `evaluate_block()` returns a list holding its
#' `status`, `"ok"`, `"invalid"` or `"error"`, the block expression as `expr`,
#' the `result`, its `summary` and the `conditions` raised, where what was not
#' reached is `NULL`. The result is what `block_eval()` returns, while
#' `block_summary()` returns a string, `block_migrate()` a state and
#' `eval_env()` an environment.
#'
#' @export
evaluate_block <- function(x, state = list(), inputs = list(),
                           results = list(), ...) {

  cls <- as_block_class(x)
  run <- new_evaluation()

  data <- input_results(class_inputs(cls), inputs, results)

  if (is.function(cls[["inputs"]])) {

    res <- run_phase(run, "inputs", do.call(cls[["inputs"]], data))

    if (inherits(res, "phase_failure")) {
      return(evaluation(run, "error"))
    }
  }

  res <- run_phase(
    run,
    "expr",
    interpolate(do.call(cls[["expr"]], state, quote = TRUE), inputs)
  )

  if (inherits(res, "phase_failure")) {
    return(evaluation(run, failure_status(res)))
  }

  run[["expr"]] <- res

  res <- run_phase(
    run,
    "block_eval",
    block_eval(
      structure(state, class = cls[["class"]]),
      run[["expr"]],
      eval_env(results[unique(unlst(inputs, recursive = TRUE))]),
      ...
    )
  )

  if (inherits(res, "phase_failure")) {
    return(evaluation(run, failure_status(res)))
  }

  run[["result"]] <- res

  res <- run_phase(run, "block_summary", block_summary(run[["result"]]))

  if (!inherits(res, "phase_failure")) {
    run[["summary"]] <- res
  }

  evaluation(run, "ok")
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

new_evaluation <- function() {

  res <- new.env(parent = emptyenv())
  res[["conditions"]] <- list()

  res
}

run_phase <- function(run, phase, expr) {

  record <- function(cnd) {
    run[["conditions"]] <- c(
      run[["conditions"]],
      list(condition_record(cnd, phase))
    )
  }

  try_catch_continue(
    expr,
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
      structure(list(phase = phase, condition = e), class = "phase_failure")
    }
  )
}

condition_record <- function(cnd, phase) {

  severity <- if (inherits(cnd, "error")) {
    "error"
  } else if (inherits(cnd, "warning")) {
    "warning"
  } else {
    "message"
  }

  list(
    severity = severity,
    class = class(cnd),
    phase = phase,
    message = sub("\n$", "", conditionMessage(cnd))
  )
}

failure_status <- function(failure) {

  cnd <- failure[["condition"]]

  if (inherits(cnd, "invalid") ||
        (identical(failure[["phase"]], "expr") && is_missing_arg_error(cnd))) {
    return("invalid")
  }

  "error"
}

evaluation <- function(run, status) {
  list(
    status = status,
    expr = run[["expr"]],
    result = run[["result"]],
    summary = run[["summary"]],
    conditions = run[["conditions"]]
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

  dev <- grDevices::dev.cur()

  grDevices::pdf(NULL)
  rec <- grDevices::dev.cur()

  on.exit(restore_device(rec, dev))

  grDevices::dev.control(displaylist = "enable")

  res <- eval(expr, env)
  plot <- grDevices::recordPlot()

  if (!length(plot[[1L]])) {
    return(res)
  }

  structure(plot, rerun = function() eval(expr, env))
}

restore_device <- function(rec, dev) {

  grDevices::dev.off(rec)

  if (dev > 1L) {
    grDevices::dev.set(dev)
  }

  invisible()
}

#' @param data Input data (a named list)
#' @rdname evaluate_block
#' @export
eval_env <- function(data) {
  if (isTRUE(blockr_option("attach_default_packages", FALSE))) {
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
block_summary <- function(result) {
  UseMethod("block_summary")
}

#' @rdname evaluate_block
#' @export
block_summary.default <- function(result) {

  res <- paste0("<", type_desc(result), ">")
  dims <- dim_desc(result)

  if (length(dims)) {
    res <- paste0(res, " [", dims, "]")
  }

  res
}

type_desc <- function(x) {
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

#' @param version The version of the class's package that saved the state
#' @rdname evaluate_block
#' @export
block_migrate <- function(x, version) {
  UseMethod("block_migrate")
}

#' @rdname evaluate_block
#' @export
block_migrate.block <- function(x, version) {
  unclass(x)
}
