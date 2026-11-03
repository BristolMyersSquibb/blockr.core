#' Block class fixtures
#'
#' A block class's two halves, its R side and its UI module, meet only in
#' JSON, so its R tests and its module tests share one fixture file, which
#' lists states and records what the class's requests answer. Called in a
#' test, `expect_block_fixture()` checks that the block expression built for
#' each listed state runs on the sample inputs and, where every one does,
#' records the answer of each listed request call and keeps both as a testthat
#' file snapshot (see
#' [testthat::expect_snapshot_file()]), `_snaps/<test file>/<name>.json` in
#' the package's `tests/testthat/`. The module tests read it through the stub
#' of the block's host that core ships, `stubBlockHost()` of
#' `@blockr/core/stubs`, which answers the module's requests from it and
#' refuses a submit of a state it does not list. A change to what a request
#' answers then fails the test until the snapshot is accepted.
#'
#' The fixture is a JSON object holding the `states` and the `requests`. Each
#' state is written as a block given those args holds it, as its constructor
#' takes it from the frame `expr` ran in (see [block_constructor()]), so that
#' an arg left out takes its default, and by the JSON Schema that the class's
#' UI module ships, where it ships one. A module test then compares the state
#' a module submits with it as it is. Without a schema, a value of length one
#' is written bare, but where [I()] keeps it an array. Each request is written
#' with its `name`, the `args` and `options` it was called with, and its
#' `answer`, with every vector as an array, as R writes a request's answer.
#'
#' In a test file, such as `tests/testthat/test-head-block.R`:
#'
#' ```r
#' test_that("the head block's fixture", {
#'   expect_block_fixture(
#'     head_block,
#'     states = list(list(n = 6L), list(n = 10L)),
#'     inputs = list(data = datasets::iris),
#'     requests = list(list(name = "n_max"))
#'   )
#' })
#' ```
#'
#' @param x A block class or a registry id
#' @param states A list of states, each a named list of the state args' values
#' @param inputs Sample upstream results by data input, with a variadic block's
#'   under `...`
#' @param requests A list of request calls, each a list of the request's `name`
#'   and, optionally, its `args` and `options` (see [run_block_request()])
#' @param name The fixture's file name, without extension, which defaults to
#'   the class's name
#'
#' @return Called for its expectations, returning invisibly.
#'
#' @export
expect_block_fixture <- function(x, states, inputs = list(), requests = list(),
                                 name = NULL) {

  rlang::check_installed("testthat")

  cls <- as_block_class(x)
  ids <- sample_input_ids(inputs)

  runs <- lgl_ply(seq_along(states), state_runs, states, cls, ids)

  if (!all(runs)) {
    return(invisible())
  }

  file <- tempfile(fileext = ".json")
  on.exit(unlink(file))

  writeLines(
    json_str(
      list(
        states = lapply(states, fixture_state, cls),
        requests = lapply(requests, record_request, cls, inputs)
      ),
      auto_unbox = TRUE,
      pretty = TRUE
    ),
    file
  )

  testthat::expect_snapshot_file(
    file,
    paste0(coal(name, cls[["class"]][1L], fail_all = FALSE), ".json")
  )

  invisible()
}

state_runs <- function(i, states, cls, ids) {

  res <- evaluate_block(cls, states[[i]], ids[["inputs"]], ids[["results"]])
  ok <- identical(res[["status"]], "ok")

  testthat::expect(
    ok,
    paste0(
      "State ", i, " of block class ", cls[["class"]][1L], " does not run ",
      "(", res[["status"]], "): ",
      paste(chr_xtr(res[["conditions"]], "message"), collapse = "; ")
    )
  )

  ok
}

fixture_state <- function(state, cls) {
  as_json_object(shape_by_schema(expr_state(cls, state), class_schema(cls)))
}

sample_input_ids <- function(inputs) {

  named <- inputs[setdiff(names(inputs), "...")]

  ids <- as.list(set_names(nm = names(named)))
  results <- named

  if ("..." %in% names(inputs)) {

    dots <- inputs[["..."]]
    dot_ids <- paste0("input_", seq_along(dots))

    ids <- c(ids, list(... = set_names(as.list(dot_ids), names(dots))))
    results <- c(results, set_names(dots, dot_ids))
  }

  list(inputs = ids, results = results)
}

record_request <- function(call, cls, inputs) {

  args <- coal(call[["args"]], list(), fail_all = FALSE)
  options <- coal(call[["options"]], list(), fail_all = FALSE)

  list(
    name = call[["name"]],
    args = as_json_object(args),
    options = as_json_object(options),
    answer = encode_answer(
      run_block_request(cls, call[["name"]], inputs, args, options)
    )
  )
}

as_json_object <- function(x) {

  if (!length(x)) {
    return(set_names(list(), character()))
  }

  x
}
