# R's spans of the messages that the evaluator handles, as otelsdk records
# them in memory.

client_span <- "b7ad6b7169203331"
client_trace <- "0af7651916cd43dd8448eb211c80319c"
client_parent <- paste0("00-", client_trace, "-", client_span, "-01")

# The spans that R records as `expr` runs, by their names.
recorded_spans <- function(expr) {

  testthat::skip_if_not_installed("otelsdk")

  spans <- otelsdk::with_otel_record(expr, what = "traces")[["traces"]]
  set_names(spans, chr_xtr(spans, "name"))
}

span_attributes <- function(span) {
  unclass(span[["attributes"]])
}

traced <- function(req, parent = client_parent) {
  c(req, list(traceparent = parent))
}

test_that("An evaluation's span is the client's child and holds its phases", {

  spans <- recorded_spans({
    session <- local_evaluator(session_id = "token-1")
    session$receive(
      list(traced(evaluate_request("17", "a", 2L, sample_recipe("a"))))
    )
    session$run()
  })

  expect_setequal(
    names(spans),
    c("evaluate", "inputs", "expr", "block_eval", "result_summary")
  )

  evaluation <- spans[["evaluate"]]

  # A child of the client's span, in the client's trace.
  expect_identical(evaluation[["trace_id"]], client_trace)
  expect_identical(evaluation[["parent"]], client_span)
  expect_identical(evaluation[["kind"]], "server")
  expect_identical(evaluation[["status"]], "unset")

  expect_mapequal(
    span_attributes(evaluation),
    list(
      session.id = "token-1",
      blockr.block.id = "a",
      blockr.block.revision = 2,
      blockr.block.registry_id = "blockr.core-dataset_block",
      blockr.reply.status = "ok"
    )
  )

  for (phase in c("inputs", "expr", "block_eval", "result_summary")) {
    expect_identical(spans[[phase]][["trace_id"]], client_trace)
    expect_identical(spans[[phase]][["parent"]], evaluation[["span_id"]])
  }
})

test_that("R evaluates a traced block as the fixture evaluate-traced has it", {

  spans <- recorded_spans({
    session <- local_evaluator()
    hold_result(session, "a", 2L)
    hold_result(session, "b", 3L, list(read_of("a", 2L, "data")))
    expect_fixture_replies(session, "evaluate-traced")
  })

  traced_evaluation <- Filter(
    function(span) identical(span[["parent"]], client_span),
    spans
  )

  expect_length(traced_evaluation, 1L)
  expect_identical(
    span_attributes(traced_evaluation[[1L]])[["blockr.block.id"]],
    "c"
  )

  # R replies the same where it records no spans.
  session <- local_evaluator()
  hold_result(session, "a", 2L)
  hold_result(session, "b", 3L, list(read_of("a", 2L, "data")))
  expect_fixture_replies(session, "evaluate-traced")
})

test_that("R's span of a message without a valid traceparent is a root span", {

  spans <- recorded_spans({
    session <- local_evaluator()
    session$receive(
      list(
        evaluate_request("17", "a", 2L, sample_recipe("a")),
        traced(evaluate_request("18", "a", 3L, sample_recipe("a")), "zzz"),
        traced(evaluate_request("19", "a", 4L, sample_recipe("a")), 42L)
      )
    )
    session$run()
  })

  evaluations <- spans[names(spans) == "evaluate"]

  expect_length(evaluations, 3L)

  for (evaluation in evaluations) {
    expect_identical(evaluation[["parent"]], "0000000000000000")
    expect_false(identical(evaluation[["trace_id"]], client_trace))
    # Without a session, the span names none.
    expect_null(span_attributes(evaluation)[["session.id"]])
  }
})

test_that("R marks the spans of a failed evaluation and its phase as failed", {

  local_block_class(static_block)

  failing <- list(
    registry_id = "blockr.core-static_block",
    state = list(expression = "stop('No data.')"),
    inputs = list()
  )

  spans <- recorded_spans({
    session <- local_evaluator()
    session$receive(list(traced(evaluate_request("2", "s", 1L, failing))))
    session$run()
  })

  expect_identical(spans[["evaluate"]][["status"]], "error")
  expect_identical(spans[["evaluate"]][["description"]], "No data.")
  expect_identical(
    span_attributes(spans[["evaluate"]])[["blockr.reply.status"]],
    "error"
  )

  expect_identical(spans[["block_eval"]][["status"]], "error")
  expect_identical(spans[["block_eval"]][["description"]], "No data.")
  expect_identical(spans[["expr"]][["status"]], "unset")
})

test_that("a phase that stops the block without failing it is not marked", {

  # The merge block's expr signals blockr_invalid() for an unset `by`.
  unset <- sample_recipe(
    "c",
    list(read_of("a", 2L, "x"), read_of("b", 3L, "y"))
  )
  unset$state$by <- NULL

  spans <- recorded_spans({
    session <- local_evaluator()
    hold_result(session, "a", 2L)
    hold_result(session, "b", 3L, list(read_of("a", 2L, "data")))
    session$receive(list(traced(evaluate_request("21", "c", 9L, unset))))
    session$run()
  })

  evaluation <- Filter(
    function(span) identical(span[["parent"]], client_span),
    spans
  )[[1L]]

  expect_identical(
    span_attributes(evaluation)[["blockr.reply.status"]],
    "invalid"
  )
  expect_identical(evaluation[["status"]], "unset")

  inside <- Filter(
    function(span) identical(span[["parent"]], evaluation[["span_id"]]),
    spans
  )

  expect_true("expr" %in% names(inside))
  expect_true(all(chr_xtr(inside, "status") == "unset"))
})

test_that("R's span of an output names the registry id of its block's class", {

  spans <- recorded_spans({
    session <- local_evaluator(session_id = "token-1")
    hold_result(session, "a", 2L)
    session$receive(
      list(traced(output_request("18", "a", 2L, obj(n_rows = 5L))))
    )
    session$run()
  })

  output <- spans[["output"]]

  expect_identical(output[["parent"]], client_span)
  expect_mapequal(
    span_attributes(output),
    list(
      session.id = "token-1",
      blockr.block.id = "a",
      blockr.block.revision = 2,
      blockr.block.registry_id = "blockr.core-dataset_block",
      blockr.reply.status = "ok"
    )
  )

  # R records no span of an output's phase, which the output's span covers.
  expect_false("output" %in% names(spans)[names(spans) != "output"])
  expect_length(spans[names(spans) == "output"], 1L)
})

test_that("R's span of a request names the request and what it belongs to", {

  local_request_types()

  spans <- recorded_spans({
    session <- local_evaluator()
    session$receive(
      list(
        traced(
          list(
            id = "4",
            kind = "request",
            type = "plugin",
            plugin = "blockr.notes-notes_plugin",
            name = "x",
            args = obj(),
            reads = list()
          )
        )
      )
    )
    session$run()
  })

  expect_named(spans, "request x")

  request <- spans[["request x"]]

  expect_identical(request[["parent"]], client_span)
  expect_identical(
    span_attributes(request)[c(
      "blockr.request.type", "blockr.request.name", "blockr.plugin.class_id"
    )],
    list(
      blockr.request.type = "plugin",
      blockr.request.name = "x",
      blockr.plugin.class_id = "blockr.notes-notes_plugin"
    )
  )
})

test_that("a request that R drops ends its span as cancelled", {

  spans <- recorded_spans({
    session <- local_evaluator()
    session$receive(
      list(
        traced(evaluate_request("12", "a", 2L, sample_recipe("a"))),
        list(kind = "cancel", request = "12")
      )
    )
    session$run()
  })

  expect_named(spans, "evaluate")
  expect_identical(
    span_attributes(spans[["evaluate"]])[["blockr.reply.status"]],
    "cancelled"
  )
  expect_identical(spans[["evaluate"]][["status"]], "unset")
})

test_that("R records no span and reads no traceparent while it traces none", {

  session <- local_evaluator()

  session$receive(
    list(traced(evaluate_request("17", "a", 2L, sample_recipe("a")), "zzz"))
  )

  expect_null(session$evaluator$queue[[1L]][["span"]])

  session$run()

  expect_identical(last_reply(session)[["status"]], "ok")
  expect_length(session$logged, 0L)
})

test_that("R records no span of a hello or of reading blockr options", {

  spans <- recorded_spans({
    session <- local_evaluator(hello = FALSE)
    session$receive(hello_message())
    session$receive(
      list(traced(list(id = "2", kind = "blockr_options", names = list())))
    )
    session$run()
  })

  expect_length(spans, 0L)
})

test_that("the lines R logs as it handles a message name the session", {

  local_block_class(static_block)

  failing <- list(
    registry_id = "blockr.core-static_block",
    state = list(expression = "stop('No data.')"),
    inputs = list()
  )

  session <- local_evaluator(session_id = "token-1")

  session$receive(
    list(
      list(id = "3", kind = "nonsense"),
      evaluate_request("2", "s", 1L, failing)
    )
  )
  session$run()

  # As R reads the message, and as it sends the reply.
  expect_identical(
    chr_xtr(session$logged, "message"),
    c(
      "Request 3: R reads no request of kind \"nonsense\".",
      "Block s at revision 1: No data."
    )
  )
  expect_identical(chr_xtr(session$logged, "session"), c("token-1", "token-1"))
  expect_null(logged_session$id)
})
